{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Database
  ( withDatabase
  , dbGetPlayer
  , dbAddGame
  , dbAddTurn
  , dbFinishGame
  ) where

import Control.Monad
import Data.Aeson
import Data.List (sort)
import Data.Text (pack)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as SQL
import System.Exit (die)

import Game

withDatabase :: String -> (SQL.Connection -> IO a) -> IO a
withDatabase path f = SQL.withConnection path \conn -> do
  putStrLn $ "Connected to database " ++ path
  isValid <- checkDatabase conn
  unless isValid (initDatabase conn)
  f conn

newtype TableName = TableName { unTableName :: String }

getTables :: SQL.Connection -> IO [String]
getTables conn = SQL.queryWith_ SQL.field conn "SELECT name FROM sqlite_master;"

myTables :: [String]
myTables = ["fields", "games", "participations", "players", "turns"]

checkDatabase :: SQL.Connection -> IO Bool
checkDatabase conn = do
  tables <- getTables conn
  return $ all (flip elem tables) myTables

initDatabase :: SQL.Connection -> IO ()
initDatabase conn = do
  putStrLn "Initializing database"
  tables <- getTables conn
  forM_ (filter (flip elem myTables) tables) \t ->
    SQL.execute_ conn $ SQL.Query $ pack $ ("DROP TABLE " ++ t ++ ";")
  SQL.execute_ conn $ SQL.Query $ pack $ unlines
    [ "CREATE TABLE fields ("
    , "  id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
    , "  name TEXT NOT NULL,"
    , "  json BLOB NOT NULL UNIQUE"
    , ");" ]
  SQL.execute_ conn $ SQL.Query $ pack $ unlines
    [ "CREATE TABLE players ("
    , "  id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
    , "  name TEXT NOT NULL UNIQUE,"
    , "  games INTEGER NOT NULL DEFAULT 0,"
    , "  wins INTEGER NOT NULL DEFAULT 0"
    , ");" ]
  SQL.execute_ conn $ SQL.Query $ pack $ unlines
    [ "CREATE TABLE games ("
    , "  id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
    , "  time_start TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,"
    , "  time_end TIMESTAMP,"
    , "  finished INTEGER NOT NULL DEFAULT 0,"
    , "  field_id INTEGER NOT NULL,"
    , "  FOREIGN KEY (field_id) REFERENCES fields(id)"
    , ");" ]
  SQL.execute_ conn $ SQL.Query $ pack $ unlines
    [ "CREATE TABLE participations ("
    , "  game_id INTEGER NOT NULL,"
    , "  player_num INTEGER NOT NULL,"
    , "  player_id INTEGER NOT NULL,"
    , "  score INTEGER NOT NULL DEFAULT 0,"
    , "  FOREIGN KEY (game_id) REFERENCES games(id),"
    , "  FOREIGN KEY (player_id) REFERENCES players(id),"
    , "  PRIMARY KEY (game_id, player_num)"
    , ");" ]
  SQL.execute_ conn $ SQL.Query $ pack $ unlines
    [ "CREATE TABLE turns ("
    , "  game_id INTEGER NOT NULL,"
    , "  player_num INTEGER NOT NULL,"
    , "  turn_index INTEGER NOT NULL,"
    , "  is_finish INTEGER NOT NULL DEFAULT 0,"
    , "  x INTEGER,"
    , "  y INTEGER,"
    , "  FOREIGN KEY (game_id) REFERENCES games(id),"
    , "  PRIMARY KEY (game_id, turn_index)"
    , ");" ]
  return ()

dbGetPlayer :: SQL.Connection -> String -> IO (Int, PlayerRatingInfo)
dbGetPlayer conn name = do
  let query = "SELECT id, games, wins FROM players WHERE name = ?;"
  (SQL.query conn query (SQL.Only name) :: IO [(Int, Int, Int)]) >>= \case
    [(id, games, wins)] -> return (id, PlayerRatingInfo name games wins)
    [] -> do
      SQL.execute conn "INSERT INTO players (name) VALUES (?);" (SQL.Only name)
      rowid <- SQL.lastInsertRowId conn
      id <- head <$> SQL.queryWith SQL.field conn "SELECT id FROM players WHERE rowid = ?;" (SQL.Only rowid)
      return (id, PlayerRatingInfo name 0 0)
    _ -> die "Duplicating player entry in database"

dbGetField :: SQL.Connection -> String -> Value -> IO Int
dbGetField conn name json = do
  let jsonStr = encode json
  let query = "SELECT id FROM fields WHERE json = ?;"
  (SQL.queryWith SQL.field conn query (SQL.Only jsonStr) :: IO [Int]) >>= \case
    [id] -> return id
    [] -> do
      SQL.execute conn "INSERT INTO fields (name, json) VALUES (?, ?);" (name, jsonStr)
      rowid <- SQL.lastInsertRowId conn
      head <$> SQL.queryWith SQL.field conn "SELECT id FROM fields WHERE rowid = ?;" (SQL.Only rowid)
    _ -> die "Duplicating field entry in database"

dbAddGame :: SQL.Connection -> String -> Value -> Vector Int -> IO Int
dbAddGame conn name json players = do
  fieldId <- dbGetField conn name json
  SQL.execute conn "INSERT INTO games (field_id) VALUES (?);" (SQL.Only fieldId)
  rowid <- SQL.lastInsertRowId conn
  id <- head <$> SQL.queryWith SQL.field conn "SELECT id FROM games WHERE rowid = ?;" (SQL.Only rowid)
  V.forM_ (V.indexed players) \(num, pid) -> do
    let query = "INSERT INTO participations (game_id, player_num, player_id) VALUES (?, ?, ?);";
    SQL.execute conn query (id, num + 1, pid)
  return id

dbAddTurn :: SQL.Connection -> Int -> Int -> Int -> Turn -> IO ()
dbAddTurn conn turnIndex gameId playerNum TurnFinish = do
  let query = "INSERT INTO turns (game_id, player_num, turn_index, is_finish) VALUES (?, ?, ?, 1);"
  SQL.execute conn query (gameId, playerNum, turnIndex)
dbAddTurn conn turnIndex gameId playerNum (Turn (CellIndex x y)) = do
  let query = "INSERT INTO turns (game_id, player_num, turn_index, x, y) VALUES (?, ?, ?, ?, ?);"
  SQL.execute conn query (gameId, playerNum, turnIndex, x, y)

dbFinishGame :: SQL.Connection -> Int -> Vector Int -> Vector Int -> Vector PlayerRatingInfo -> IO ()
dbFinishGame conn gameId playerIds scores ratingInfo = do
  let query = "UPDATE games SET finished = 1, time_end = CURRENT_TIMESTAMP WHERE id = ?;"
  SQL.execute conn query (SQL.Only gameId)
  let n = length playerIds
  let maxScore = V.maximum scores
  forM_ [0 .. n - 1] \pi -> do
    let score = scores ! pi
    let query = "UPDATE participations SET score = ? WHERE game_id = ? AND player_num = ?;"
    SQL.execute conn query (score, gameId, pi + 1)
    let playerId = playerIds ! pi
    let PlayerRatingInfo _ games wins = ratingInfo ! pi
    let query = "UPDATE players SET games = ?, wins = ? WHERE id = ?;"
    SQL.execute conn query (games + 1, wins + if score == maxScore then 1 else 0, playerId)
