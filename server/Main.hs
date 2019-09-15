{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (intercalate)
import Data.Text (pack)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import Database
import qualified Database.SQLite.Simple as SQL
import GHC.IO
import Lens.Micro.Platform ((^.))
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as NBS
import System.Environment
import System.Exit (die)

import Game
import GameJsonImpl
import Gameplay
import ServerConfig
import ShowGame
import SocketUtils
import Utils

main :: IO ()
main = do
  argv <- getArgs
  if length argv /= 2
    then die "Usage: ./VirusVar-server-exe config_file.json database.sqlite"
    else withDatabase (argv !! 1) (mainWithArgs $ head argv)

eitherParse :: FromJSON a => Value -> Either String a
eitherParse = resultToEither . fromJSON
  where
    resultToEither :: Result a -> Either String a
    resultToEither (Error x) = Left x
    resultToEither (Success x) = Right x

mainWithArgs :: String -> SQL.Connection -> IO ()
mainWithArgs configFile conn = withSocketsDo $ do
  configContent <- tryIO (BS.readFile configFile) ("Failed to read " ++ configFile)
  config <- eitherDecode configContent `orDie` "Failed to parse config"
  let gameJson = config ^. initialGameJson
  game <- eitherParse gameJson `orDie` "Failed to parse game"

  let ports = config ^. playerPorts
  when (V.length ports /= V.length (game ^. gamePlayers)) $ die "Number of ports should be equal to number of players"
  putStrLn "Server started"
  print game
  putStrLn $ "Listening on ports " ++ intercalate ", " (V.toList (V.map show ports))
  namesRef <- newIORef []
  mySockets <- V.mapM createSocket ports >>= V.mapM \sock -> do
    names <- readIORef namesRef
    (mySock, name) <- acceptPlayer names sock
    writeIORef namesRef (name : names)
    return mySock
  names <- V.reverse . V.fromList <$> readIORef namesRef
  finally (runServer conn game gameJson mySockets names) (V.forM mySockets myClose)

acceptPlayer :: [String] -> Socket -> IO (MySocket, String)
acceptPlayer prevNames sock = untilJust do
  newSock <- fst <$> accept sock
  mySock <- mySocket newSock
  recvJSON mySock >>= \case
    Nothing -> return Nothing
    Just name -> if isValidPlayerName name && not (elem name prevNames)
      then return $ Just (mySock, name)
      else do
        myClose mySock
        return Nothing

createSocket :: PortNumber -> IO Socket
createSocket port = do
  let hints = defaultHints {
    addrFlags = [AI_PASSIVE],
    addrSocketType = Stream
  }
  addr : _ <- getAddrInfo (Just hints) Nothing (Just $ show port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 1
  return sock

runServer :: SQL.Connection -> Game -> Value -> Vector MySocket -> Vector String -> IO ()
runServer conn initialGame initialGameJson sockets playerNames = do
  putStrLn "Players connected"
  dbPlayersInfo <- V.mapM (dbGetPlayer conn) playerNames
  let ratingInfo = V.map snd dbPlayersInfo
  let dbPlayerIds = V.map fst dbPlayersInfo
  dbGameId <- dbAddGame conn (initialGame ^. gameName) initialGameJson dbPlayerIds
  putStrLn $ "Game id = " ++ show dbGameId
  V.forM ratingInfo print
  V.forM (V.indexed sockets) \(i, sock) -> do
    sendJSON initialGameJson sock
    sendJSON (Player i) sock
    sendJSON ratingInfo sock
  turnIndexRef <- newIORef 0
  endGame <- iterateUntilM (\g -> isGameOver $ g ^. gameCurrentTurn)
    (runStep dbGameId turnIndexRef) initialGame

  let scores = V.generate (length playerNames) \pi -> playerScore (Player pi) (endGame ^. gameField)
  dbFinishGame conn dbGameId dbPlayerIds scores ratingInfo

  where
    runStep :: Int -> IORef Int -> Game -> IO Game
    runStep dbGameId turnIndexRef game = do
      putStrLn ""
      print game
      (p, remaining) <- case game ^. gameCurrentTurn of
        GameOver -> die ""
        CurrentTurn x y -> return (x, y)
      let sock = sockets ! unPlayer p
      turn <- recvJSON sock <&> \case
        Nothing -> TurnFinish
        Just t -> t
      putStrLn $ "Turn: " ++ show turn
      case execStateT (performTurn turn) game of
        Left e -> do
          putStrLn $ "Error: " ++ e
          sendJSON (object [ pack "error" .= toJSON e ]) sock
          return game
        Right newGame -> do
          putStrLn "OK"
          turnIndex <- readIORef turnIndexRef
          writeIORef turnIndexRef (turnIndex + 1)
          dbAddTurn conn turnIndex dbGameId (unPlayer p) turn
          sendJSON emptyObject sock
          V.forM_ (V.ifilter (\i _ -> i /= unPlayer p) sockets) $ sendJSON turn
          return newGame
