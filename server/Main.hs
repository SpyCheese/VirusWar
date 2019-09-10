{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Text (pack)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
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
  if length argv /= 1
    then die "Usage: ./VirusVar-server-exe config_file.json"
    else mainWithConfigFile $ head argv

eitherParse :: FromJSON a => Value -> Either String a
eitherParse = resultToEither . fromJSON
  where
    resultToEither :: Result a -> Either String a
    resultToEither (Error x) = Left x
    resultToEither (Success x) = Right x

mainWithConfigFile :: String -> IO ()
mainWithConfigFile configFile = withSocketsDo $ do
  configContent <- tryIO (BS.readFile configFile) ("Failed to read " ++ configFile)
  config <- eitherDecode configContent `orDie` "Failed to parse config"
  let gameJson = config ^. initialGameJson
  game <- eitherParse gameJson `orDie` "Failed to parse game"

  let ports = config ^. playerPorts
  when (V.length ports /= V.length (game ^. gamePlayers)) $ die "Number of ports should be equal to number of players"
  putStrLn "Server started"
  print game
  putStrLn $ "Listening on ports " ++ intercalate ", " (V.toList (V.map show ports))
  sockets <- (V.mapM createSocket ports >>= V.mapM accept) <&> V.map fst
  mySockets <- V.mapM mySocket sockets
  finally (runServer game gameJson mySockets) (V.forM sockets close)

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

runServer :: Game -> Value -> Vector MySocket -> IO ()
runServer initialGame initialGameJson sockets = do
  putStrLn "Players connected"
  V.forM (V.indexed sockets) \(i, sock) -> do
    sendJSON initialGameJson sock
    sendJSON (Player i) sock
  iterateUntilM (\g -> isGameOver $ g ^. gameCurrentTurn) runStep initialGame
  return ()
  where
    runStep :: Game -> IO Game
    runStep game = do
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
          sendJSON emptyObject sock
          V.forM_ (V.ifilter (\i _ -> i /= unPlayer p) sockets) $ sendJSON turn
          return newGame
