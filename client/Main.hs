{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State (execStateT)
import Data.Aeson
import Data.Aeson.Types
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import GHC.IO (finally)
import Lens.Micro.Platform ((^.))
import Network.Socket
import System.Environment
import System.Exit (die)
import Text.Read (readMaybe)

import Game
import GameJsonImpl
import Gameplay
import Graphics
import SocketUtils

printUsage :: IO a
printUsage = die "Usage: ./VirusWar-client-exe host port"

main :: IO ()
main = withSocketsDo $ do
  argv <- getArgs
  when (length argv /= 2) printUsage
  sock <- createSocket (head argv) (argv !! 1)
  mySock <- mySocket sock
  finally (runClient mySock) (close sock)
  return ()

createSocket :: HostName -> ServiceName -> IO Socket
createSocket host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr : _ <- getAddrInfo (Just hints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock

runClient :: MySocket -> IO ()
runClient sock = do
  initialGame <- (recvJSON sock :: IO (Maybe Game)) >>= \case
    Nothing -> die ""
    Just g -> return g
  me <- (recvJSON sock :: IO (Maybe Player)) >>= \case
    Nothing -> die ""
    Just g -> return g

  gameRef <- newIORef initialGame
  turnVar <- newEmptyMVar
  thread <- forkOS $ void $ runThread sock gameRef turnVar me initialGame
  startGraphics initialGame gameRef turnVar me
  return ()

runThread :: MySocket -> IORef Game -> MVar Turn -> Player -> Game -> IO (Game)
runThread sock gameRef turnVar me =
  iterateUntilM (\g -> isGameOver $ g ^. gameCurrentTurn) \g -> do
    let (CurrentTurn p _) = g ^. gameCurrentTurn
    if p == me
      then do
        turn <- if (V.any (V.any id) (getAvailableMoves g))
          then takeMVar turnVar
          else return TurnFinish
        sendJSON turn sock
        result@(Object _) <- fromJust <$> recvJSON sock
        if result == emptyObject
          then do
            let (Right newGame) = execStateT (performTurn turn) g
            atomicWriteIORef gameRef newGame
            return newGame
          else do
            return g
      else do
        (Just turn) <- recvJSON sock
        let (Right newGame) = execStateT (performTurn turn) g
        atomicWriteIORef gameRef newGame
        return newGame

