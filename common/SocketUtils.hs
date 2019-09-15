{-# LANGUAGE LambdaCase #-}
module SocketUtils
 ( MySocket
 , mySocket
 , sendJSON
 , recvJSON
 , myClose
 ) where

import Control.Exception (catch)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import GHC.IO.Exception
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as NBS

data MySocket = MySocket Socket (IORef [BS.ByteString])

mySocket :: Socket -> IO MySocket
mySocket sock = do
  contents <- NBS.getContents sock
  ref <- newIORef (BS.split 0 contents)
  return $ MySocket sock ref

sendJSON :: ToJSON a => a -> MySocket -> IO ()
sendJSON x (MySocket sock _) =
  catch (NBS.sendAll sock (encode x `BS.snoc` 0)) (\IOError {} -> return ()) 

recvJSON :: FromJSON a => MySocket -> IO (Maybe a)
recvJSON (MySocket _ ref) =
  readIORef ref >>= \case
    [] -> return Nothing
    x : xs -> do
      writeIORef ref xs
      return $ decode x

myClose :: MySocket -> IO ()
myClose (MySocket sock _) = close sock
