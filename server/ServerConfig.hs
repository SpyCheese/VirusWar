{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module ServerConfig
 ( ServerConfig
 , initialGameJson, playerPorts
 ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Vector
import Data.Text (pack)
import Network.Socket
import Lens.Micro.Platform

import Utils
import Data.Word (Word16)

data ServerConfig = ServerConfig
  { _initialGameJson :: Value
  , _playerPorts :: Vector PortNumber
  }

makeLenses ''ServerConfig


instance FromJSON PortNumber where
  parseJSON :: Value -> Parser PortNumber
  parseJSON json = fromInteger <$> parseJSON json

instance FromJSON ServerConfig where
  parseJSON :: Value -> Parser ServerConfig
  parseJSON (Object v) = ServerConfig
                         <$> v .: pack "game"
                         <*> v .: pack "ports"

  parseJSON _ = fail "Object expected"
