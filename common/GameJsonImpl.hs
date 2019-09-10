{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module GameJsonImpl where

import Control.Monad (forM, when)
import Data.Aeson
import Data.Aeson.Types
import Data.Char (ord)
import Data.Text (pack)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Game
import Utils


instance ToJSON CellIndex where
  toJSON :: CellIndex -> Value
  toJSON (CellIndex x y) = Array $ V.fromList [toJSON x, toJSON y]

instance FromJSON CellIndex where
  parseJSON :: Value -> Parser CellIndex
  parseJSON (Array arr) | V.length arr == 2 =
    CellIndex <$> parseJSON (arr ! 0) <*> parseJSON (arr ! 1)
  parseJSON _ = fail "Array expected"


instance ToJSON Player where
  toJSON :: Player -> Value
  toJSON = toJSON . unPlayer

instance FromJSON Player where
  parseJSON :: Value -> Parser Player
  parseJSON json = Player <$> parseJSON json


instance ToJSON Turn where
  toJSON :: Turn -> Value
  toJSON (Turn c) = toJSON c
  toJSON TurnFinish = toJSON "finish"

instance FromJSON Turn where
  parseJSON :: Value -> Parser Turn
  parseJSON arr@(Array _) = Turn <$> parseJSON arr
  parseJSON s@(String _) =
    parseJSON s >>= \r -> if r == "finish" then return TurnFinish else fail "\"finish\" expected"
  parseJSON _ = fail "Array or \"finish\" expected"


instance FromJSON Game where
  parseJSON :: Value -> Parser Game
  parseJSON (Object v) = do
    nPlayers <- v .: pack "players" >>= takeIf (\x -> 2 <= x && x <= 10) "Number of players should be in 2..10"
    field <- v .: pack "field" >>= parseField nPlayers
    t <- v .: pack "subturns" >>= takeIf (> 0) "Subturns must be > 0"
    return $ Game (V.replicate nPlayers $ PlayerInfo True) field (CurrentTurn (Player 0) t) t
    where
      parseField :: Int -> Vector String -> Parser Field
      parseField n ss = do
        when (V.length ss == 0) $ fail "Field is empty"
        let sy = length $ V.head ss
        when (sy == 0) $ fail "Field is empty"
        when (any ((/= sy) . length) ss) $ fail "Sizes of rows are not equal"
        V.forM ss \row ->
          V.forM (V.fromList row) $ \case
            '.' -> return Empty
            '#' -> return Blocked
            c -> do
              let i = ord c - ord '0'
              if i < 0 || i >= n
                then fail $ "Invalid player " ++ show c
                else return $ Alive (Player i)

  parseJSON _ = fail "Object expected"
