{-# LANGUAGE TemplateHaskell #-}
module Game
  ( CellIndex (CellIndex)
  , Grid
  , cellAt, updateCell, inField
  , Player (Player)
  , unPlayer
  , Cell (Empty, Blocked, Alive, Dead)
  , Field
  , Turn (Turn, TurnFinish)
  , CurrentTurnInfo (CurrentTurn, GameOver)
  , isGameOver
  , PlayerInfo (PlayerInfo)
  , isPlayerActive
  , Game (Game)
  , gamePlayers, gameField, gameCurrentTurn, gameSubTurnCount
  ) where

import Control.Monad.State
import Control.Monad (replicateM)
import Data.Char (chr, ord)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Lens.Micro.Platform


data CellIndex = CellIndex Int Int

type Grid a = Vector (Vector a)

cellAt :: CellIndex -> Grid a -> a
cellAt (CellIndex x y) g = g ! x ! y

updateCell :: CellIndex -> a -> Grid a -> Grid a
updateCell (CellIndex x y) c g =
  V.update g $ V.singleton (x, V.update (g ! x) $ V.singleton (y, c))

inField :: Field -> CellIndex -> Bool
inField f (CellIndex x y) = x >= 0 && y >= 0 && x < sx && y < sy
  where
    sx, sy :: Int
    sx = length f
    sy = length (V.head f)


newtype Player = Player { unPlayer :: Int }
  deriving (Eq)

data Cell
  = Empty
  | Blocked
  | Alive Player
  | Dead Player Player
  deriving (Eq)

type Field = Grid Cell

data Turn = Turn CellIndex | TurnFinish

data CurrentTurnInfo = CurrentTurn Player Int | GameOver

isGameOver :: CurrentTurnInfo -> Bool
isGameOver GameOver = True
isGameOver _ = False

newtype PlayerInfo = PlayerInfo
    { _isPlayerActive :: Bool
    }

data Game = Game
  { _gamePlayers :: Vector PlayerInfo
  , _gameField :: Field
  , _gameCurrentTurn :: CurrentTurnInfo
  , _gameSubTurnCount :: Int
  }

makeLenses ''Game
makeLenses ''PlayerInfo
