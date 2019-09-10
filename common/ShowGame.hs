{-# LANGUAGE InstanceSigs #-}
module ShowGame where

import Control.Monad.State
import Data.Char
import Data.Vector (Vector, imap)
import qualified Data.Vector as V
import Lens.Micro.Platform

import Game
import Gameplay (playerScore)


instance Show Player where
  show :: Player -> String
  show (Player p) = show p

instance Show Cell where
  show :: Cell -> String
  show Empty = "___"
  show Blocked = "###"
  show (Alive p) = " " ++ show p ++ " "
  show (Dead _ p) = "<" ++ show p ++ ">"


rowToString :: Int -> String
rowToString = doRowToString 1
  where
    doRowToString :: Int -> Int -> String
    doRowToString len x =
      if x >= 26 ^ len
        then doRowToString (len + 1) (x - 26 ^ len)
        else reverse $ evalState (replicateM len step) x

    step :: State Int Char
    step = get >>= (\x -> put (x `div` 26) >> return (chr (ord 'A' + x `mod` 26)))

instance Show CellIndex where
  show :: CellIndex -> String
  show (CellIndex x y) = rowToString x ++ show (y + 1)

showField :: Field -> String
showField f = firstRow (length $ V.head f) ++ concat (imap showRow f)
  where
    rowNameLen :: Int
    rowNameLen = length $ rowToString (length f - 1)

    fixedLength :: Int -> String -> String
    fixedLength len s = let slen = length s
                        in if slen >= len
                          then drop (slen - len) s
                          else replicate (len - length s) ' ' ++ s

    firstRow :: Int -> String
    firstRow len = replicate (rowNameLen + 3) ' ' ++
                   unwords (fmap (fixedLength 3 . show) [1 .. len]) ++ "\n"

    showRow :: Int -> Vector Cell -> String
    showRow i xs = let rowName = rowToString i
                   in fixedLength rowNameLen rowName ++ " [ " ++
                      unwords (V.toList $ fmap show xs) ++ " ]\n"


instance Show Turn where
  show :: Turn -> String
  show (Turn t) = show t
  show TurnFinish = "finish"

instance Show CurrentTurnInfo where
  show :: CurrentTurnInfo -> String
  show (CurrentTurn p n) = "Turn of player " ++ show p ++
                           ", remaining turns: " ++ show n
  show GameOver = "Game is over"

instance Show PlayerInfo where
  show :: PlayerInfo -> String
  show p = if p ^. isPlayerActive then "ACTIVE" else "inactive"


instance Show Game where
  show :: Game -> String
  show g = "================ Game ================\n" ++
           "Players:\n" ++
           concat (imap playerLine (g ^. gamePlayers)) ++
           showField (g ^. gameField) ++
           show (g ^. gameCurrentTurn)
    where
      playerLine :: Int -> PlayerInfo -> String
      playerLine i p = "  #" ++ show i ++ ": " ++
                       show (playerScore (Player i) (g ^. gameField)) ++
                       " cells, " ++ show p ++ "\n"