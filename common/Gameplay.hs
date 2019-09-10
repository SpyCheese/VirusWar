{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Gameplay
 ( playerScore
 , TurnMonad
 , performTurn
 , getAvailableMoves
 ) where

import Control.Monad (forM_, when, unless)
import Control.Monad.ST
import Control.Monad.State (replicateM)
import Control.Monad.Trans.State
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Lens.Micro.Platform

import Game
import Utils

playerScore :: Player -> Field -> Int
playerScore p f = sum $ fmap (sum . fmap cellToScore) f
  where
    cellToScore :: Cell -> Int
    cellToScore (Alive p2) =
      if p2 == p
        then 1
        else 0
    cellToScore _ = 0

type TurnMonad a = StateT Game (Either String) a

pass :: Monad m => m ()
pass = return ()

performTurn :: Turn -> TurnMonad ()
performTurn t =
  (view gameCurrentTurn <$> get) >>= \case
    GameOver -> fail "Game is over"
    CurrentTurn p turnsRemaining -> case t of
      TurnFinish -> do
        modify $ set (gamePlayers . singular (ix (unPlayer p)) . isPlayerActive) False
        findNextActivePlayer p
      Turn c@(CellIndex x y) -> do
        field <- view gameField <$> get
        unless (inField field c) $ fail "Cell is outside the field"
        case cellAt c field of
          Blocked -> fail "Cell is blocked"
          Dead _ _ -> fail "Cell is already killed"
          Alive p1 -> when (p == p1) $ fail "You have already occupied this cell"
          _ -> pass
        available <- getAvailableMoves <$> get
        unless (cellAt c available) $ fail "Cell is not reachable"
        let newCell =
              case cellAt c field of
                Empty -> Alive p
                Alive p1 -> Dead p1 p
                z -> z
        modify $ set (gameField . singular (ix x) . singular (ix y)) newCell
        if turnsRemaining == 1
          then findNextActivePlayer p
          else modify $ set gameCurrentTurn $ CurrentTurn p (turnsRemaining - 1)

findNextActivePlayer :: Player -> TurnMonad ()
findNextActivePlayer p0 = do
  subTurns <- view gameSubTurnCount <$> get
  nextPlayer p0 >>= findNextActivePlayerImpl >>= \case
    Nothing -> modify $ set gameCurrentTurn GameOver
    Just p -> modify $ set gameCurrentTurn (CurrentTurn p subTurns)
  where
    nextPlayer :: Player -> TurnMonad Player
    nextPlayer (Player p) = do
      n <- length . view gamePlayers <$> get
      if p == n - 1
        then return $ Player 0
        else return $ Player (p + 1)
    findNextActivePlayerImpl :: Player -> TurnMonad (Maybe Player)
    findNextActivePlayerImpl p = do
      players <- view gamePlayers <$> get
      if (players ! unPlayer p) ^. isPlayerActive
        then return $ Just p
        else if p == p0
               then return Nothing
               else nextPlayer p >>= findNextActivePlayerImpl

getAvailableMoves :: Game -> Grid Bool
getAvailableMoves game = case game ^. gameCurrentTurn of
  GameOver -> V.replicate sx (V.replicate sy False)
  CurrentTurn p _ -> do
    let field = game ^. gameField
    let reachable = getReachableCells p field
    V.generate sx \x -> V.generate sy \y -> case field ! x ! y of
      Alive p1 -> p /= p1 && reachable ! x ! y
      Empty -> reachable ! x ! y
      _ -> False
  where
    sx, sy :: Int
    sx = V.length (game ^. gameField)
    sy = V.length (V.head $ game ^. gameField)

    getReachableCells :: Player -> Field -> Grid Bool
    getReachableCells p field =
      runST $ do
        reachable <- MV.replicateM sx $ MV.replicate sy False
        forM_ [0 .. sx - 1] $ \x ->
          forM_ [0 .. sy - 1] $ \y -> do
            let cell = CellIndex x y
            if cellAt cell field == Alive p
              then dfs cell reachable
              else pass
        V.forM (V.generate sx id) $ \x ->
          V.forM (V.generate sy id) $ \y -> do
              row <- MV.read reachable x
              MV.read row y
      where
        dfs :: CellIndex -> MV.MVector s (MV.MVector s Bool) -> ST s ()
        dfs c@(CellIndex x y) reachable =
          if not $ inField field c
            then pass
            else do
              row <- MV.read reachable x
              val <- MV.read row y
              if val
                then pass
                else do
                  MV.write row y True
                  let cell = cellAt c field
                  let continue =
                        case cell of
                          Alive p1 -> p == p1
                          Dead _ p1 -> p == p1
                          _ -> False
                  if continue
                    then do
                      dfs (CellIndex (x + 1) y) reachable
                      dfs (CellIndex (x - 1) y) reachable
                      dfs (CellIndex x (y + 1)) reachable
                      dfs (CellIndex x (y - 1)) reachable
                    else pass

