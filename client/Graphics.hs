{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics
  ( startGraphics
  ) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Loops
import Data.FileEmbed (embedFile)
import Data.IORef
import Data.Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.C (CInt)
import Lens.Micro.Platform ((^.))
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Primitive as SDL
import SDL.Vect ((^-^), (^+^))

import Game
import Gameplay
import SDLUtils
import ShowGame

startGraphics :: Game -> IORef Game -> MVar Turn -> Player -> IO ()
startGraphics initialGame gameRef turnVar me =
  withSDL $ withTTF $
  withWindow "Virus war" windowSize \window ->
  withRenderer window \renderer -> do
    font <- TTF.decode $(embedFile "resources/DejaVuSansMono.ttf") infoScoreHeight
    mainLoop gameRef turnVar me renderer font
  where
    mainLoop :: IORef Game -> MVar Turn -> Player -> SDL.Renderer -> TTF.Font -> IO ()
    mainLoop gameRef turnVar me renderer font = untilM_ (return ()) do
      events <- SDL.pollEvents
      game <- readIORef gameRef
      quit <- anyM (processEvent game) events
      if quit
        then return True
        else do
          SDL.rendererDrawColor renderer $= black
          SDL.clear renderer
          drawField renderer game me
          drawPlayers renderer game me font
          SDL.present renderer
          return False

    processEvent :: Game -> SDL.Event -> IO Bool
    processEvent game event = case SDL.eventPayload event of
      SDL.QuitEvent -> return True
      SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ (SDL.P (SDL.V2 x y))) -> do
        case game ^. gameCurrentTurn of
          GameOver -> return False
          CurrentTurn p _ -> if p /= me then return False else do
            let c = CellIndex (fromIntegral y `div` cellSize) (fromIntegral x `div` cellSize)
            let field = game ^. gameField
            when (inField field c && cellAt c (getAvailableMoves game)) $
              void $ (tryPutMVar turnVar $ Turn c)
            return False
      _ -> return False

    drawField :: SDL.Renderer -> Game -> Player -> IO ()
    drawField renderer game me = do
      let moves = getAvailableMoves game
      V.forM_ (V.indexed $ game ^. gameField) \(i, row) ->
        V.forM_ (V.indexed row) \(j, cell) ->
          drawCell i j cell (moves ! i ! j)
      where
        drawCell :: Int -> Int -> Cell -> Bool -> IO ()
        drawCell i j cell available = do
          let p1 = vec2 (j * cellSize) (i * cellSize)
          let p2 = vec2 ((j + 1) * cellSize) ((i + 1) * cellSize)
          let vctHalf = vec2 (cellSize `div` 2) (cellSize `div` 2)
          let c = p1 ^+^ vctHalf
          SDL.rectangle renderer p1 p2 white
          case cell of
            Empty -> return ()
            Blocked -> SDL.fillRectangle renderer p1 p2 white
            Alive p -> do
              SDL.fillCircle renderer c cellRadius (playerColor ! unPlayer p)
              SDL.fillCircle renderer c innerRadius black
            Dead p1 p2 -> do
              SDL.fillCircle renderer c cellRadius (playerColor ! unPlayer p1)
              SDL.fillCircle renderer c innerRadius (playerColor ! unPlayer p2)
          when available $ case game ^.gameCurrentTurn of
            GameOver -> return ()
            CurrentTurn p@(Player pi) _ -> if p == me
              then do
                SDL.P (SDL.V2 mx my) <- SDL.getAbsoluteMouseLocation
                when (my `div` cellSize == fromIntegral i && mx `div` cellSize == fromIntegral j) $
                  case cell of
                    Empty -> do
                      SDL.fillCircle renderer c cellRadius (playerColor ! unPlayer me `withAlpha` 70)
                      SDL.fillCircle renderer c innerRadius black
                    Alive _ -> do
                      SDL.fillCircle renderer c innerRadius (playerColor ! unPlayer me `withAlpha` 70)
                    _ -> return ()
                SDL.fillCircle renderer c markRadius (playerColor ! pi)
              else
                SDL.fillCircle renderer c markRadius (playerColor ! pi `withAlpha` 70)

    drawPlayers :: SDL.Renderer -> Game -> Player -> TTF.Font -> IO ()
    drawPlayers renderer game me font = do
      SDL.fillRectangle renderer (vec2 fieldX 0) (vec2 (fieldX + separatorWidth) fieldY) (playerColor ! unPlayer me)
      void $ V.generateM (V.length $ game ^. gamePlayers) drawPlayer
      where
        fieldX, fieldY :: Int
        fieldX = (V.length $ V.head (game ^.gameField)) * cellSize
        fieldY = (V.length $ game ^.gameField) * cellSize

        drawPlayer :: Int -> IO ()
        drawPlayer pi = do
          let x0 = fieldX + separatorWidth
          let y0 = pi * infoHeight
          let col = playerColor ! pi
          let cx = x0 + infoCellX
          let cy = y0 + infoHeight `div` 2
          SDL.fillCircle renderer (vec2 cx cy) infoCellRadius col
          SDL.fillCircle renderer (vec2 cx cy) infoCellInner black
          unless (((game ^. gamePlayers) ! pi) ^. isPlayerActive) $
            SDL.thickLine renderer (vec2 (cx - infoCellInner) (cy - infoCellInner)) (vec2 (cx + infoCellInner) (cy + infoCellInner)) 3 col
          case game ^. gameCurrentTurn of
            GameOver -> return ()
            CurrentTurn (Player p1) t -> when (p1 == pi) do
              let n = game ^. gameSubTurnCount
              void $ V.generateM t \i -> do
                let y = y0 + ((i * 2 + 1) * infoHeight) `div` (n * 2)
                SDL.fillCircle renderer (vec2 (x0 + infoTurnsX) y) infoTurnsRadius col
          let text = pack $ show (playerScore (Player pi) (game ^. gameField))
          let textPos = (vec2 (x0 + infoTextX) (y0 + (infoHeight - infoScoreHeight) `div` 2))
          drawText renderer font textPos text col
          when (pi == unPlayer me) $
            SDL.fillRectangle renderer (vec2 x0 y0) (vec2 (x0 + infoWidth) (y0 + infoHeight)) $ col `withAlpha` 100

    drawText :: SDL.Renderer -> TTF.Font -> SDL.Pos -> Text -> SDL.Color -> IO ()
    drawText renderer font pos text col = do
      surface <- TTF.solid font col text
      texture <- SDL.createTextureFromSurface renderer surface
      vec <- SDL.surfaceDimensions surface
      SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P pos) vec)
      SDL.destroyTexture texture
      SDL.freeSurface surface
      return ()

    separatorWidth, cellSize, cellRadius, innerRadius, markRadius :: Integral a => a
    cellSize = fromIntegral $
      min (650 `div` (V.length $ initialGame ^. gameField))
          (800 `div` (V.length $ V.head $ initialGame ^. gameField))
    cellRadius = (cellSize - 6) `div` 2
    innerRadius = cellRadius - min 4 (cellSize `div` 10)
    markRadius = 3
    separatorWidth = 15

    infoWidth, infoHeight, infoCellRadius, infoCellInner, infoCellX :: Integral a => a
    infoWidth = 200
    infoHeight = 66
    infoCellRadius = 31
    infoCellInner = 27
    infoCellX = 61

    infoTurnsX, infoTurnsRadius, infoScoreHeight, infoTextX :: Integral a => a
    infoTurnsX = 15
    infoTurnsRadius = 4
    infoScoreHeight = 40
    infoTextX = 100

    windowSize :: (Int, Int)
    windowSize = let f = initialGame ^. gameField in
      (cellSize * (V.length $ V.head f) + separatorWidth + infoWidth,
      max (cellSize * V.length f) ((V.length $ initialGame ^. gamePlayers) * infoHeight))

black, white :: SDL.Color
black = color 0 0 0
white = color 255 255 255

playerColor :: Vector SDL.Color
playerColor = V.fromList [
    color 255 0 0,
    color 0 255 0,
    color 0 0 255,
    color 0 255 255,
    color 255 0 255,
    color 255 255 0,
    color 255 128 0,
    color 254 185 234,
    color 180 180 180,
    color 90 0 157
  ]
