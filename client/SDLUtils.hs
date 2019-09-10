module SDLUtils
  ( withSDL
  , withTTF
  , withWindow
  , withRenderer
  , vec2
  , color
  , withAlpha
  ) where

import Control.Exception (finally)
import Data.IORef
import Data.Text
import Data.Word (Word8)
import Foreign.C (CInt)
import qualified SDL
import qualified SDL.Primitive as SDL
import qualified SDL.Font as TTF

withSDL :: IO a -> IO a
withSDL f = do
  SDL.initialize []
  finally f SDL.quit

withTTF :: IO a -> IO a
withTTF f = do
  TTF.initialize
  finally f TTF.quit

withWindow :: String -> (Int, Int) -> (SDL.Window -> IO a) -> IO a
withWindow title (x, y) f = do
  let size = SDL.V2 (fromIntegral x) (fromIntegral y)
  let config = SDL.defaultWindow { SDL.windowInitialSize = size }
  window <- SDL.createWindow (pack title) config
  SDL.showWindow window
  finally (f window) (SDL.destroyWindow window)

withRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withRenderer window f = do
  let config = SDL.RendererConfig {
    SDL.rendererType = SDL.AcceleratedVSyncRenderer,
    SDL.rendererTargetTexture = False
  }
  renderer <- SDL.createRenderer window (-1) config
  finally (f renderer) (SDL.destroyRenderer renderer)


vec2 :: Integral a => a -> a -> SDL.Pos
vec2 x y = SDL.V2 (fromIntegral x) (fromIntegral y)

color :: Word8 -> Word8 -> Word8 -> SDL.Color
color r g b = SDL.V4 r g b 255

withAlpha :: SDL.Color -> Word8 -> SDL.Color
withAlpha (SDL.V4 r g b _) = SDL.V4 r g b
