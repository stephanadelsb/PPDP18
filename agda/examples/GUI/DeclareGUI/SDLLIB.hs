{-# LANGUAGE OverloadedStrings #-}

module DeclareGUI.SDLLIB (initialize,
                          createWin,
                          createRenderer,
                          threadDelayAndQuit,
                          renderImg,
                          getMousePosition,
                          SDLWindow (..),
                          createWindow) where

import Data.Text as Text
import Control.Concurrent (threadDelay)

import Data.Vector.Generic (thaw)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable as V

import Data.Word (Word8)
import Foreign.C.Types (CInt)


import qualified Codec.Picture as CP
import qualified Codec.Picture.Types as CPT
import SDL.Vect (Point(P), V2(V2), V4(V4))

import qualified SDL as SDL
import SDL (($=))

import Codec.Picture.Types (PixelRGBA8 (..), Image)
import qualified SDL.Event as E
import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import DeclareGUI.Style (winWidth, winHeight)

data SDLWindow = SDLWindow SDL.Window SDL.Renderer Font

createWindow :: IO SDLWindow
createWindow = do
  Right font <- loadFontFile "GUI/DeclareGUI/fonts/DejaVuSans.ttf"

  initialize
  win <- (createWin "" winWidth winHeight) :: IO SDL.Window
  SDL.showWindow win

  renderer <- (createRenderer win) :: IO SDL.Renderer

  return $ SDLWindow win renderer font





getMousePosition :: E.MouseMotionEventData -> (Int, Int)
getMousePosition (E.MouseMotionEventData _ _ _ (SDL.P (SDL.V2 x y)) _) =
  (fromIntegral x , fromIntegral y)



initialize :: IO ()
initialize = SDL.initializeAll


createWin :: Text.Text -> Int -> Int -> IO SDL.Window
createWin str width heigtht = do
  win <- SDL.createWindow str SDL.defaultWindow {
             SDL.windowPosition = SDL.Centered,
             SDL.windowInitialSize =
                SDL.V2 (fromIntegral width) (fromIntegral heigtht)
             }
  SDL.showWindow win
  return win
   
  

createRenderer :: SDL.Window -> IO SDL.Renderer
createRenderer window =
  SDL.createRenderer window (-1) cfg
  where
    cfg =  SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer,
                                SDL.rendererTargetTexture = True }


presentRenderer :: SDL.Renderer -> IO ()
presentRenderer = SDL.present


threadDelayAndQuit :: SDL.Window -> SDL.Renderer -> IO ()
threadDelayAndQuit window renderer = do
  threadDelay 200000000
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


renderImg :: SDL.Renderer -> Image PixelRGBA8 -> IO ()
renderImg renderer img = do
  renderImage renderer img
  SDL.present renderer


-- Conversion part:
--

renderImage :: SDL.Renderer -> CP.Image CP.PixelRGBA8 -> IO ()
renderImage renderer img = do
  let surfaceSize :: SDL.V2 CInt
      surfaceSize = SDL.V2 (fromIntegral $ CPT.imageWidth img) (fromIntegral $ CPT.imageHeight img)

  surface <- image2SDLSurface img surfaceSize
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  let source = SDL.Rectangle (P $ SDL.V2 0 0) surfaceSize
      dest = SDL.Rectangle (P $ SDL.V2 0 0) surfaceSize
      angle = 0.0
      center = Nothing
      flipNone = SDL.V2 False False

  SDL.copyEx renderer texture (Just source) (Just dest) angle center flipNone
  SDL.destroyTexture texture

image2SDLSurface :: CP.Image CP.PixelRGBA8 -> SDL.V2 CInt -> IO SDL.Surface
image2SDLSurface image surfaceSize = do

  mutableVector <- convertToMutableVector (CP.imageData image)
  SDL.createRGBSurfaceFrom mutableVector surfaceSize pitch SDL.ABGR8888
  where
    pitch :: CInt
    pitch = 4 * fromIntegral (CP.imageWidth image)
    --
    convertToMutableVector :: Vector Word8 -> IO (IOVector Word8)
    convertToMutableVector = thaw
