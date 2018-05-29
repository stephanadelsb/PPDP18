

module DeclareGUI.RenderEvents where

import Control.Concurrent (threadDelay)
import Control.Monad as CM
import Data.Maybe (fromJust, isNothing, catMaybes)

import Data.Text (unpack)

import qualified SDL as SDL
import qualified SDL.Event as E

import Codec.Picture.Types (Image, PixelRGBA8 (..))
import Graphics.Rasterific (renderDrawing)

import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import DeclareGUI.SDLLIB

import DeclareGUI.Lib

import DeclareGUI.Definitions
import DeclareGUI.Draw

import DeclareGUI.GUI (frame2window)

import DeclareGUI.Style



--
-- Render
--

renderFrameToScreen :: SDLWindow -> Frame -> IO ()
renderFrameToScreen ffiWindow@(SDLWindow _ renderer font) frame = do
  let window = frame2window frame
  threadDelay 10

  renderWindow font window renderer
  return ()


renderWindow :: Font -> Window -> SDL.Renderer -> IO ()
renderWindow font window@(Window boxes constraints events textFocus) renderer = do
  --let elements =   -- in Draw.hs
  renderImg renderer pic 
  --(imgFromElements font elements)
  where
    pic :: Image PixelRGBA8
    pic = renderDrawing 800 600 windowBackgroundColor $ drawWindow font window



--
-- Events
--
processEvents :: Time -> IO [Event]
processEvents time = do
  events <- E.pollEvents
  r1 <- (mapM (processEvent time) events) :: IO [Maybe Event]   --EventFired Time Event
  return $ catMaybes r1

-- needs window??
-- or or simply say focus move down?
--
processEvent :: Time -> E.Event -> IO (Maybe Event)
processEvent time ev =
  case E.eventPayload ev of
{-    E.MouseMotionEvent x -> do
      let myPosition = getMousePosition x
      -- puts myPosition
      return $ Just (MouseMovedTo myPosition)-}

    (E.MouseButtonEvent
     (E.MouseButtonEventData a E.Released c E.ButtonLeft e (SDL.P (SDL.V2 x y)))) -> do
       puts $ "clicked:  " ++ show x ++ ", " ++ show y
       return $ Just (LeftButtonReleased time (fromIntegral x, fromIntegral y))

    (E.MouseMotionEvent x) -> do  -- later a underscore event
      return Nothing

    (E.KeyboardEvent
     (E.KeyboardEventData win E.Pressed repeat (SDL.Keysym scanCode (SDL.Keycode keyCode) modifier))) -> do
      case (keyCode == 13) of
        True -> do
          puts $ "//// \nKeyboard Event -- key: " ++ (show keyCode)
          return $ Just EnterFocusChange
          
        False -> return Nothing

    (E.TextInputEvent (E.TextInputEventData win txt )) -> do
      puts $ "$&%§&/&%  TextInputEvent Event: " ++ (show txt)
      return $ Just $ Textinput time $ unpack txt

    x -> do
      --puts $ "underscore Event: " ++ (show x)
      return Nothing


