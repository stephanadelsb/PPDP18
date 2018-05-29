

module DeclareGUI.DeclareRenderEvents where

import Control.Monad as CM
import Data.Maybe (fromJust, isNothing, catMaybes)

import qualified SDL as SDL
import qualified SDL.Event as E

import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import DeclareGUI.SDLLIB

import DeclareGUI.Lib

import DeclareGUI.DeclareDefinitions
import DeclareGUI.DeclareDraw


renderWindow :: Font -> Window -> SDL.Renderer -> IO ()
renderWindow font window@(Window messages constraints) renderer = do
  let elements = drawWindow window
  renderImg renderer (imgFromElements font elements)

processEvents :: IO [Event]
processEvents = do
  events <- E.pollEvents
  r1 <- (mapM processEvent events) :: IO [Maybe Event]   --EventFired Time Event
  return $ catMaybes r1
        
processEvent :: E.Event -> IO (Maybe Event)
processEvent ev =
  case E.eventPayload ev of
    E.MouseMotionEvent x -> do
      let myPosition = getMousePosition x 
      -- puts myPosition
      return $ Just (MouseMovedTo myPosition)
      
    (E.MouseButtonEvent
     (E.MouseButtonEventData a E.Released c E.ButtonLeft e (SDL.P (SDL.V2 x y)))) -> do
       puts $ "clicked:  " ++ show x ++ ", " ++ show y
       return $ Just (LeftButtonReleased (fromIntegral x, fromIntegral y))
    _ -> do
      --puts $ "underscore Event: "
      return Nothing

event2message :: Time -> Event -> Message
event2message time event = EventFired time event

events2messages :: [Event] -> Time -> [Message]
events2messages events time = map (event2message time) events
