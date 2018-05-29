{-# LANGUAGE OverloadedStrings #-}


module DeclareGUI.ProcessEvents  where 

import DeclareGUI.ResolveConstraints

import DeclareGUI.Helpers 

import Control.Concurrent (threadDelay)
import Control.Monad as CM
import Data.Maybe (fromJust, isNothing, catMaybes)
import Data.Text (unpack)

import qualified SDL as SDL
import qualified SDL.Event as E

import DeclareGUI.SDLLIB

import DeclareGUI.Lib

import qualified DeclareGUI.Style as Style

import DeclareGUI.Definitions
import DeclareGUI.FrameToWindow (frame2window)
import DeclareGUI.Draw (renderWindow)

import DeclareGUI.Helpers

-- get max time + 1
--
processWindowEvents :: Integer -> Window -> IO Window
processWindowEvents nowTime window@(Window bx co events) = do
  threadDelay 15

  --eventsNew <- processEvents ((maxTime events) + 1)
  eventsNew <- processEvents nowTime {- 0 -}
  return $ Window bx co (events ++ eventsNew)
  where
    --
    maxTime :: [Event] -> Integer
    maxTime ev@(_ : _) = getMaxTimeEvents ev
    maxTime _ = 0


{-
-- assumes we already have the events in the window
getButtonClicked :: {-Integer -> -} Window -> IO (Maybe Int)
getButtonClicked {-now-} winWithEvents = do
  res <- resolveConstraintsOfWindow winWithEvents 0 {- now-}
  case res of
    Just (ButtonClicked num) -> return $ Just num
    _ -> return $ Nothing
-}

-- Lower level function, how internal int works
--
getButtonClickedFromWindow :: SDLWindow -> Int -> Window -> Time -> IO (Integer, [String])
getButtonClickedFromWindow ffiWindow@(SDLWindow _ renderer font) listLengthEvents win time = do
  winWithEvents@(Window boxes constr events) <- processWindowEvents time win

  res <- resolveConstraintsOfWindow winWithEvents time -- 0 {- now-}
  CM.when (Style.debug) $ case res of
    Nothing -> putStr ""
    Just x -> putStrLn ("µµµµµµµµ   " ++ show x)

  case res of
    -- Button Clicked
    (Just (ButtonClicked num)) -> do
      --putStrLn ("Constr." ++ show constr)
      return $ ((fromIntegral num), returnStrings)

    -- Texbox Clicked (focus change)
    (Just (TextboxClicked textboxName)) -> do
      CM.when (Style.debug) $ putStrLn ("^####^####^####^####^ Textboxclick  [" ++ textboxName ++ "]")
      let newEvents :: [Event]
          newEvents = events ++ [TextBoxFocusOn 0 {-now-} textboxName]
          --
          newWin :: Window
          newWin = (Window boxes constr newEvents)
               
          in getButtonClickedFromWindow ffiWindow (length newEvents) newWin (time + 1)

    Nothing -> do
      CM.when (length events > listLengthEvents) $ do
        threadDelay 10
        renderWindow font winWithEvents renderer
        CM.when (Style.debug) $  putStrLn "CHANGE =======>"
        CM.when (Style.debug) $  putStrLn $ show events
      
      getButtonClickedFromWindow ffiWindow (length events) winWithEvents time
   where
     --
     returnStrings :: [String]
     returnStrings = getTextboxInptus win


-- Used getEventsFFI; external interface
--
getButtonClickedFromFrame :: SDLWindow -> Frame -> IO (Integer, [String])
getButtonClickedFromFrame ffiwin frame = do
  let nComponents = fromIntegral (length buttonComponents)  -- count only the button components
  let window = frame2window frame
  winWithEvents@(Window boxes constraints events) <- processWindowEvents 0 window

  (numClicked, strTextboxInputs) <- getButtonClickedFromWindow ffiwin (length events) winWithEvents 0
  return $ ((nComponents - (fromIntegral numClicked)), strTextboxInputs)
  where
    buttonComponents = filter isButtonComp frame





--
-- Events
--
processEvents :: Time -> IO [Event]
processEvents time = do
  events <- E.pollEvents

  -- each event should have its own time
  --
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
       CM.when (Style.debug) $ puts $ "clicked:  " ++ show x ++ ", " ++ show y
       return $ Just (LeftButtonReleased time (fromIntegral x, fromIntegral y))

    (E.MouseMotionEvent x) -> do  -- later a underscore event
      return Nothing

    (E.KeyboardEvent
     (E.KeyboardEventData win E.Pressed repeat (SDL.Keysym scanCode (SDL.Keycode keyCode) modifier))) -> do
      CM.when (Style.debug) $ puts $ "//// \nKeyboard Event -- key: " ++ (show keyCode)
      case ((keyCode == 13),(keyCode == 8),(keyCode == 9))  of
        (True, _, _) -> do
          return $ Just $ (MetaKey time EnterFocusChange)
        (_, _, True) -> do
          return $ Just $ (MetaKey time EnterFocusChange)          
        (_, True,_) -> do
          return $ Just $ (MetaKey time Backspace)
        (_, _, _) -> return Nothing

    (E.TextInputEvent (E.TextInputEventData win txt )) -> do
      CM.when (Style.debug) $ puts $ "$&%§&/&%  TextInputEvent Event: " ++ (show txt)
      return $ Just $ Textinput time $ unpack txt

    x -> do
      --puts $ "underscore Event: " ++ (show x)
      return Nothing





