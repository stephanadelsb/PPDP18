{-# LANGUAGE OverloadedStrings #-}


module DeclareGUI.MainGUI  where 

import Control.Concurrent (threadDelay)
import Control.Monad as CM

import Data.Maybe (fromJust, isNothing, catMaybes)

import qualified SDL as SDL
import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import DeclareGUI.SDLLIB

import DeclareGUI.Definitions
import DeclareGUI.RenderEvents
import DeclareGUI.GUI

import DeclareGUI.SDLLIB (SDLWindow)
import DeclareGUI.ResolveConstraints
import DeclareGUI.Helpers (isButtonComp, numberOfTextboxes)

import Control.Monad as CM

import qualified Data.Text as T

-- get max time + 1
--
processWindowEvents :: {-Integer -> -} Window -> IO Window
processWindowEvents {-nowTime-} window@(Window bx co events tf) = do
  threadDelay 15

  eventsNew <- processEvents 0
  return $ Window bx co (events ++ eventsNew) tf



-- assumes we already have the events in the window
getButtonClicked :: {-Integer -> -} Window -> IO (Maybe Int)
getButtonClicked {-now-} winWithEvents = do
  resolveConstraintsOfWindow winWithEvents 0 {- now-}


getButtonClickedFromWindow :: SDLWindow -> Int -> Window -> IO Integer
getButtonClickedFromWindow ffiWindow@(SDLWindow _ renderer font) listLengthEvents win = do
  winWithEvents@(Window boxes constr events tf) <- processWindowEvents win

  maybeNum <- getButtonClicked winWithEvents
  
  case maybeNum of
    (Just num) -> return $ fromIntegral num
    Nothing -> do
      CM.when (length events > listLengthEvents) $ do
        threadDelay 10
        renderWindow font winWithEvents renderer
        putStrLn "CHANGE =======>"
        putStrLn $ show events
      
      getButtonClickedFromWindow ffiWindow (length events) winWithEvents


getButtonClickedFromFrame :: SDLWindow -> Frame -> IO Integer
getButtonClickedFromFrame ffiwin frame = do
  let nComponents = fromIntegral (length buttonComponents)  -- count only the button components
  let window = frame2window frame
  winWithEvents@(Window boxes constraints events textFocus) <- processWindowEvents window

  numClicked <- getButtonClickedFromWindow ffiwin (length events) winWithEvents
  return $ nComponents - (fromIntegral numClicked)
  where
    buttonComponents = filter isButtonComp frame


-- Conversion functions for Agda (since Agda products are difficult with Hs FFI)
--
type NumAndStringList = (Integer, [String])

getNum :: NumAndStringList -> Integer
getNum (x , _) = x

getStringList :: NumAndStringList -> [T.Text]
getStringList (_ , strList) = map T.pack strList

getEventsFFI ::  SDLWindow -> Frame -> IO NumAndStringList
getEventsFFI ffiwin frame = do
  num <- getButtonClickedFromFrame ffiwin frame
  putStrLn (" num= " ++ show num)

  CM.when lengthDiffer (putStrLn " $%&$%&%&$ >> lengths differ in function 'getEventsFFI' (!!)")
  
  return (num , returnStrings) 
  where
    returnStrings :: [String]
    returnStrings = ["60", "60"] --["60", "60"] -- WARNING placeholder
    --
    numberTxtboxes :: Int
    numberTxtboxes = numberOfTextboxes frame
    --
    lengthDiffer :: Bool
    lengthDiffer = (length returnStrings) /= numberTxtboxes




