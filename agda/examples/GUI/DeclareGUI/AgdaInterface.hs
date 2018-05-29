{-# LANGUAGE OverloadedStrings #-}


module DeclareGUI.AgdaInterface  where 

import Control.Concurrent (threadDelay)

import DeclareGUI.SDLLIB

import DeclareGUI.Definitions
import DeclareGUI.ProcessEvents
import DeclareGUI.FrameToWindow

import qualified DeclareGUI.Style as Style

import DeclareGUI.Helpers (isButtonComp, numberOfTextboxes)

import Control.Monad as CM

import qualified Data.Text as T

import DeclareGUI.ProcessEvents

import DeclareGUI.Lib

import DeclareGUI.Draw(renderWindow)



renderFrameToScreen :: SDLWindow -> Frame -> IO ()
renderFrameToScreen ffiWindow@(SDLWindow _ renderer font) frame = do
  let window = frame2window frame
  CM.when (Style.debug) $ putStrLn $ ("\n\n============\n" ++ show window)
  threadDelay 10

  renderWindow font window renderer
  return ()
  


-- Conversion functions for Agda (since Agda products are difficult with Hs FFI)
--
type NumAndStringList = (Integer, [String])

getNum :: NumAndStringList -> Integer
getNum (x , _) = x

getStringList :: NumAndStringList -> [T.Text]
getStringList (_ , strList) = map T.pack strList

--
-- FOR AGDA
--
getEventsFFI ::  SDLWindow -> Frame -> IO NumAndStringList
getEventsFFI ffiwin frame = do

  -- getting the Button blocks until a button is pressed
  --
  (num , returnStrings)  <- getButtonClickedFromFrame ffiwin frame

  CM.when (Style.debug) $ putStrLn (" num= " ++ show num)

  CM.when (lengthDiffer returnStrings) (putStrLn " $%&$%&%&$ >> lengths differ in function 'getEventsFFI' (!!)")
  
  return (num , map removeCharactersBeforeBackspace returnStrings) 
  where    
    --
    numberTxtboxes :: Int
    numberTxtboxes = numberOfTextboxes frame
    --
    lengthDiffer :: [String] -> Bool
    lengthDiffer retStrings = (length retStrings) /= numberTxtboxes




