module Main  where 


import DeclareGUI.Definitions
import DeclareGUI.GUI
import DeclareGUI.MainGUI
import DeclareGUI.SDLLIB (SDLWindow, createWindow)
import DeclareGUI.RenderEvents



renderFrame :: SDLWindow -> HandleGUI -> IO ()
renderFrame ffiwin handleGUI@(HandleGUI frame obj) = do
  renderFrameToScreen ffiwin frame
  
  -- debug:
  putStrLn $ show $ frame
  putStrLn $ "frame equals frame: " ++ show (frame == frame) ++ "\n\n ===="
  putStrLn $ show $ frame2window frame
  putStrLn "\n ==== {{{{{ " 
  --  printWindowBoxes $ frame2window frame
  --
  --
  --
  numClicked <- getButtonClickedFromFrame ffiwin frame
  putStrLn ("> >>> %&>   found num:  "  ++ show numClicked)
  newHandleGUI <- obj (fromIntegral numClicked)
  --
  renderFrame ffiwin newHandleGUI


main :: IO ()
main = do
  win <- createWindow
  renderFrame win (HandleGUI twoButtonFrameTwoTextbox objectTwoButtonGUI)



--
-- Definitions
--

type GUIObject = Int -> IO HandleGUI

data HandleGUI =
  HandleGUI Frame GUIObject


  
--
-- Example
--

-- Frame
--
createFrame :: Frame
createFrame = []


add :: Component -> Frame -> Frame
add c xs = xs ++ [c]


oneButtonFrame :: Frame
oneButtonFrame = add (ButtonComponent "OK") createFrame

--twoButtonFrame :: Frame
--twoButtonFrame = add (ButtonComponent "Chancel") oneButtonFrame

twoButtonFrameTwoTextbox :: Frame
twoButtonFrameTwoTextbox =
  add (TextboxComponent "Textbox2:") (add (TextboxComponent "Textbox1:")
                                      (add (LabelComponent "Label1:") (
                                          (add (ButtonComponent "Cancel") oneButtonFrame))))


-- Objects
--

-- Objects
--

objectOneButtonGUI :: GUIObject
objectOneButtonGUI 0 = do
  putStrLn ">>> click btn 1 / OK"
  return $ HandleGUI twoButtonFrameTwoTextbox objectTwoButtonGUI

-- To make it total:
--
objectOneButtonGUI _ =
  return $ HandleGUI twoButtonFrameTwoTextbox objectTwoButtonGUI
   


objectTwoButtonGUI :: GUIObject
objectTwoButtonGUI 1 = do
  putStrLn ">>> click btn 1 / OK"
  return $ HandleGUI oneButtonFrame objectOneButtonGUI
objectTwoButtonGUI 0 = do
  putStrLn ">>> click btn 0 / Cancel"
  return $ HandleGUI twoButtonFrameTwoTextbox objectTwoButtonGUI

-- to make it total:
--
objectTwoButtonGUI x = do
  putStrLn $ "ERROR TOTAL BUTTON CLICKED: " ++ show x
  return $ HandleGUI oneButtonFrame objectOneButtonGUI

