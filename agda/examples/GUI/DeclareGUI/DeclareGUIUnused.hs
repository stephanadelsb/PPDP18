{-
--
-- Example
--

twoButtonGUI :: GUI
twoButtonGUI =
  GUI twoButtonFrame objectTwoButtonGUI


twoButtonWindow :: Window
twoButtonWindow = gui2Window twoButtonGUI

  

createButton :: String -> Component
createButton = Button

add :: Component -> Frame -> Frame
add c xs = xs ++ [c]

createFrame :: Frame
createFrame = []

oneButtonFrame :: Frame
oneButtonFrame = add (Button "OK") createFrame

twoButtonFrame :: Frame
twoButtonFrame = add (Button "Chancel") oneButtonFrame

--twoButtonWindow :: Window
--twoButtonWindow = frame2Window 0 

firstButton :: Int
firstButton = 1

secondButton :: Int
secondButton = 2

-}  

{-
objectOneButtonGUI :: GUIObject
objectOneButtonGUI 1 =
  return $ HandleGUI twoButtonFrame objectTwoButtonGUI

-- To make it total:
--
objectOneButtonGUI _ =
  return $ HandleGUI twoButtonFrame objectTwoButtonGUI
   

-- Is it possible to pattern match on an abbrivation for 1
--

objectTwoButtonGUI :: GUIObject
objectTwoButtonGUI 2 = do
  putStrLn ">>> click btn 2"
  return $ HandleGUI twoButtonFrame objectTwoButtonGUI
objectTwoButtonGUI 1 = do
  putStrLn ">>> click btn 1"
  return $ HandleGUI oneButtonFrame objectOneButtonGUI

-- to make it total:
--
objectTwoButtonGUI _ = do
  putStrLn ">>> click btn 1"
  return $ HandleGUI oneButtonFrame objectOneButtonGUI
-}
