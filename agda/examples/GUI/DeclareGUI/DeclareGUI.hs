


module DeclareGUI.DeclareGUI where

import Data.List as L

import DeclareGUI.DeclareDefinitions
import DeclareGUI.DeclareHelpers
import DeclareGUI.Declare


getSubLabelNames :: [Message] -> [String]
getSubLabelNames ( (SetSubLabel _ labelName labelString) : xs) = labelName : getSubLabelNames xs
getSubLabelNames ( _ : xs) = getSubLabelNames xs
getSubLabelNames [] = []

getSubboxNamesOfBut :: [Message] -> String -> [String]
getSubboxNamesOfBut ( (SubBoxOf _ subBoxName butName) : xs) buttonName | (butName == buttonName) =
                                                                         subBoxName : getSubboxNamesOfBut xs buttonName
getSubboxNamesOfBut ( _ : xs ) butName = getSubboxNamesOfBut xs butName
getSubboxNamesOfBut [] _ = []


getSubLabelForButtonName :: [Message] -> String -> String
getSubLabelForButtonName messages butName =
  head $ L.intersect (getSubboxNamesOfBut messages butName) (getSubLabelNames messages)


getButtonNames :: [Message] -> [String]
getButtonNames ( (SetButton _ buttonName) : xs) = buttonName : getButtonNames xs
getButtonNames ( _ : xs) = getButtonNames xs
getButtonNames [] = []

printButtonsAndSubLabels :: Window -> IO ()
printButtonsAndSubLabels (Window messages _) = do
  putStrLn $ show $ buttonNames
  putStrLn "\n\n ===="
  putStrLn $ show $ map getSubLabel buttonNames
  --
  where
   getSubLabel :: String -> String
   getSubLabel butName = getSubLabelForButtonName messages butName
   --
   buttonNames :: [String]
   buttonNames = getButtonNames messages

  
                                         
                                                                                   


frame2messages :: Time -> Frame -> [Message]
frame2messages t frameList =
  frame2messages' t [1..(length frameList)] frameList

frame2messages' :: Time -> [Int] -> Frame -> [Message]
frame2messages' t (n : ns) ((Button label) : cs) =
  makeButton n label t  ++ (frame2messages' (t + 8) ns cs)

frame2messages' t [] [] = []
frame2messages' _ _ _ = []


buttonNum2constraint :: Int -> Constraint
buttonNum2constraint n =
  When (wasBoxClickedNow buttonName) n
  where
    buttonName = "Button" ++ show n

frame2constraints :: Frame -> [Constraint]
frame2constraints frameList =
  map buttonNum2constraint [1..(length frameList)]


--
-- Frame <==> Window
--

-- assumes time "0"
--
frame2window :: Frame -> Window
frame2window frameList =
  Window (frame2messages 0 frameList) (frame2constraints frameList)

window2frame :: Window -> Frame  






 

 

