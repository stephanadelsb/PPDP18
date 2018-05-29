


module DeclareGUI.FrameToWindow where


import DeclareGUI.Definitions
import DeclareGUI.Helpers
import DeclareGUI.CreateComponents

import qualified DeclareGUI.Style as Style

import Debug.Trace (trace)

frame2boxes :: Time -> Frame -> [Box]
frame2boxes t frameList =
  frame2boxes' vlist t [1..(length frameList)] frameList
  where
    vlist = cumulateList $ vspaceList frameList


frame2boxes' :: [Int] -> Time -> [Int] -> Frame -> [Box]
frame2boxes' vlist t (n : ns) ((ButtonComponent label) : cs) =
  (makeButton vlist n label t)  ++ (frame2boxes' vlist t ns cs)

frame2boxes' vlist t (n : ns) ((TextboxComponent label) : cs) =
  (makeTextbox vlist n label t)  ++ (frame2boxes' vlist t ns cs)

frame2boxes' vlist t (n : ns) ((LabelComponent label) : cs) =
  (makeLabel vlist n label t)  ++ (frame2boxes' vlist t ns cs)


frame2boxes' _ t [] [] = []
frame2boxes' _ _ _ _ = []


buttonNum2constraint :: Int -> Int -> Constraint
buttonNum2constraint positionN nthButtonAgda =
  When (wasBoxClickedNow buttonName) (ButtonClicked nthButtonAgda)
  where
    buttonName = "Button" ++ show positionN

textboxNum2constraint :: Int -> Constraint
textboxNum2constraint positionN =
  When (wasBoxClickedNow textboxName) (TextboxClicked textboxName)
  where
    textboxName = "Textbox" ++ show positionN


-- have the position on the screen
-- and have the button number (the nth button was clicked)
--
-- map buttonNum2constraint positions

frame2constraints :: Int -> Int -> Frame -> [Constraint]

frame2constraints positionN nthButtonAgda ((ButtonComponent str) : xs) =
  (buttonNum2constraint (positionN + 1) (nthButtonAgda + 1)) :
    frame2constraints (positionN + 1) (nthButtonAgda + 1) xs

frame2constraints positionN nthButtonAgda ((TextboxComponent unusedString) : xs) =
  (textboxNum2constraint (positionN + 1)) : 
    frame2constraints (positionN + 1) (nthButtonAgda) xs



frame2constraints positionN nthButtonAgda (_ : xs) =
    frame2constraints (positionN + 1) (nthButtonAgda) xs
    
frame2constraints positionN nthButtonAgda [] = []
  
    

getSubLabelFromTextbox :: Name -> [Box] -> Maybe Box
getSubLabelFromTextbox name boxes = box
   where
     Just (Box time na _ _ (_ : labelBoxName : _)) = getBoxByName name boxes
     box = getBoxByName labelBoxName boxes


{-
--
-- later both: Frame <==> Window?
-- assumes time "0"
--
-}

vspaceComponent :: Component -> Int
vspaceComponent (ButtonComponent _) = 45 + Style.buttonAdditionalVerticalOffset -- Style.labelVerticalOffset + Style.buttonAdditionalVerticalOffset
vspaceComponent (TextboxComponent _) = 40 -- Style.textboxVerticalOffset
vspaceComponent (LabelComponent _) = 50 -- Style.labelVerticalOffset

cumulateList :: [Int] -> [Int]
cumulateList = scanl1 (+)

verticalOffsetTop = 8

vspaceList :: Frame -> [Int]
vspaceList fr@(_ : _ : _ : _) = 0 : verticalOffsetTop : (tail $ map vspaceComponent fr)
vspaceList _ = 0 : (verticalOffsetTop + 30) : 70 : []  -- : 55 : []


frame2window :: Frame -> Window
frame2window frameList =
   Window boxes constraints textfocusEvent
   where
     boxes = (frame2boxes 0 frameList) 
     constraints = frame2constraints 0 0 frameList
     --
     textfocusEvent :: [Event]
     textfocusEvent = case (boxes2firstTextboxFocus boxes) of
                   Just x -> [x]
                   Nothing -> []
     --
     boxes2firstTextboxFocus :: [Box] -> Maybe Event
     boxes2firstTextboxFocus ( (Box _ name Textbox _ _ ) : xs ) = Just (TextBoxFocusOn 0 name)
     boxes2firstTextboxFocus (_ : xs) = boxes2firstTextboxFocus xs
     boxes2firstTextboxFocus [] = Nothing







