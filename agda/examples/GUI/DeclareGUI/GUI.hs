


module DeclareGUI.GUI where

import Data.List as L

import DeclareGUI.Definitions
import DeclareGUI.Helpers
import DeclareGUI.CreateComponents




frame2boxes :: Time -> Frame -> [Box]
frame2boxes t frameList =
  frame2boxes' t [1..(length frameList)] frameList


frame2boxes' :: Time -> [Int] -> Frame -> [Box]
frame2boxes' t (n : ns) ((ButtonComponent label) : cs) =
  (makeButton n label t)  ++ (frame2boxes' t ns cs)

frame2boxes' t (n : ns) ((TextboxComponent label) : cs) =
  (makeTextbox n label t)  ++ (frame2boxes' t ns cs)

frame2boxes' t (n : ns) ((LabelComponent label) : cs) =
  (makeLabel n label t)  ++ (frame2boxes' t ns cs)


frame2boxes' t [] [] = []
frame2boxes' _ _ _ = []


buttonNum2constraint :: Int -> Int -> Constraint
buttonNum2constraint positionN nthButtonAgda =
  When (wasBoxClickedNow buttonName) nthButtonAgda
  where
    buttonName = "Button" ++ show positionN


-- have the position on the screen
-- and have the button number (the nth button was clicked)
--

-- map buttonNum2constraint positions

frame2constraints :: Int -> Int -> Frame -> [Constraint]

frame2constraints positionN nthButtonAgda ((ButtonComponent str) : xs) =
  (buttonNum2constraint (positionN + 1) (nthButtonAgda + 1)) :
    frame2constraints (positionN + 1) (nthButtonAgda + 1) xs

frame2constraints positionN nthButtonAgda (_ : xs) =
    frame2constraints (positionN + 1) (nthButtonAgda) xs
    
frame2constraints positionN nthButtonAgda [] = []
  


{-
frame2constraints :: Frame -> [Constraint]
frame2constraints frameList = map buttonNum2constraint positions
  where
    positions :: [Int]
    positions = selectButtonInts buttonPositions constraintListAll 
    --
    constraintListAll :: [Int]
    constraintListAll = [1..(length frameList)]
    --
    buttonPositions :: [Bool]
    buttonPositions = map isButtonComp frameList
    --
    selectButtonInts :: [Bool] -> [Int] -> [Int]
    selectButtonInts (True : xs)  (num : nums) = num : selectButtonInts xs nums
    selectButtonInts (False : xs) (_   : nums) = selectButtonInts xs nums
    selectButtonInts [] [] = []
    selectButtonInts _ _ = [] -- POTENTIALLY DANGEROUS
-}    
    

getSubLabelFromTextbox :: Name -> [Box] -> Maybe Box
getSubLabelFromTextbox name boxes = box
   where
     Just (Box time na _ _ (_ : labelBoxName : _)) = getBoxByName name boxes
     box = getBoxByName labelBoxName boxes


{-
--
-- later: Frame <==> Window?
-- assumes time "0"
--
-}
frame2window :: Frame -> Window
frame2window frameList =
   Window boxes constraints [] textfocus
   where
     boxes = frame2boxes 0 frameList
     constraints = frame2constraints 0 0 frameList
     textfocus = boxes2firstTextboxFocus boxes -- delete: (TextBoxFocusOn 0 "Textbox4") : []
     --
     boxes2firstTextboxFocus :: [Box] -> [TextFocus]
     boxes2firstTextboxFocus ( (Box _ name Textbox _ _ ) : xs ) = (TextBoxFocusOn 0 name) : []
     boxes2firstTextboxFocus (_ : xs) = boxes2firstTextboxFocus xs
     boxes2firstTextboxFocus [] = []







