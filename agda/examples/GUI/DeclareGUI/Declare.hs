




module DeclareGUI.Declare where

import Data.List (sort, reverse)
import DeclareGUI.Lib

import DeclareGUI.DeclareDefinitions
import DeclareGUI.DeclareHelpers




-- Rectangle of a Button
--
buttonRectangle :: Int -> Rectangle
buttonRectangle n = boxRect
  where
    marginLeft = 80
    width = 180
    height = 45
    vspace = 20
    --
    boxRect = ((marginLeft, (height + vspace) * n),
               (marginLeft + width, (height + vspace) * n + height))

-- Textlabel of a Button
--
labelRectangle :: Int -> Rectangle
labelRectangle n = labelRect
  where
    ((x, y), (x2, y2)) = buttonRectangle n
    --
    labelRect = ((x + 30, y + 30),
                 (x2 + 30, y2))

-- Make a button, with label etc. as sub boxes
--
makeButton :: Int -> String -> Time -> [Message]
makeButton n label t =
  -- create boxes
  (New t       (buttonName, boxRect)) :
  (New (t + 1) (rectangleName, boxRect)) :
  (New (t + 2) (labelName, labelRect)) :
  --
  -- define subrelation
  (SubBoxOf (t + 3) rectangleName buttonName) :
  (SubBoxOf (t + 4) labelName buttonName) :
  --
  -- define elements
  (SetButton (t + 5) buttonName) :
  (SetRectangle (t + 6) rectangleName (0, 255, 0, 255)) : -- COLOR
  (SetSubLabel (t + 7) labelName label) :
  --
  []
  where
    -- names
    strN = show n
    buttonName = "Button" ++ strN
    rectangleName = "Rectangle" ++ strN
    labelName = "SubLabel" ++ strN
    --
    boxRect = buttonRectangle n
    labelRect = labelRectangle n
  







