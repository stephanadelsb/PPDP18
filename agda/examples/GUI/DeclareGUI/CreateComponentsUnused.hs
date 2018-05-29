


{-
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
  (Sub (t + 3) rectangleName buttonName) :
  (Sub (t + 4) labelName buttonName) :
  --
  -- define elements
  (BecomeButton (t + 5) buttonName) :
  (BecomeRectangle (t + 6) rectangleName (0, 255, 0, 255)) : -- COLOR
  (BecomeSubLabel (t + 7) labelName label) :
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
-}
