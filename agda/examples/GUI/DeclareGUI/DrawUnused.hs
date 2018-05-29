

{-
messages2boxs :: [Message] -> IO [Box]
messages2boxs messages = do
  return $ map (\name -> getLatestBox name messages) (getNames messages)
  --puts $ "latest : " ++ (show $ getLatestBox "hand" messages)
  --res <- sequence $ map (\name -> getLatestBox name messages) names
  --return res


drawWindow :: Window -> [Element]
drawWindow win@(Window messages _) =
  concat $ map (drawBox win) (getNames messages)
-}




{-
drawSubLabel :: Int -> Int -> String -> Element
drawSubLabel x y str = SubLabel (V2 (fromIntegral x) (fromIntegral y)) str


drawBox :: Window -> Name -> [Element]
drawBox win@(Window messages _) name
  | hasSubLabel && hasRectangle =
      [drawRectangle box color] ++ [drawSubLabel x y strSubLabel]

  | hasRectangle =
      [drawRectangle box color]

  | hasSubLabel =
      [drawSubLabel x y strSubLabel]

  | otherwise = []

  where
    relevantMessages =  filter (isOfName name) messages
    --
    labels = filter isSubLabel $ relevantMessages
    rectangles = filter isRectangle $ relevantMessages
    --
    hasSubLabel = length labels >= 1
    hasRectangle = length rectangles >= 1
    --
    (BecomeRectangle _ _ color) = head $ reverse $ sort $ rectangles
    --
    -- here we also need to query what changes to the lable where made as Textinput (!)
    --
    (BecomeSubLabel _ labelName strSubLabelUponCreation) = head $ reverse $ sort $ labels
    --
    strSubLabel = if (not (labelWasInFocus labelName messages)) then strSubLabelUponCreation
         else (strSubLabelUponCreation ++ getTextInput messages)
    --
    box@(na, ((x, y), (x2, y2))) = getLatestBox name messages
-}
