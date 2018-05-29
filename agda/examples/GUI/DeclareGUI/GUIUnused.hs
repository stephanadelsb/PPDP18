
{-
printWindowBoxes :: Window -> IO ()
printWindowBoxes (Window boxes _ _ _) = do
  putStrLn "\n\n ===="
  putStrLn $ show $ boxes
  putStrLn "\n\n ===="
-}

{-
getSubLabelNames :: [Message] -> [String]
getSubLabelNames ( (BecomeSubLabel _ labelName labelString) : xs) = labelName : getSubLabelNames xs
getSubLabelNames ( _ : xs) = getSubLabelNames xs
getSubLabelNames [] = []

getSubboxNamesOfBut :: [Message] -> String -> [String]
getSubboxNamesOfBut ( (Sub _ subBoxName butName) : xs) buttonName | (butName == buttonName) =
                                                                         subBoxName : getSubboxNamesOfBut xs buttonName
getSubboxNamesOfBut ( _ : xs ) butName = getSubboxNamesOfBut xs butName
getSubboxNamesOfBut [] _ = []


getSubLabelNameForButtonName :: [Message] -> String -> String
getSubLabelNameForButtonName messages butName =
  head $ L.intersect (getSubboxNamesOfBut messages butName) (getSubLabelNames messages)


getButtonNames :: [Message] -> [String]
getButtonNames ( (BecomeButton _ buttonName) : xs) = buttonName : getButtonNames xs
getButtonNames ( _ : xs) = getButtonNames xs
getButtonNames [] = []

--labelWasInFocus

printButtonsAndSubLabels :: Window -> IO ()
printButtonsAndSubLabels (Window messages _) = do
  putStrLn $ show $ buttonNames
  putStrLn "\n\n ===="
  putStrLn $ show $ map getSubLabel buttonNames
  --
  where
   getSubLabel :: String -> String
   getSubLabel butName = getSubLabelNameForButtonName messages butName
   --
   buttonNames :: [String]
   buttonNames = getButtonNames messages
-}


