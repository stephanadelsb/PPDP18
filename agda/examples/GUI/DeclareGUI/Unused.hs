
{-
getButtonClickedFromOld :: Frame -> IO (Maybe Integer)
getButtonClickedFromOld frame = do
  let nComponents = fromIntegral (length frame)
  let window = frame2window frame
  winWithEvents <- processWindowEvents window
  
  maybeNum <- getButtonClicked {-0 time-} {-newWindow-} winWithEvents
  return $ (fmap (\x -> (nComponents - (fromIntegral x))) maybeNum) 
-}

