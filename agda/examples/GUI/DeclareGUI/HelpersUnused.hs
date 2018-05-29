

{-
Bad version:
getBoxByName :: Name -> [Box] -> Maybe Box
getBoxByName name ( b@( Box _ bxName _ _ _) : xs ) | name == bxName = Just b
getBoxByName name ( _ : xs ) = getBoxByName name xs
getBoxByName _ [] = Nothing
-}


{-
events2points :: [Event] -> [Point]
events2points ( (LeftButtonReleased _ p) : xs ) = p : event2point xs
events2points ( _ : xs ) = event2point xs
events2points [] = []
-}

{-    pointOverlaps = pointOverlapsBox box
    --
    boolList = map pointOverlaps pointsClicked
    --
    bool = all boolList
    --
    pointsClicked :: [Point]
    pointsClicked = events2points nowClicked
    --
    nowClicked :: [Event]
    nowClicked = filter isNowClicked events
-}

{- | (length nowClicked >= 1) &&
   (pointOverlapsBox pointClicked box) = True
 | otherwise = False -}


--  (==) (BoxMovedTo t1 n1 r1) (BoxMovedTo t2 n2 r2) =

--  (==) (BoxMovedTo t1 n1 r1) (BoxMovedTo t2 n2 r2) =
--    (t1 == t2) && (n1 == n2) && (r1 == r2)
