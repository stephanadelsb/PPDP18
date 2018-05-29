

{-
testMessages :: [Message]
testMessages =
  (makeButton 1 "1" 0) ++
  (makeButton 2 "2" 8) ++
  (makeButton 3 "3" 16) ++
  (makeButton 4 "4" 24) ++
  (makeButton 5 "5" 32)

  
testWindow :: IO Window
testWindow = do
  puts $ boxHasBeenMoved "hand" testMessages
  return $ Window testMessages [] -- (clickConstraint : []) --(followConstraint : constraintMouseMovesHand : [])
-}
{-
drawColor :: Color
drawColor = (0, 134, 193, 255)
-}

{-
transformBoxPosition :: Box -> Point -> Box
transformBoxPosition (name, ((x, y), (x', y'))) (relativePointX, relativePointY) =
  (name,
   ((x + relativePointX, y + relativePointY),
   (x' + relativePointX, y' + relativePointY)))
-}




{-
moveHandToLatestMousePosition :: Window -> Time -> Window
moveHandToLatestMousePosition win@(Window msgs constraints) time =
  (Window
    ((newMsg position) : msgs)
    constraints)
  where
    position = getLastMousePosition win time
    newMsg (x, y) = BoxMovedTo time "hand" ((x, y), (x + 100, y + 100))
-}

{-
-- move to the left of other box
--
moveBoxToOtherBoxPosition :: Name -> Name -> Window -> Time -> Window
moveBoxToOtherBoxPosition toBeMovedName otherBoxName win@(Window msgs constraints) time =
  let (_, ((xLeft, yLeft), (_, _))) = getLatestBox otherBoxName msgs
      box = getLatestBox toBeMovedName msgs
      width = getWidthBox box
      height = getHeightBox box
      --
      newMsg = BoxMovedTo
                  time
                  toBeMovedName
                  ((xLeft - width - 1, yLeft), (xLeft - 1, yLeft + height))
  
  in (Window (newMsg : msgs) constraints)
-}
