

module DeclareGUI.DeclareHelpers where

import Data.List (nub, sort)

import System.Exit (die)

import DeclareGUI.Lib

import DeclareGUI.DeclareDefinitions

instance Eq Message where
  (==) (New t1 b1) (New t2 b2) = (t1 == t2) && (b1 == b2)
  (==) (BoxMovedTo t1 n1 r1) (BoxMovedTo t2 n2 r2) =
    (t1 == t2) && (n1 == n2) && (r1 == r2)
  (==) (EventFired t1 e1) (EventFired t2 e2) =
    (t1 == t2) && (e1 == e2)
  (==) _ _ = False

-- WE SORT ONLY BY TIME !!
--
instance Ord Message where
  m1 `compare` m2 = (getTime m1) `compare` (getTime m2)

--
-- Is
--
isNew :: Message -> Bool
isNew (New _ _) = True
isNew _ = False

isSubLabel :: Message -> Bool
isSubLabel (SetSubLabel _ _ _) = True
isSubLabel _ = False

isRectangle :: Message -> Bool
isRectangle (SetRectangle _ _ _) = True
isRectangle _ = False


isBoxMovedTo :: Message -> Bool
isBoxMovedTo (BoxMovedTo _ _ _) = True
isBoxMovedTo _ = False

isOfName :: Name -> Message -> Bool
isOfName name (New _ (nam, _)) = (nam == name)
isOfName name (BoxMovedTo _ nam _) = (nam == name)
isOfName name (SetSubLabel _ nam _) = (nam == name)
isOfName name (SetRectangle _ nam _x) = (nam == name)
isOfName _ _ = False  

isEventFiredMouseMoved :: Message -> Bool
isEventFiredMouseMoved (EventFired _ (MouseMovedTo _)) = True
isEventFiredMouseMoved _ = False


--
-- SET
--
setSubLabel :: Name -> String -> Window -> Time -> Window
setSubLabel name str (Window messages constraints) time =
  (Window (newMessage : messages) constraints)
  where
    newMessage = (SetSubLabel time name str)


--
-- GET
--

getTime :: Message -> Time
getTime (New t _) = t
getTime (BoxMovedTo t _ _) = t
getTime (EventFired t _) = t
getTime (SetSubLabel t _ _) = t
getTime (SetRectangle t _ _) = t
-- getTime _ = -9999999999 -- DANGEROUS!!!
-- ==> I think better to have an exception!

getMaxTime :: [Message] -> Time
getMaxTime xs = maximum $ map getTime xs

getNamesOld :: [Message] -> IO [Name]
getNamesOld xs = do
  --puts $ nub $ getAllNames xs
  return $ nub $ getAllNames xs
  where
    getAllNames :: [Message] -> [Name]
    getAllNames ((New _ (name, _)) : xs) = name : getAllNames xs
    getAllNames ((BoxMovedTo _ name _) : xs) = name : getAllNames xs
    getAllNames ((EventFired _ _) : xs) = getAllNames xs
    getAllNames ((SetButton _ _) : xs) = getAllNames xs
    getAllNames ((SetSubLabel _ _ _) : xs) = getAllNames xs
    getAllNames ((SetRectangle _ _ _) : xs) = getAllNames xs
    getAllNames ((SubBoxOf _ _ _) : xs) = getAllNames xs
    getAllNames [] = []

getNames :: [Message] -> [Name]
getNames xs = do
  nub $ getAllNames xs
  where
    getAllNames :: [Message] -> [Name]
    getAllNames ((New _ (name, _)) : xs) = name : getAllNames xs
    getAllNames ((BoxMovedTo _ name _) : xs) = name : getAllNames xs
    getAllNames ((EventFired _ _) : xs) = getAllNames xs
    getAllNames ((SetButton _ _) : xs) = getAllNames xs
    getAllNames ((SetSubLabel _ _ _) : xs) = getAllNames xs
    getAllNames ((SetRectangle _ _ _) : xs) = getAllNames xs
    getAllNames ((SubBoxOf _ _ _) : xs) = getAllNames xs
    getAllNames [] = []

    

-- 1. has been moved: get latest movement
-- 2. has not been moved: get its new position
--
getLatestBox :: Name -> [Message] -> Box
getLatestBox name xs
  | boxHasBeenMoved name xs =  -- do
    -- NOTE: we need here last as the list is sorted with 0 at the start!!
    --
    -- puts $ "#### ###" ++ (show $ last $ map show (filter isBoxMovedTo messagesByName))
    {-return $-}
    getMovedBox $ last $ sort (filter isBoxMovedTo messagesByName)
  | otherwise =
      getNewBox $ head $ (filter isNew messagesByName)
      
  where    
    messagesByName = filter (isOfName name) xs
    --
    getMovedBox (BoxMovedTo _ name rect) = (name, rect)
    getMovedBox _ = ("ERROR BOX NOT FOUND", ((-999, -999), (-999, -999)))
    --
    getNewBox (New _ box) = box
    getNewBox _ = ("ERROR BOX NOT FOUND", ((-999, -999), (-999, -999)))


getWidthBox :: Box -> Int
getWidthBox (_, ((x1, _), (x2, _))) = x2 - x1

getHeightBox :: Box -> Int
getHeightBox (_, ((_, y1), (_, y2))) = y2 - y1

boxesOfSameName :: Name -> [Message] -> [Message]
boxesOfSameName name xs = filter (isOfName name) xs

boxesMovedOfName :: Name -> [Message] -> [Message]
boxesMovedOfName name xs =
  filter isBoxMovedTo $ filter (isOfName name) xs

getLastMousePosition :: Window -> Time -> Point
getLastMousePosition (Window msgs constraints) time =
  msg2point $ last $ filterMouseMovementsAfterTime time msgs
  where
    msg2point (EventFired _ (MouseMovedTo p)) = p
    msg2point _ = (-999, -999)


getlatestBoxMovementTime :: Name -> [Message] -> Time
getlatestBoxMovementTime name msgs =
  maximum $ map getTime messagesOfSameName
  where
    messagesOfSameName = filter (isOfName name) msgs


--
-- Filter
--
filterMessagesGreaterTime :: Time -> [Message] -> [Message]
filterMessagesGreaterTime time xs =
  filter (\m -> (getTime m) >= time) xs

filterMouseMovementsAfterTime :: Time -> [Message] -> [Message]
filterMouseMovementsAfterTime time msgs =
  filter isEventFiredMouseMoved $ filterMessagesGreaterTime time msgs


--
-- Predicate
--
-- Mouse has been moved 
mouseHasMovedAfterTime :: Window -> Time -> Bool
mouseHasMovedAfterTime (Window messages _) time =
  (length (filterMouseMovementsAfterTime time messages)) >= 1


boxHasBeenMoved :: Name -> [Message] -> Bool
boxHasBeenMoved name xs =
  (filter isBoxMovedTo (boxesOfSameName name xs)) /= []


pointOverlapsBox :: Point -> Box -> Bool
pointOverlapsBox (x, y) (_, ((xLeft, yLeft), (xRight, yRight)))
 | (inBetween x xLeft xRight) &&
   (inBetween y yLeft yRight) = True
 | otherwise = False

otherBoxMovedLater :: Name -> Name -> Window -> Time -> Bool
otherBoxMovedLater name otherName win@(Window msgs constraints) time =
  timeMovedOtherBox > timeMoved
  where
    timeMoved = getlatestBoxMovementTime name msgs
    timeMovedOtherBox = getlatestBoxMovementTime otherName msgs


wasBoxClickedNow :: Name -> Window -> Time -> Bool
wasBoxClickedNow boxName window@(Window messages constraints) time
 | (length nowClicked >= 1) &&
   (pointOverlapsBox pointClicked box) = True
 | otherwise = False
  where
    (EventFired _ (LeftButtonReleased pointClicked)) = head nowClicked
    --
    box = getLatestBox boxName messages
    --
    nowClicked :: [Message]
    nowClicked = filter isNowClicked messages
    --
    isNowClicked :: Message -> Bool
    isNowClicked (EventFired t (LeftButtonReleased _)) = t == time
    isNowClicked _ = False


-- Misc Helpers
--

appendMessage :: Window -> Message -> Window
appendMessage window@(Window messages constraints) msg =
  Window (messages ++ [msg]) constraints



