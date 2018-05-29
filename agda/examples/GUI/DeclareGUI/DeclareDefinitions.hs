

module DeclareGUI.DeclareDefinitions where


--
-- Primitive
--
type Time = Integer

type Point = (Int, Int)
type Name = String

type Rectangle = (Point, Point)
type Box = (Name, Rectangle)

--
--  GUI
--

data Component =
  Button String
  deriving (Eq, Ord, Show)

type Frame = [Component]


--
-- Main
--
type Color = (Int, Int, Int, Int)

data Event =
  MouseMovedTo Point
  | LeftButtonReleased Point
  deriving (Eq, Show)

data EventType =
  MouseMovement
  deriving (Eq, Show)


data Message =
  New Time Box
  | BoxMovedTo Time Name Rectangle
  --
  | SubBoxOf Time Name Name
  --
  | SetButton Time Name
  | SetSubLabel Time Name String -- name correspons to box name (that contains the position)
  | SetRectangle Time Name Color
  --
  | EventFired Time Event
  deriving (Show)


data Constraint =
  When (Window -> Time -> Bool) {-then-} Int  {- int is the nth button -}

{-
data Constraint =
  When (Window -> Time -> Bool) {-then-} Int  {- int is the nth button -}
  | WhenOld (Window -> Time -> Bool) {-then-} HandleGUI Int  {- int is the nth button -}
  | BasicWhen (Window -> Time -> Bool) {-then-} (Window -> Time -> Window)
       -- ;; time in predicate should always check for >= time
       -- as later recursivly new stuff beyond time could have been added
-}

  
data Window = Window [Message] [Constraint]

instance Show Window where
  show (Window msgs constraints) = ("[WINDOW]  " ++ show msgs)


resolveConstraintsOfWindow :: Window -> Time -> IO (Maybe Int)
resolveConstraintsOfWindow win@(Window msgs constraints) time =
  resolveConstraints constraints win time

--
-- !! Dangerous: Constraints are not recursive
--  if one constraint triggers another, this is currently ignored
--

resolveConstraints :: [Constraint] -> Window -> Time -> IO (Maybe Int)
resolveConstraints (constraint@(When pred num) : cs) win time
  | (pred win time) = return $ Just num
  | otherwise = resolveConstraints cs win time
resolveConstraints [] win time = return Nothing










