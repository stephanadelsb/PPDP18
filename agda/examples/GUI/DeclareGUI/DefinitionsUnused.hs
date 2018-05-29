

data Event =
  MouseMovedTo Point
  | LeftButtonReleased Point
  deriving (Eq, Show)



{-
data EventType =
  MouseMovement
  deriving (Eq, Show)
-}


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
