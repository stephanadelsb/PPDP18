

module DeclareGUI.Definitions where

import Graphics.Text.TrueType(PointSize (..) )

--
--  GUI
--

data Component =
  ButtonComponent String
  | TextboxComponent String
  | LabelComponent String    -- NOTE: "String" here is in almost all cases the empty String!

  deriving (Eq, Ord, Show)

type Frame = [Component]

--
-- Idea: at first the time is the position in the list??
--


--
-- Primitive
--
type Time = Integer

type Point = (Int, Int)
type Name = String

type Position = (Point, Point)

type Color = (Int, Int, Int, Int)

type LabelStr = String

--
-- Main
--
data MKey =
  EnterFocusChange
  | Backspace
  deriving (Eq, Show)
  

data Event =
  LeftButtonReleased Time Point
  | Textinput Time String
  | MetaKey Time MKey
  | TextBoxFocusOn Time Name 
  deriving (Eq, Show)

-- store declerative which label is highlighted
-- entering text adds to that label!
-- (later use a cursor)
--

-- Defines the Type and also stores the state/data it needs
--
data TypeState =
  Default
  | Button
  | Textbox
  | Label 
  | Rectangle Color
  | SubLabel LabelStr Color PointSize
  deriving (Eq, Show)
  

data Box = Box Time Name TypeState Position [Name] deriving (Eq, Show)
  

data ConstraintResult =
  ButtonClicked Int
  | TextboxClicked Name
  deriving (Eq, Show)
  

data Constraint =
  When (Window -> Time -> Bool) {-then-} ConstraintResult  {- int is the nth button -}



data Window = Window [Box] [Constraint] [Event] 


instance Show Window where
  show (Window boxes constraints events) = ("[WINDOW]  "
                                                        ++ " " ++ show boxes
                                                        ++ " " ++ show events)
  --
  -- TODO: instance EQ
  --


