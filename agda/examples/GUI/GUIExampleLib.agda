--@PREFIX@GUIExampleLib
module GUI.GUIExampleLib where

open import Data.String
open import Data.List
open import Data.Fin hiding (_+_)
open import Data.Sum
open import Data.Unit
open import Data.Empty
open import Data.Product
open import Data.Bool
open import Data.Nat

open import Data.Product
open import Function

open import NativeIO
open import Size
open import SizedIO.Base using (IOInterface; Command; Response; IO; return)
open import SizedIO.Console using (consoleI; translateIOConsole)
open import StateSizedIO.GUI.BaseStateDependent
open import GUI.GUIDefinitionsBase
open import GUI.GUIDefinitions
open import GUI.RasterificFFI
open import heap.libraryVec

open import SizedIO.Base
open import SizedIO.Console using (putStrLn)

Button : Set
Button = GUIel frameCmpStruc button

Label : Set
Label = GUIel frameCmpStruc label

Textbox : Set
Textbox = GUIel frameCmpStruc textbox
--

createButton : String → Button
createButton str = createCmp {_}{button} str

createLabel : String → Label
createLabel str = createCmp {_}{label} str

-- should be a text box which a pre filled in value
createTextboxStr : String → Textbox
createTextboxStr str = createCmp {_}{textbox} str

-- should be a text box which has empty input initially (the default)
createTextbox : Textbox
createTextbox = createCmp {_}{textbox} ""


addButtonCmp : Button → Frame → Frame
addButtonCmp bt fr = addCmp fr buttonIdx bt

-- \GUIExampleLib
--@BEGIN@FrameElements
emptyFrame  :  Frame
addButton   :  String → Frame → Frame
addLabel    :  String → Frame → Frame
addTextbox  :  Frame → Frame
--@END


addButton str fr = addCmp fr buttonIdx (createButton str)


addLabel str fr = addCmp fr labelIdx (createLabel str)

addTextbox fr = addCmp fr textboxIdx createTextbox


--
emptyFrame = createCmp tt

buttonStr : String → Frame
buttonStr str = addButton str emptyFrame

onebuttonStrGUI : {i : Size} → String → GUI {i} → GUI {↑ i}
onebuttonStrGUI str f .gui = buttonStr str
onebuttonStrGUI str f .obj .method m = returnGUI f






endEventGUI : String → GUI {∞}
endEventGUI str .gui = addLabel str emptyFrame
endEventGUI str .obj .method ()

xorGUI : List (String × GUI {∞}) → GUI {∞}
xorGUI [] .gui = emptyFrame
xorGUI [] .obj .method ()
xorGUI ((str , g) ∷ l) .gui = addButton str (xorGUI l .gui)
xorGUI ((str , g) ∷ l) .obj .method (zero , _) = returnGUI g
xorGUI ((str , g) ∷ l) .obj .method (suc m , x) = xorGUI l .obj .method (m , x)


multiTextboxFrame : (n : ℕ) → Tuple String n → Frame
multiTextboxFrame 0 v = emptyFrame
multiTextboxFrame 1 str = addLabel str (addTextbox (addButton "Continue" emptyFrame))
multiTextboxFrame (suc (suc n)) (str , v) =
                  addLabel str
                  (addTextbox
                  (multiTextboxFrame (suc n) v  ))


{-
multiTextboxFrame : (n : ℕ) → Tuple String n → Frame
multiTextboxFrame 0 v = emptyFrame
multiTextboxFrame 1 str = addTextbox (addLabel str (addButton "Continue" emptyFrame))
multiTextboxFrame (suc (suc n)) (str , v) =
                  addTextbox
                  (addLabel str
                  (multiTextboxFrame (suc n) v  ))
-}
{-
test : IO {!!} {!!} {!!}
test = {!!}
-}

multiTextboxHandler : {i : Size}(n k : ℕ) (v : Tuple String n)
                      (f : Tuple String n → Tuple String k → GUI {i})
                      → Tuple String k
                      → FrameObj {↑ i} (multiTextboxFrame n v)
multiTextboxHandler 0 k v f v' .method (() , _)
multiTextboxHandler 1 k v f v' .method ( _ ,  str ) .force =
   exec' (putStrLn "Handler is activated >>") λ _ → returnGUI (f  str v' )
   --returnGUI (f  str v' )
multiTextboxHandler (suc (suc n)) k (str , v) f v' .method ( l  , m) =
            multiTextboxHandler (suc n) (suc k) v
             (λ v'' → λ v''' → f (headTuple m , v'')
                                 (tailTuple {String} v''') )
             (consVec {String} (headTuple m)  v')
             .method (l , tailTuple {String}  m)


multiTextboxGUI : ∀{i} → (n : ℕ) → (v : Tuple String n) → (f : Tuple String n → GUI {i}) →
  GUI {↑ i}
multiTextboxGUI n v f .gui = multiTextboxFrame n v
multiTextboxGUI n v f .obj = multiTextboxHandler n 0 v (λ v _ → f v) _
