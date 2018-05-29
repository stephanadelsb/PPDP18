--@PREFIX@GUIExampleLibPartTwo
module GUI.GUIExampleLibPart2 where

open import Size
open import Data.Nat
open import Data.String
open import Data.Maybe.Base
open import StateSizedIO.GUI.BaseStateDependent
-- open import SizedIO.Base using (IOInterface; Command; Response; IO; return)
open import Data.Product
open import Relation.Binary.PropositionalEquality


open import heap.libraryVec
open import SizedIO.Console using (consoleI; translateIOConsole)
open import SizedIO.Base
open import GUI.GUIExampleLib
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase

lemmaNrTextboxesMultiTextBoxFrame  : (n : ℕ) (str : Tuple String n)
                                    → guiEl2NrTextboxes frameCmpStruc frame (multiTextboxFrame n str) ≡ n
lemmaNrTextboxesMultiTextBoxFrame zero str = refl
lemmaNrTextboxesMultiTextBoxFrame (suc zero) str = refl
lemmaNrTextboxesMultiTextBoxFrame (suc (suc n)) str = cong suc (lemmaNrTextboxesMultiTextBoxFrame (suc n) (proj₂ str))

transferNrTextboxesMultiTextBoxFrame : {A : Set}(n : ℕ) (str : Tuple String n)
                                       (check : Tuple String n → Maybe A)
                                       → Tuple String (guiEl2NrTextboxes frameCmpStruc frame (multiTextboxFrame n str))
                                       → Maybe A
transferNrTextboxesMultiTextBoxFrame n str check t rewrite  (lemmaNrTextboxesMultiTextBoxFrame n str)
             = check t


conditionalIO : {i : Size}
                (str : Maybe String) -- just s means input was wrong and an error message with this string is returned
                                     -- and you return to original gui
                                     -- nothing means was okay
                (originalGUI : GUI {i})
                (nextGUI     : IO consoleI ∞ (Σ[ g ∈ Frame ](FrameObj {i} g)))
                → IO consoleI ∞ (Σ[ g ∈ Frame ](FrameObj {i} g))
conditionalIO (just error) originalGUI nextGUI = returnGUI (onebuttonStrGUI error originalGUI)
conditionalIO nothing originalGUI nextGUI = nextGUI

multiTextboxGUIWithCheckFunObj
    : {i : Size} (n : ℕ) (v : Tuple String n)
      (f : Tuple String n → GUI {i})
      (check : Tuple String n  → Maybe String)
      (originalGui : GUI {i})
       →   GUIObj frameCmpStruc frame {↑ i} (multiTextboxFrame n v)
multiTextboxGUIWithCheckFunObj {i} n v f check originalGui .method (k , str) =
     conditionalIO (transferNrTextboxesMultiTextBoxFrame n v check str) originalGui (multiTextboxHandler n 0 v (λ v _ → f v) _ .method (k , str))



multiTextboxGUIWithCheckFun
    : {i : Size} (n : ℕ) (v : Tuple String n)
      (f : Tuple String n → GUI {i})
      (check : Tuple String n  → Maybe String)
      (originalGui : GUI {i})
       →   GUI {↑ i}
multiTextboxGUIWithCheckFun n v f check originalGui  .gui = multiTextboxFrame n v
multiTextboxGUIWithCheckFun {i} n v f check originalGui .obj .method (k , str) =
     conditionalIO (transferNrTextboxesMultiTextBoxFrame n v check str) originalGui (multiTextboxHandler n 0 v (λ v _ → f v) _ .method (k , str))
-- (guiEl2NrTextboxes frameCmpStruc frame (multiTextboxFrame n v))
