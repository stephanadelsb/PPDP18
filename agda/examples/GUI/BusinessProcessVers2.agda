--@PREFIX@BusinessProcessVersTWo
-- \BusinessProcessVersTWo

module GUI.BusinessProcessVers2 where


-- version contains business processes where inputs are checked and
--   if inptus are not valid one goes back to the input GUI.

open import Data.String
open import Data.List renaming (map to mapL)
open import Data.Product hiding (map)
open import Data.Nat
open import Data.Maybe
open import Size
open import Data.Fin renaming (_+_ to _+fin_)

open import heap.libraryVec
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import GUI.GUIExampleLib
open import GUI.GUIExampleLibPart2
open import StateSizedIO.GUI.BaseStateDependent

-- \BusinessProcess
--@BEGIN@BusinessProcess
data BusinessProcess : Set where
  startEvent :  String → BusinessProcess → BusinessProcess
  endEvent  :  String → BusinessProcess
  xor        :  List (String ×  BusinessProcess)
                →  BusinessProcess
  input      :  {n : ℕ} → Tuple String n
                →  (Tuple String n → Maybe String)
                →  (Tuple String n → BusinessProcess)
                →  BusinessProcess
  activity     :  String → BusinessProcess → BusinessProcess
--@END


-- with Tuple slit m into (str' , m)
--  and used str' for the first two and m for the last goal



mutual
  businessModel2GUI : ∀ {i}  → BusinessProcess
                             → GUI {i}
  businessModel2GUI (endEvent x) = endEventGUI x
  businessModel2GUI (xor l) = businessModel2GUIxor l
  businessModel2GUI (input {n} str g f) .gui = multiTextboxFrame n str
  businessModel2GUI (input {n} str g f) .obj .method {j}
             = multiTextboxGUIWithCheckFunObj n str (λ v → businessModel2GUI (f v))
               g
               (businessModel2GUI {j} (input {n} str g f)) .method

  businessModel2GUI (activity str b)  =   onebuttonStrGUI str  (businessModel2GUI b)
  businessModel2GUI (startEvent str b)  =   onebuttonStrGUI str  (businessModel2GUI b)

-- an unsized version for presentation purposes
-- \BusinessProcess
--@BEGIN@BusinessProcesstoGUI
  businessModel2Gui : BusinessProcess → GUI
--@END
  businessModel2Gui = businessModel2GUI {∞}

  businessModel2GUIxor : {i : Size} → List (String × BusinessProcess) → GUI {i}
  businessModel2GUIxor [] .gui = emptyFrame
  businessModel2GUIxor [] .obj .method ()
  businessModel2GUIxor ((str , _) ∷ l) .gui = addButton str (businessModel2GUIxor l .gui)
  businessModel2GUIxor ((str , g) ∷ l) .obj .method (zero , _) = returnGUI  (businessModel2GUI g)
  businessModel2GUIxor ((str , _) ∷ l) .obj .method (suc m , s) = businessModel2GUIxor l .obj .method (m , s)
