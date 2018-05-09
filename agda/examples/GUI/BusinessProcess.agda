-- \BusinessProcess

module GUI.BusinessProcess where

open import Data.String
open import Data.List renaming (map to mapL)
open import Data.Product hiding (map)
open import Data.Nat
open import Size
open import Data.Fin renaming (_+_ to _+fin_)

open import heap.libraryVec
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import GUI.GUIExampleLib
open import StateSizedIO.GUI.BaseStateDependent

-- \BusinessProcess
data BusinessModel : Set where
  terminate  :  String → BusinessModel
  xor        :  List (String ×  BusinessModel)
                →  BusinessModel
  input      :  {n : ℕ} → Tuple String n
                →  (Tuple String n → BusinessModel)
                →  BusinessModel
  simple     :  String → BusinessModel → BusinessModel


-- with Tuple slit m into (str' , m)
--  and used str' for the first two and m for the last goal



mutual
  businessModel2GUI : ∀ {i}  → BusinessModel
                             → GUI {i}
  businessModel2GUI (terminate x) = terminateGUI x
  businessModel2GUI (xor l) = businessModel2GUIxor l
  businessModel2GUI (input {n} str f) = multiTextboxGUI n str (λ v → businessModel2GUI (f v))
  businessModel2GUI (simple str b)  =   onebuttonStrGUI str (businessModel2GUI b)

-- an unsized version for presentation purposes
-- \BusinessProcess
  businessModel2Gui : BusinessModel → GUI
  businessModel2Gui = businessModel2GUI {∞}

  businessModel2GUIxor : {i : Size} → List (String × BusinessModel) → GUI {i}
  businessModel2GUIxor [] .gui = emptyFrame
  businessModel2GUIxor [] .obj .method ()
  businessModel2GUIxor ((str , _) ∷ l) .gui = addButton str (businessModel2GUIxor l .gui)
  businessModel2GUIxor ((str , g) ∷ l) .obj .method (zero , _) = returnGUI  (businessModel2GUI g)
  businessModel2GUIxor ((str , _) ∷ l) .obj .method (suc m , s) = businessModel2GUIxor l .obj .method (m , s)
