module GUI.GUIDefinitionsBase where

open import Data.Unit
open import Data.Empty
open import Data.Bool
open import Data.String.Base


--\GUIDefinitionsBase
record CompStruc : Set₁ where 
  field 
    Components     :  Set
    Properties     :  Components → Set
    SubCompIndex   :  Components → Set
    subComp        :  (c : Components) → SubCompIndex c
                      → Components
    hasMethod      :  Components → Bool
    returnsStr     :  Components → Bool
open CompStruc public

--\GUIDefinitionsBase
data FrameCmpts : Set where
  frame button label textbox : FrameCmpts


data FrameSubCmptIndex : Set where
  buttonIdx labelIdx textboxIdx : FrameSubCmptIndex

FrameSubCmpIdx : FrameCmpts → Set
FrameSubCmpIdx frame = FrameSubCmptIndex
FrameSubCmpIdx _ = ⊥


frameSubCmpIdx2Cmpt : (c : FrameCmpts)(i : FrameSubCmpIdx c) → FrameCmpts
frameSubCmpIdx2Cmpt frame buttonIdx = button
frameSubCmpIdx2Cmpt frame labelIdx = label
frameSubCmpIdx2Cmpt frame textboxIdx = textbox
frameSubCmpIdx2Cmpt button ()
frameSubCmpIdx2Cmpt label ()
frameSubCmpIdx2Cmpt textbox ()

frameCmptsProp : FrameCmpts → Set
frameCmptsProp frame = ⊤
frameCmptsProp _     = String
-- in case of textbox it is the initial value inserted into a textbox

frameCmptHasMethod : FrameCmpts → Bool
frameCmptHasMethod button = true
frameCmptHasMethod _      = false


frameCmpIsTxtBox : FrameCmpts → Bool
frameCmpIsTxtBox textbox = true
frameCmpIsTxtBox _       = false


--\GUIDefinitionsBase
frameCmpStruc : CompStruc
Components   frameCmpStruc = FrameCmpts
SubCompIndex frameCmpStruc = FrameSubCmpIdx
subComp      frameCmpStruc = frameSubCmpIdx2Cmpt
Properties   frameCmpStruc = frameCmptsProp
hasMethod    frameCmpStruc = frameCmptHasMethod
returnsStr   frameCmpStruc = frameCmpIsTxtBox

--\GUIDefinitionsBase
--BEGIN@GUIel
data GUIel (cmpStruc : CompStruc)
           : (c : Components cmpStruc) → Set where
  createCmp : {c : Components cmpStruc} → Properties cmpStruc c
              → GUIel cmpStruc c
  addCmp : {c : Components cmpStruc} → GUIel cmpStruc c
              → (i : SubCompIndex cmpStruc c)
              → GUIel cmpStruc (subComp cmpStruc c i)
              → GUIel cmpStruc c
