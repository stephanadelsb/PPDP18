--@PREFIX@GUIExample
open import Data.Bool

module GUI.GUIExample   where

-- open import GUIgeneric.Prelude renaming (inj₁ to secondBtn; inj₂ to firstBtn)

-- open import GUIgeneric.PreludeGUI renaming (WxColor to Color) hiding ( _>>_)

open import StateSizedIO.GUI.BaseStateDependent
open import StateSizedIO.writingOOsUsingIOVers4ReaderMethods hiding (nˢ) renaming(execˢⁱ to execᵢ ; returnˢⁱ to returnᵢ)
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import SizedIO.Base hiding (_>>_)
open import SizedIO.Console

open import Data.String
open import GUI.GUIExampleLib

open import Data.Fin

open import Size
open import Data.Unit
open import Data.Product

-- TODO later Properties of Buttons?
--

oneBtnFrame : Frame
oneBtnFrame = addButton "OK" emptyFrame

twoBtnFrame : Frame
twoBtnFrame = addButton "Cancel" oneBtnFrame


putStrLine' : {A : Set} → String → (f : IO consoleI ∞ A) →
           IO consoleI ∞ A
putStrLine' s f = exec (putStrLn s) (λ _ → f)

syntax putStrLine' s f = putStrLine s >> f




mutual
--@BEGIN@ObjTwoBtn
 obj2Btn : ∀ {i} → FrameObj {i} twoBtnFrame
 obj2Btn .method (zero , _ ) = putStrLine "OK! Redefining GUI." >>
                        return (oneBtnFrame , obj1Btn)
 obj2Btn .method (suc zero , _) = putStrLine "Cancel." >>
                              return (twoBtnFrame , obj2Btn)
--@END



{-
--
-- CORRECT:
--
--

 obj2Btn .method (suc zero) = putStrLine "Cancel! No change." >>
                              return (twoBtnFrame , obj2Btn)
-}
 obj2Btn .method (suc (suc ()) , _)


--@BEGIN@ObjOneBtn
 obj1Btn : ∀ {i} → FrameObj {i} oneBtnFrame
 obj1Btn .method (zero , _ )  = putStrLine "OK! Redefining GUI." >>
                        return (twoBtnFrame , obj2Btn)
 obj1Btn .method (suc () , _)
--@END



mutual
  oneBtnGUI : ∀ {i} → GUI {i}
  oneBtnGUI .gui = addButton "OK" emptyFrame
  oneBtnGUI .obj .method (zero , _ )  = putStrLine "OK! Redefining GUI." >>
                                        returnGUI twoBtnGUI
  oneBtnGUI .obj .method (suc () , _ )


  twoBtnGUI : ∀ {i} → GUI {i}
  twoBtnGUI .gui = addButton "Cancel" (addButton "OK" emptyFrame)
  twoBtnGUI .obj .method (zero , _ ) = putStrLine "OK! Redefining GUI." >>
                                      returnGUI oneBtnGUI
  twoBtnGUI .obj .method (suc zero , _) = putStrLine "Cancel." >>
                              returnGUI twoBtnGUI
  twoBtnGUI .obj .method (suc (suc ()) , _)

-- GUIObj


{-
Not yet supported:
addTxtBox : String → Frame → Frame
addTxtBox str fr = addTxtBox' str fr optimized
-}

{-
Attributes/Properties not yet supported:
-- Attributes
--
Cols = ℕ
Margin  = ℕ
HSpace  = ℕ
VSpace  = ℕ

---@BEGIN@OneColumnLayout
oneColumnLayout : Cols × Margin × HSpace × VSpace
oneColumnLayout = (1 , 10 , 2 , 2)
---@END



---@BEGIN@Black
black : Color
---@END
black = rgb 0 0 0

---@BEGIN@AttrOneBtn
propOneBtn : properties oneBtnFrame
propOneBtn = black , oneColumnLayout
---@END

---@BEGIN@AttrTwoBtn
propTwoBtn : properties twoBtnFrame
propTwoBtn = black , black , oneColumnLayout
---@END
-}

{-
Doesn't make sense any longer. We always change the GUI currently!
keepGUI : {j : Size} → HandlerObject j twoBtnFrame →
            IO consoleI ∞
      (Σ-syntax (returnType twoBtnFrame)
       (λ r →
          IOObjectˢ consoleI handlerInterface j
          (nextStateFrame twoBtnFrame r)))
keepGUI = λ obj → return (noChange , obj)


changeGUI : ∀ {j} (g : CompEls frame) {g'} (prop : properties g) obj →
              IO consoleI ∞ (Σ (returnType g') (\r -> IOObjectˢ consoleI handlerInterface j (nextStateFrame g' r)))
changeGUI = λ g prop obj →  return (changedGUI g prop , obj)
-}




{- ######
compileProg : ∀ (a : CompEls frame) (b : properties a)
             (c : {i : Size} → GUIObj {i} a) → NativeIO Unit
compileProg = λ a b c →  compileProgram a b (c {∞})

---@BEGIN@Main
main : NativeIO Unit
main = compileProg twoBtnFrame propTwoBtn obj2Btn
---@END

--old
--main : NativeIO Unit
--main = compileProgram  twoBtnFrame propTwoBtn
--                       (obj2Btn {∞})
###-}
