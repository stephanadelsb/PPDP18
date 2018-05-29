--@PREFIX@GUIExample
open import Data.Bool

module GUI.GUIExampleTest   where

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

twoBtnGUI : Frame
twoBtnGUI = addButton "OK" (addButton "dummy" emptyFrame)

threeBtnGUI : Frame
threeBtnGUI = addButton "OK" (addButton "Chancel" (addButton "dummy" emptyFrame))


putStrLine' : {A : Set} → String → (f : IO consoleI ∞ A) →
           IO consoleI ∞ A
putStrLine' s f = exec (putStrLn s) (λ _ → f)

syntax putStrLine' s f = putStrLine s >> f

mutual
 obj3Btn : ∀ {i} → FrameObj{i} threeBtnGUI
 obj3Btn .method (suc (suc zero) , _) = putStrLine "OK! Changing Gui!" >>
                                    return (twoBtnGUI , obj2Btn)
 obj3Btn .method (suc zero , _) = putStrLine "Cancel! No change!" >>
                              return (threeBtnGUI , obj3Btn)
 obj3Btn .method (zero , _) = putStrLine ">>>>>>>>>>> zero button [NOT WORKING]" >>
                        return (threeBtnGUI , obj3Btn)

 obj3Btn .method (suc (suc (suc ())) , _)

--@BEGIN@ObjTwoBtn
 obj2Btn : ∀ {i} → FrameObj {i} twoBtnGUI
 obj2Btn .method (zero , _) = putStrLine ">>>>>>>>>>> zero button [NOT WORKING]" >>
                        return (twoBtnGUI , obj2Btn)
 obj2Btn .method (suc zero , _) = putStrLine "OK! Changing Gui!" >>
                              return (threeBtnGUI , obj3Btn)
 obj2Btn .method (suc (suc ()), _ )

--@END
