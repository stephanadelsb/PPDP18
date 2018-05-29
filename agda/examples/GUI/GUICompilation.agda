--@PREFIX@GUICompilation
{-# OPTIONS --allow-unsolved-metas #-}

module GUI.GUICompilation where

open import Data.String renaming (_++_ to _++Str_)
open import Data.List
open import Data.Fin hiding (_+_)
open import Data.Sum
open import Data.Unit
open import Data.Empty
open import Data.Product
open import Data.Bool
open import Data.Nat
--open import Data.Nat.Show renaming (show to showNat)
open import Data.Maybe.Base

open import Data.Product
open import Function

open import Size
open import SizedIO.Base using (IOInterface; Command; Response; IO) -- ; return)
open import SizedIO.Console using (consoleI; translateIOConsole)
open import StateSizedIO.GUI.BaseStateDependent

open import GUI.GUIDefinitionsBase
open import GUI.GUIDefinitions

open import GUI.GUIExampleLib

open import GUI.RasterificFFI -- hiding (main)

open import NativeIO renaming (nativeReturn to return;
                               _native>>=_ to _>>=_;
                               _native>>_ to _>>_)


open import GUI.GUIExampleTest
open import heap.libraryVec

open import GUI.Debug


maxFin : (n : ℕ) → Fin (suc n)
maxFin zero = zero
maxFin (suc n) = suc (maxFin n)

embed : {n : ℕ} → Fin n → Fin (suc n)
embed {zero} ()
embed {suc n} zero = zero
embed {suc n} (suc m) = suc (embed m)

-- not needed here but useful to add to library
predFin : {n : ℕ} → Fin n → Fin n
predFin {zero} ()
predFin {suc n} zero = zero
predFin {suc n} (suc m) = embed m


invertFin : {n : ℕ}(k :  Fin n) → Fin n
invertFin {zero} ()
invertFin {suc n} zero = maxFin n
invertFin {suc n} (suc k) = embed (invertFin k)

liftFinN : {A : Set} → (n : ℕ) → (Fin n → A) → A → (ℕ → A)
liftFinN zero f a m    = a
liftFinN (suc n) f a zero = f zero
liftFinN (suc n) f a (suc m) = liftFinN n (f ∘ suc) a m

liftStringList : {A : Set} → (n : ℕ) → (Tuple String n → A) → A → (List String → A)
liftStringList zero f a l = f _
liftStringList (suc n) f a [] = a
liftStringList (suc zero) f a (x ∷ l) = f x
liftStringList (suc (suc n)) f a (x ∷ l) = liftStringList (suc n) (λ y → f (x , y)) a l
{- liftStringList n (λ x → f (a , x)) a l -}

liftFinNStringList : {A : Set} → (n m : ℕ) → (Fin n → Tuple String m → A) → A → (ℕ → List String → A)
liftFinNStringList {A} n m f a = liftFinN n (λ k → liftStringList m (f k) a) (λ v → a)





mutual
  {-# NON_TERMINATING #-}
  triggerHsLoop : SDLWindow → GUI → (ℕ → List String → NativeIO GUI) → NativeIO Unit
  triggerHsLoop win (guic gui obj) f =
    nativePutStrLn "beginning of triggerHsLoop function" >>= λ _ →

    getEventsFFI win gui >>=  λ {
    (n , listStr) → f n listStr >>= λ gui →   
       nativePutStrLn (showNat "triggerHsLoop" n) >> compile win gui 
    }


  {-# NON_TERMINATING #-}
-- \GUICompilation
--@BEGIN@compile
  compile : SDLWindow → GUI → NativeIO Unit
--@END
  compile win (guic gui obj) =
         nativePutStrLn "beginning of compile function" >>= λ _ →
         renderFrameToScreenFFI win gui >>= λ _ →
         triggerHsLoop win (guic gui obj) (λ n s →
         (debug ((translateIOConsole (liftFinNStringList
                             (guiEl2NrButtons frameCmpStruc frame gui)
                             (guiEl2NrTextboxes frameCmpStruc frame gui)

                             -- If buttons are wrongly displayed, then check here and wether invertFin n' is needed or not
                             -- (λ n' s → obj .method ((invertFin n') , s))
                             (λ n' s → obj .method (debug (invertFin n') (showNat "n' object method call" (toℕ  n')) , s))
                             (SizedIO.Base.return (gui , obj)) n s))
         >>= λ {
                  (gui' , obj') → return (guic gui' obj') }) "debug translateIOConsole"))





--
-- Main
--

main : NativeIO Unit
main = do
  win <- createWindowFFI
  compile win (guic threeBtnGUI obj3Btn)
  nativePutStrLn "hello Agda"
