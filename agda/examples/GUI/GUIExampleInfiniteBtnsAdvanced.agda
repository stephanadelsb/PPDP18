


module GUI.GUIExampleInfiniteBtnsAdvanced   where


open import StateSizedIO.GUI.BaseStateDependent
open import StateSizedIO.writingOOsUsingIOVers4ReaderMethods hiding (nˢ) renaming(execˢⁱ to execᵢ ; returnˢⁱ to returnᵢ)
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import SizedIO.Base hiding (_>>_ ; _>>=_) renaming (return to returnIO)
open import SizedIO.Console hiding (main)

open import Data.String hiding (show)
open import GUI.GUIExampleLib

open import Data.Fin hiding (_+_)

open import Size
open import Data.Unit
open import Data.Product
open import Data.Nat
open import Data.Nat.Show using (show)
open import NativeIO renaming (nativeReturn to return;
                               _native>>=_ to _>>=_;
                               _native>>_ to _>>_)
open import GUI.GUICompilation hiding (main)
open import GUI.RasterificFFI

-- \GUIExampleInfiniteBtnsAdvanced
nFrame : (n : ℕ) →  Frame
nFrame 0        =  emptyFrame
nFrame (suc n)  =  addButton (show n) (nFrame n)
-- above n =  number of buttons

-- todo, consider renaming to: convertℕToStr

-- size descends only in the .obj (.method has hidden argument {j} where j < i
-- but not in .gui
-- therefore .gui needs to be defined separately as nFrame
-- if we define the value (n + (n ∸  (toℕ m))
--   by a where clause the system shows a termination check, which is avoided by inlining that value

-- TODO: remove "∀ {i}" and {i} in code example and give full type later in text
--

-- \GUIExampleInfiniteBtnsAdvanced
infiniteBtns :  ∀{i} → (n : ℕ) → GUI {i}
infiniteBtns n  .gui = nFrame n
infiniteBtns 0  .obj .method ()
infiniteBtns (suc n) .obj .method (m , _)  =
  returnGUI (infiniteBtns (n + finToℕ  m))
  where
    finToℕ = toℕ

--  Note that we didn't invert the button numbers, this was instead
--    done in GUICompilation.agda  for all GUIs
--       code
--           (invertFin n')
--  in definition of compile in GUICompilation.agda
--  If this choice is inconvinient one can replace
--           (invertFin n')
--   by
--           n'
-- and then instead   use in the code above
--      finToℕ  m
-- by
--      (invertFin (finToℕ  m))
--
-- and needsto add something in the text of PPDP18 (that we apply invertFin

-- infiniteBtns 0 .gui = emptyFrame
-- infiniteBtns (suc n) .gui = addButton (show n) (infiniteBtns n .gui)
--   where    m' = n + (n ∸  (toℕ m))


-- *** no longer used ***
-- was defining the gui component of infiniteBtns

-- remove (", _") ?
-- \GUIExampleInfiniteBtnsAdvanced

objn : ∀ {i} → (n : ℕ) → FrameObj {i} (nFrame n)
objn 0 .method ()
objn (suc n) .method (m , _) = returnIO (nFrame m' , objn m')
  where
      m' = n + toℕ m

--    m' = n + (n ∸  (toℕ m))


{-
main' : NativeIO Unit
main' = compileProgram (nFrame 1) (propn 1) (objn' 1)
-}

-- \GUIExampleInfiniteBtnsAdvanced
main : NativeIO Unit
main = do  win <- createWindowFFI
           compile win (infiniteBtns 3)
           nativePutStrLn "hello Agda"
