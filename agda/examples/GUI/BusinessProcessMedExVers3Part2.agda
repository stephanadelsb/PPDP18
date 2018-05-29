--@PREFIX@BusinessProcessMedExVersThreeParttwo
-- \BusinessProcess

module GUI.BusinessProcessMedExVers3Part2  where

-- More advanced properties verified for BusinessProcessVers3.agda

open import StateSizedIO.GUI.BaseStateDependent
open import StateSizedIO.writingOOsUsingIOVers4ReaderMethods hiding (nˢ) renaming(execˢⁱ to execᵢ ; returnˢⁱ to returnᵢ)
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import SizedIO.Base hiding (_>>_)
open import SizedIO.Console hiding (main)

open import Data.String
open import GUI.GUIExampleLib

open import Data.Fin renaming (_+_ to _+fin_)
open import Data.Nat
open import Data.Empty
open import Data.List renaming (map to mapL)
open import Data.Product hiding (map)
open import Relation.Binary.PropositionalEquality

open import Size
open import Data.Unit

open import Data.String renaming (_==_ to _==Str_)


open import GUI.GUIModelRenamed
open import GUI.GUIExample
open import GUI.GUIExampleLib
open import Relation.Nullary


open import heap.libraryVec
open import GUI.BusinessProcessMedExLib
open import GUI.BusinessProcessMedExVers3

lowdoseSelectionState : State
lowdoseSelectionState = businessModel2State lowdoseSelection

-- NOACSelectionDState : RenalCat → AgeCat → State
-- NOACSelectionDState r a = businessModel2State (NOACSelectionD r a)



--\BusinessProcessParttwo
--@BEGIN@theoremNoLowDosisauxthirty
theoremNoLowDosis<30aux : (f : FallRisk)(r : RenalCat) (a : AgeCat)(w : WghtCat)
                      → r ≡ ≥25<30
                      → (r' : RenalCat≥30)
                      → (a' : AgeCat)
                      → diagnosisState f r a w -gui-> NOACSelectionDState r' a'
                      → {s : State}
                      →  NOACSelectionDState r' a' -gui-> s
                      → ¬ (s  ≡ lowdoseSelectionState)
--@END
theoremNoLowDosis<30aux f .≥25<30 a w refl r' a' x₁ refl-gui-> ()
theoremNoLowDosis<30aux fallRisk .≥25<30 <75 ≤60 refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step (() , proj₄) x₁)))) (step c refl-gui->) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 <75 ≤60 refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 <75 >60 refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step (() , proj₄) x₁)))) (step c refl-gui->) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 <75 >60 refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 ≥75 w refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step (() , proj₄) x₁)))) (step c refl-gui->) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 ≥75 ≤60 refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 ≥75 >60 refl ≥30<50 <75 (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 a ≤60 refl ≥30<50 ≥75 (step c₁ (step c₂ (step c₃ (step (() , proj₄) x₁)))) (step c refl-gui->) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 a ≤60 refl ≥30<50 ≥75 (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 a >60 refl ≥30<50 ≥75 (step c₁ (step c₂ (step c₃ (step (() , proj₄) x₁)))) (step c refl-gui->) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 a >60 refl ≥30<50 ≥75 (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 a w refl ≥50 a' (step c₁ (step c₂ (step c₃ (step (() , proj₄) x₁)))) (step c refl-gui->) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a ≤60 refl ≥50 a' (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a >60 refl ≥50 a' (step c₁ (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁))))) (step c refl-gui->) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 a w refl r' a' (step c₂ (step c₃ (step c₄ (step (() , proj₄) x₁)))) (step c (step c₁ refl-gui->)) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a ≤60 refl r' a' (step c₂ (step c₃ (step c₄ (step c₅ (step (() , proj₄) x₁))))) (step c (step c₁ refl-gui->)) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a >60 refl r' a' (step c₂ (step c₃ (step c₄ (step c₅ (step (() , proj₄) x₁))))) (step c (step c₁ refl-gui->)) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 <75 ≤60 refl r' a' (step c₃ (step c₄ (step c₅ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ x₂))) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 <75 ≤60 refl r' a' (step c₃ (step c₄ (step c₅ (step c₆ (step (() , proj₄) x₁))))) (step c (step c₁ (step c₂ x₂))) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 ≥75 ≤60 refl r' a' (step c₃ (step c₄ (step c₅ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ refl-gui->))) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 ≥75 ≤60 refl r' a' (step c₃ (step c₄ (step c₅ (step c₆ (step (() , proj₄) x₁))))) (step c (step c₁ (step c₂ refl-gui->))) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 ≥75 ≤60 refl r' a' (step c₃ (step c₄ (step c₆ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ (step c₅ refl-gui->)))) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 ≥75 ≤60 refl r' a' (step c₃ (step c₄ (step c₆ (step c₇ (step (() , proj₄) x₁))))) (step c (step c₁ (step c₂ (step c₅ refl-gui->)))) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 ≥75 ≤60 refl r' <75 (step c₃ (step c₄ (step c₇ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ (step c₅ (step c₆ x₂))))) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 ≥75 ≤60 refl ≥30<50 <75 (step c₃ (step c₄ (step c₇ (step c₈ (step c₉ x₁))))) (step c (step c₁ (step (() , proj₄) (step c₅ (step c₆ x₂))))) refl
theoremNoLowDosis<30aux noFallRisk .≥25<30 ≥75 ≤60 refl ≥50 <75 (step c₃ (step c₄ (step c₇ (step c₈ (step c₉ x₁))))) (step c (step c₁ (step (() , proj₄) (step c₅ (step c₆ x₂))))) refl
theoremNoLowDosis<30aux f .≥25<30 ≥75 ≤60 refl ≥30<50 ≥75 (step c₃ (step c₄ x₁)) (step c (step c₁ (step (() , proj₄) (step c₅ (step c₆ x₂))))) refl
theoremNoLowDosis<30aux f .≥25<30 ≥75 ≤60 refl ≥50 ≥75 (step c₃ (step c₄ x₁)) (step c (step c₁ (step (() , proj₄) (step c₅ (step c₆ x₂))))) refl
theoremNoLowDosis<30aux fallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₅ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ refl-gui->))) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₅ (step c₆ (step (() , proj₄) x₁))))) (step c (step c₁ (step c₂ refl-gui->))) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₆ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ (step c₅ refl-gui->)))) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₆ (step c₇ (step (() , proj₄) x₁))))) (step c (step c₁ (step c₂ (step c₅ refl-gui->)))) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₇ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ (step c₅ (step c₆ refl-gui->))))) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₇ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ (step c₅ (step c₆ (step c₉ refl-gui->)))))) x₃
theoremNoLowDosis<30aux fallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₇ (step (() , proj₄) x₁)))) (step c (step c₁ (step c₂ (step c₅ (step c₆ (step c₉ (step c₁₀ x₂))))))) x₃
theoremNoLowDosis<30aux noFallRisk .≥25<30 a >60 refl r' a' (step c₃ (step c₄ (step c₇ (step c₈ (step (() , proj₄) x₁))))) (step c (step c₁ (step c₂ (step c₅ (step c₆ x₂))))) x₃

{-
theoremNoLowDosis<30auxUsingAuto : (f : FallRisk)(r : RenalCat) (a : AgeCat)(w : WghtCat)
                      → r ≡ ≥25<30
                      → (r' : RenalCat≥30)
                      → (a' : AgeCat)
                      → diagnosisState f r a w -gui-> NOACSelectionDState r' a'
                      → {s : State}
                      →  NOACSelectionDState r' a' -gui-> s
                      → ¬ (s  ≡ lowdoseSelectionState)
theoremNoLowDosis<30auxUsingAuto f r a w rproof r' a' path path2 s≡  = {!!}
-}

--\BusinessProcessParttwo
--@BEGIN@theoremNoLowDosisthirty
theoremNoLowDosis<30 :
   (strAge strWght strFallR strScore strBlood : String)
   →  str2RenalCat strBlood  ≡ ≥25<30
   →  (r' : RenalCat≥30)
   →  (a' : AgeCat)
   →  stateAfterBloodTest  strAge strWght strFallR
                          strScore strBlood
      -gui-> NOACSelectionDState r' a'
   →  {s : State}
   →  NOACSelectionDState r' a' -gui-> s
   →  ¬ (s  ≡ lowdoseSelectionState)
--@END
theoremNoLowDosis<30 strAge strWght strFallR strScore strBlood =
       theoremNoLowDosis<30aux
            (patientHist2FallRisk strFallR)
            (str2RenalCat strBlood)
            (str2AgeCat strAge)
            (str2WghtCat strWght)


{-
theoremNoLowDosis<30aux : (f : FallRisk)(r : RenalCat) (a : AgeCat)(w : WghtCat)
                      → r ≡ ≥30<50 ⊎ r ≡ ≥25<30 ⊎ r ≡ <25
                      → {s : State}
                      → diagnosisState f r a w -gui-> s3
                      → ¬ (s  ≡ lowdoseSelectionState)
theoremNoLowDosis<30aux f .≥30<50 a w (inj₁ refl) refl-gui-> ()
theoremNoLowDosis<30aux f .≥30<50 a w (inj₁ refl) (step c refl-gui->) ()
theoremNoLowDosis<30aux fallRisk .≥30<50 <75 ≤60 (inj₁ refl) (step c (step c₁ refl-gui->)) ()
theoremNoLowDosis<30aux noFallRisk .≥30<50 <75 ≤60 (inj₁ refl) (step c (step c₁ refl-gui->)) ()
theoremNoLowDosis<30aux fallRisk .≥30<50 <75 >60 (inj₁ refl) (step c (step c₁ refl-gui->)) ()
theoremNoLowDosis<30aux noFallRisk .≥30<50 <75 >60 (inj₁ refl) (step c (step c₁ refl-gui->)) ()
theoremNoLowDosis<30aux fallRisk .≥30<50 ≥75 w (inj₁ refl) (step c (step c₁ refl-gui->)1) ()
theoremNoLowDosis<30aux noFallRisk .≥30<50 ≥75 w (inj₁ refl) (step c (step c₁ refl-gui->)) ()
theoremNoLowDosis<30aux fallRisk .≥30<50 <75 ≤60 (inj₁ refl) (step c (step c₁ (step c₂ refl-gui->))) refl = {!!}
theoremNoLowDosis<30aux fallRisk .≥30<50 <75 >60 (inj₁ refl) (step c (step c₁ (step c₂ refl-gui->))) refl = {!!}
theoremNoLowDosis<30aux fallRisk .≥30<50 <75 w (inj₁ refl) (step c (step c₁ (step c₂ (step c₃ q)))) refl = {!!}
theoremNoLowDosis<30aux fallRisk .≥30<50 ≥75 w (inj₁ refl) (step c (step c₁ (step c₂ q))) refl = {!!}
theoremNoLowDosis<30aux noFallRisk .≥30<50 a w (inj₁ refl) (step c (step c₁ (step c₂ q))) refl = {!!}
theoremNoLowDosis<30aux f .≥25<30 a w (inj₂ (inj₁ refl)) q p' = {!!}
theoremNoLowDosis<30aux f .<25 a w (inj₂ (inj₂ refl)) q p' = {!!}
-}
