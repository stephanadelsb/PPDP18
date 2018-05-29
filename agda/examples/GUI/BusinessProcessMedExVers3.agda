--@PREFIX@BusinessProcessMedExVersThree
-- \BusinessProcessMedExVersThree

module GUI.BusinessProcessMedExVers3   where

-- example where the conditions when which medicine is prescribed
--   further brought in line with real world before
--   PPDP18

open import StateSizedIO.GUI.BaseStateDependent
open import StateSizedIO.writingOOsUsingIOVers4ReaderMethods hiding (nˢ) renaming(execˢⁱ to execᵢ ; returnˢⁱ to returnᵢ)
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import GUI.GUIModelRenamed
open import GUI.GUIExample
open import GUI.GUIExampleLib
open import GUI.BusinessProcess
open import GUI.BusinessProcessMedExLib

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
open import Relation.Nullary
open import Size
open import Data.Unit
open import Data.String renaming (_==_ to _==Str_)
open import heap.libraryVec


discharge : BusinessProcess
lowdoseSelection  : BusinessProcess
highdoseSelection  : BusinessProcess

-- \BusinessProcess
--@BEGIN@examplesBusinessmodel
discharge          =  endEvent "Discharge Patient"
lowdoseSelection   =  activity "Low Dose"   discharge
highdoseSelection  =  activity "High Dose"  discharge
--@END

-- \BusinessProcess
--@BEGIN@doseSelection
doseSelectionA : WghtCat → BusinessProcess
doseSelectionA ≤60 = lowdoseSelection
doseSelectionA >60 = highdoseSelection
--@END

-- \BusinessProcess
--@BEGIN@doseSelectionNotA
doseSelection¬A :  RenalCat≥30  →  AgeCat
                   → BusinessProcess
doseSelection¬A  ≥30<50  <75  =  lowdoseSelection
doseSelection¬A  ≥50     <75  =  highdoseSelection
doseSelection¬A  r       ≥75  =  lowdoseSelection
--@END

-- Note above that RenalCat is always ≥30 (use of type RenalCat≥30)

-- \BusinessProcess
--@BEGIN@NOACSelectionA
NOACSelectionA : WghtCat → BusinessProcess
NOACSelectionA w = activity "Med A" (doseSelectionA w)
--@END

--@BEGIN@NOACSelectionD
NOACSelectionD : RenalCat≥30 →  AgeCat → BusinessProcess
NOACSelectionD r a = activity "Med D" (doseSelection¬A r a)
--@END

-- \BusinessProcess
--@BEGIN@NOACSelectionAll
NOACSelectionAll : RenalCat≥30 →  AgeCat →  WghtCat → BusinessProcess
NOACSelectionAll r a w = xor (("Med A" , doseSelectionA   w) ∷
                              ("Med D" , doseSelection¬A  r a) ∷
                              [])
--@END


warfarin : BusinessProcess
warfarin  = activity "Prescribe warfarin"  discharge



medicationSelection : FallRisk → RenalCat → AgeCat → WghtCat → BusinessProcess
medicationSelection _          <25 _    _ = warfarin
medicationSelection fallRisk   ≥25<30 a w = warfarin
medicationSelection fallRisk   ≥30<50 a w = NOACSelectionD   ≥30<50 a
medicationSelection fallRisk   ≥50    a w = NOACSelectionD   ≥50    a
medicationSelection noFallRisk ≥25<30 a w = NOACSelectionA   w
medicationSelection noFallRisk ≥30<50 a w = NOACSelectionAll ≥30<50 a w
medicationSelection noFallRisk ≥50    a w = NOACSelectionAll ≥50    a w


mdChoice : FallRisk → RenalCat → AgeCat → WghtCat → BusinessProcess
mdChoice  f r a w = activity "MD Choice"
                     (medicationSelection f r a w)

diagnosis : FallRisk → RenalCat → AgeCat → WghtCat → BusinessProcess
diagnosis f r a w =  activity "Diagnosis"
                    (activity "MD Choice"
                    (medicationSelection f r a w))


-- \BusinessProcess
--@BEGIN@bloodTestRes
bloodTestRes :  FallRisk → AgeCat → WghtCat
                → BusinessProcess
bloodTestRes f a w =
    input "Enter Bloodtest Result"  λ str  →
    diagnosis f (str2RenalCat str ) a w
--@END

drawBlood : FallRisk → AgeCat → WghtCat → BusinessProcess
drawBlood f a w = activity "Draw Blood" (bloodTestRes f a w)

patientHistory : AgeCat → WghtCat → BusinessProcess
patientHistory a w =  input {2}  ("Enter fall/accident risk",
                                "Enter CHA2DS2-VASc-Score")
                                λ {(strFallR , strScore) →
                      drawBlood (patientHist2FallRisk strFallR) a w}

-- \BusinessProcess
--@BEGIN@patientRegistration
patientRegistration : BusinessProcess
patientRegistration = input {2} ("Enter patient age" , "Enter Wght")
                                 λ {(strAge , strWght)  →
                                     patientHistory (str2AgeCat strAge) (str2WghtCat strWght)}
--@END

-- \BusinessProcessMedExVersThree
--@BEGIN@patientRegistrationGUI
patientRegistrationGUI : GUI
patientRegistrationGUI = businessModel2GUI patientRegistration
--@END


{- We define some states -}

-- \BusinessProcess
--@BEGIN@BusinessProcesstoState
businessModel2State : BusinessProcess → State
businessModel2State b
    = state (businessModel2GUI b) notStarted
--@END


dischargeState : State
dischargeState = businessModel2State discharge

patientRegistrationState : State
patientRegistrationState = businessModel2State patientRegistration


NOACSelectionAState : WghtCat → State
NOACSelectionAState w = businessModel2State (NOACSelectionA w)

NOACSelectionDState : RenalCat≥30 → AgeCat → State
NOACSelectionDState r a = businessModel2State (NOACSelectionD r a)


warfarinState : State
warfarinState = businessModel2State warfarin

medicationSelectionState : FallRisk → RenalCat → AgeCat → WghtCat → State
medicationSelectionState f r a w = businessModel2State (medicationSelection f r a w)

mdChoiceState : FallRisk → RenalCat → AgeCat → WghtCat → State
mdChoiceState f r a w = businessModel2State (mdChoice f r a w)

diagnosisState : FallRisk → RenalCat → AgeCat → WghtCat → State
diagnosisState f r a w = businessModel2State (diagnosis f r a w)

{- *** here we have the state after all inputs *** -}


-- \BusinessProcess
--@BEGIN@stateAfterBloodTest
stateAfterBloodTest :
    (strAge strWght strFallR strScore strBlood : String)
    →  State
stateAfterBloodTest  strAge strWght strFallR strScore strBlood
                     =  guiNexts
                        patientRegistrationState
                        (nilCmd
                         >>>    textboxInput2  strAge  strWght
                         >>>    textboxInput2  strFallR strScore
                         >>>    btnClick
                         >>>    textboxInput   strBlood)
--@END

{- we show that if the renalvalue was for category <25 then we will reach the
   state where warfarin is prescribed -}

-- \BusinessProcess
--@BEGIN@theoremWarfarinAux
theoremWarfarinAux :  (f : FallRisk)(r : RenalCat)
                      (a : AgeCat)(w : WghtCat)
                      →  r ≡ <25
                      →  diagnosisState f r a w
                         -eventually-> warfarinState
theoremWarfarinAux r .<25 a w refl =
      next (λ _ → next (λ _ → hasReached))
--@END


-- \BusinessProcess
--@BEGIN@theoremWarfarin
theoremWarfarin :
     (strAge strWght strFallR strScore strBlood : String)
      →  str2RenalCat strBlood  ≡ <25
      →  stateAfterBloodTest  strAge strWght strFallR
                              strScore strBlood
         -eventually-> warfarinState
theoremWarfarin strAge strWght strFallR strScore strBlood  =
    theoremWarfarinAux (patientHist2FallRisk strFallR)
    (str2RenalCat strBlood) (str2AgeCat strAge)
    (str2WghtCat strWght)
--@END


{- a weaker statement which says that we reach Warfarin state in 2 button clicks -}

theoremWarfarinAux' : (f : FallRisk)(r : RenalCat) (a : AgeCat)(w : WghtCat)
                      → r ≡ <25
                     → diagnosisState f r a w -gui-> warfarinState
theoremWarfarinAux' r .<25 a w refl = step btnClick (step btnClick refl-gui->)


theoremWarfarin' : (strAge strWght strFallR strScore strBlood : String)
                  → str2RenalCat strBlood  ≡ <25
                  → stateAfterBloodTest strAge strWght strFallR strScore strBlood
                    -gui-> warfarinState
theoremWarfarin' strAge strWght strFallR strScore strBlood  = theoremWarfarinAux' (patientHist2FallRisk strFallR)
                                                     (str2RenalCat strBlood) (str2AgeCat strAge) (str2WghtCat strWght)



{- We show that if the renalvalue is <25, NOAC A state will not be reached -}
-- \BusinessProcess
--@BEGIN@theoremNoNOACAaux
theoremNoNOACA<25Aux : (f : FallRisk)(r : RenalCat) (a : AgeCat)(w : WghtCat)
                      → r ≡ <25
                      → {s : State}
                      → diagnosisState f r a w -gui-> s
                      → (w' : WghtCat)
                      → ¬ (s  ≡ NOACSelectionAState w')
--@END
theoremNoNOACA<25Aux f .<25 _ _ refl refl-gui-> _ ()
theoremNoNOACA<25Aux f .<25 _ _ refl (step _ refl-gui->) _ ()
theoremNoNOACA<25Aux f .<25 _ _ refl (step _ (step _ refl-gui->)) _ ()
theoremNoNOACA<25Aux f .<25 _ _ refl (step _ (step _ (step _ refl-gui->))) _ ()
theoremNoNOACA<25Aux f .<25 _ _ refl (step _ (step _ (step _ (step (() , _) _))))


-- \BusinessProcess`
--@BEGIN@theoremNoNOACA
theoremNoNOACA<25 :
   (strAge strWght strFallR strScore strBlood : String)
   →  str2RenalCat strBlood  ≡ <25
   →  {s : State}
   →  stateAfterBloodTest  strAge strWght strFallR
                           strScore strBlood
      -gui-> s
   →  (w' : WghtCat)
   →  ¬ (s  ≡ NOACSelectionAState w')
--@END
theoremNoNOACA<25 strAge strWght strFallR strScore strBlood = theoremNoNOACA<25Aux (patientHist2FallRisk strFallR)
                                                      (str2RenalCat strBlood) (str2AgeCat strAge)(str2WghtCat strWght)


{- We show that if the renalvalue is <25, NOAC D state will not be reached -}

theoremNoNOACD<25Aux : (f : FallRisk)(r : RenalCat) (a : AgeCat)(w : WghtCat)
                      → r ≡ <25
                      → {s : State}
                      → diagnosisState f r a w -gui-> s
                      → (r' : RenalCat≥30) (a' : AgeCat)
                      → ¬ (s  ≡ NOACSelectionDState r' a')
theoremNoNOACD<25Aux f .<25 _ _ refl refl-gui-> _ _ ()
theoremNoNOACD<25Aux f .<25 _ _ refl (step _ refl-gui->) _ _ ()
theoremNoNOACD<25Aux f .<25 _ _ refl (step _ (step _ refl-gui->)) _ _ ()
theoremNoNOACD<25Aux f .<25 _ _ refl (step _ (step _ (step _ refl-gui->))) _ _ ()
theoremNoNOACD<25Aux f .<25 _ _ refl (step _ (step _ (step _ (step (() , _) _))))

theoremNoNOACD<25 : (strAge strWght strFallR strScore strBlood : String)
                   → str2RenalCat strBlood  ≡ <25
                   → {s : State}
                   → stateAfterBloodTest strAge strWght strFallR strScore strBlood -gui-> s
                   → (r' : RenalCat≥30) (a' : AgeCat)
                   → ¬ (s  ≡ NOACSelectionDState r' a')
theoremNoNOACD<25 strAge strWght strFallR strScore strBlood = theoremNoNOACD<25Aux (patientHist2FallRisk strFallR)
                                                      (str2RenalCat strBlood) (str2AgeCat strAge)(str2WghtCat strWght)

{- ####
###-}
