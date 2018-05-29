--@PREFIX@BusinessProcessMedExLib
-- \BusinessProcessMedExLib

module GUI.BusinessProcessMedExLib   where

open import  heap.libraryNat

open import Data.Nat
open import Data.Bool
open import Data.String renaming (_==_ to _==Str_)

-- for business process in PPCP'18 paper

{- We start by defining the values involved -}

RenalValue : Set
RenalValue = ℕ

-- \BusinessProcessMedExLib
--@BEGIN@RenalCat
data  RenalCat : Set where
      <25  ≥25<30 ≥30<50 ≥50 : RenalCat
--@END

renal2RenalCat  : ℕ → RenalCat
renal2RenalCat  n = if (n <ℕb  25) then <25
                    else (if (n <ℕb  30) then ≥25<30
                    else (if (n <ℕb  50) then ≥30<50
                    else ≥50))
str2RenalCat : String → RenalCat
str2RenalCat str = renal2RenalCat (str2ℕ str)

-- \BusinessProcessMedExLib
--@BEGIN@RenalCatGreaterEqualThirty
data RenalCat≥30  : Set where  ≥30<50 ≥50  : RenalCat≥30
--@END




data AgeCat : Set where <75  ≥75 : AgeCat

age2AgeCat  : ℕ → AgeCat
age2AgeCat  n = if (n <ℕb  75) then <75
                               else ≥75

str2AgeCat : String → AgeCat
str2AgeCat str = age2AgeCat (str2ℕ str)


{-
renalCat≥25-2-renalCat :  RenalCat≥25 → RenalCat
renalCat≥25-2-renalCat ≥25<30  = ≥25<30
renalCat≥25-2-renalCat ≥30     = ≥30
-}




data WghtCat : Set where
   ≤60 >60 : WghtCat

wght2WghtCat  : ℕ → WghtCat
wght2WghtCat  n = if (n ≦ℕb  60) then ≤60
                               else >60
str2WghtCat :  String → WghtCat
str2WghtCat str = wght2WghtCat (str2ℕ str)

data FallRisk : Set where
  fallRisk noFallRisk : FallRisk

patientHist2FallRisk : String → FallRisk
patientHist2FallRisk str = if primStringEquality str "yes" then fallRisk
                           else noFallRisk
