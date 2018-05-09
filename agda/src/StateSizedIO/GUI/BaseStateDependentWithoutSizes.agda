
module StateSizedIO.GUI.BaseStateDependentWithoutSizes where
-- This file should go to StateSizedIO.BaseStateDependent
-- since it has nothing to do with GUI

open import Size renaming (Size to AgdaSize)
open import NativeIO
open import Function
open import Agda.Primitive
open import Level using (_⊔_) renaming (suc to lsuc)
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Size
open import SizedIO.Base

{- this file is used for presenting in papers where we want to ignore sizes -}

record IOInterfaceˢ : Set₁ where
  field
    Stateˢ    : Set
    Commandˢ  : Stateˢ → Set
    Responseˢ : (s : Stateˢ) → Commandˢ s → Set
    nextˢ     : (s : Stateˢ) → (c : Commandˢ s) → Responseˢ s c → Stateˢ
open IOInterfaceˢ public

record Interfaceˢ  : Set₁  where 
 field 
  Stateˢ   :  Set
  Methodˢ  :  Stateˢ → Set
  Resultˢ  :  (s : Stateˢ)  →  Methodˢ s → Set
  nextˢ    :  (s : Stateˢ)  (m : Methodˢ s) → Resultˢ s m
              → Stateˢ
open Interfaceˢ public


-- rest of file  BaseStateDependent.agda is not yet converted to without sizes
