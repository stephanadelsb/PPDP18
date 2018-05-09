module StateSizedIO.BaseNonPoly where

-- this is a verison of the Base which is not polymorphic in
--    Universe Levels, i.e. doesn't refer to Levels.

-- It is better to use usually StateSizedIO.GUI.BaseStateDependent
--   instead
-- But for presentation purposes it might be more suitable since it is easier
-- to read

open import Size
open import SizedIO.Base

open import Data.Product

record IOInterfaceˢ  : Set₁ where
  field
    IOStateˢ   :  Set
    Commandˢ   :  IOStateˢ → Set
    Responseˢ  :  (s : IOStateˢ) → (m : Commandˢ s) → Set
    IOnextˢ    :  (s : IOStateˢ) → (m : Commandˢ s) → (Responseˢ s m) → IOStateˢ
open IOInterfaceˢ public


record Interfaceˢ : Set₁ where 
 field 
  Stateˢ    :  Set
  Methodˢ   :  Stateˢ → Set
  Resultˢ   :  (s : Stateˢ) → (m : Methodˢ s) → Set
  nextˢ  :  (s : Stateˢ) → (m : Methodˢ s) → (Resultˢ s m) → Stateˢ
open Interfaceˢ public

{-
module _
 (ioinf   : IOInterface) --  (let C  = Command ioi)  (let R   = Response ioi)
 (objinf  : Interfaceˢ)   {-(let S = Stateˢ oi)-} --(let M  = Methodˢ objinf)    (let Rt  = Resultˢ objinf)
                               -- (let n = nextˢ objinf)
 where
 @BEGIN@IOObject
  record IOObjectˢ (i : Size) (s : Stateˢ objinf) : Set where 
    coinductive
    field
 HIDE-END
      method :
        ∀{j : Size< i}
        (m : Methodˢ objinf s) →
        IO ioinf ∞ ( Σ[ r ∈ objinf .Resultˢ s m ]
                     IOObjectˢ j (objinf .nextˢ s m r))
  @END
-}


module _
  (ioinf  : IOInterface)
  (oinf  :  Interfaceˢ)
  where
 record IOObjectˢ (i : Size) (s : oinf .Stateˢ) : Set where
  coinductive 
  field
   method :
    ∀{j : Size< i}
     (m : oinf .Methodˢ s) →
     IO ioinf ∞ (Σ[  r ∈ oinf .Resultˢ s m ]
                     IOObjectˢ j (oinf .nextˢ s m r))


module _
  (ioi   : IOInterface) (let C  = Command ioi)  (let R   = Response ioi)
  (oi  : Interfaceˢ)  (let S = Stateˢ oi) (let M  = Methodˢ oi)    (let Rt  = Resultˢ oi)
                                (let n = nextˢ oi)
  where
  record IOObjectˢ- (i : Size) (s : S) : Set where
    coinductive
    field
      method : ∀{j : Size< i} (m : M s) → IO ioi ∞ (Rt s m )

open IOObjectˢ public
open IOObjectˢ- public



module _ (I : IOInterfaceˢ )
         (let S = IOStateˢ I) (let C = Commandˢ I)
         (let R = Responseˢ I) (let n = IOnextˢ I)
           where

  mutual
    record IOˢ (i : Size)  (A : S → Set) (s : S) : Set where
      coinductive
      -- constructor delay
      field
        forceˢ : {j : Size< i} → IOˢ' j A s

    data IOˢ' (i : Size)  (A : S → Set) (s : S) : Set where
      execˢ'      :  (c : C s) (f : (r : R s c) → IOˢ i A (n s c r)) → IOˢ' i A s
      returnˢ'  :  (a : A s) → IOˢ' i A s

    data IOˢ+ (i : Size)  (A : S → Set) (s : S) : Set where
      execˢ' : (c : C s)  (f : (r : R s c) → IOˢ i A (n s c r)) → IOˢ+ i A s

open IOˢ public

delayˢ : {i : Size}{I : IOInterfaceˢ}{A : IOStateˢ I → Set}{s : IOStateˢ I} → IOˢ' I i A s → IOˢ I (↑ i) A s
delayˢ p .forceˢ = p



module _  {I : IOInterfaceˢ }
          (let S = IOStateˢ I) (let C = Commandˢ I)
          (let R = Responseˢ I) (let n = IOnextˢ I)
           where
  returnˢ : ∀{i}{A : S → Set} (s : S) (a : A s) → IOˢ I i A s
  returnˢ s a .forceˢ = returnˢ' a

  -- 2017-04-05: Argument s is hidden now.
  execˢ : ∀{i}{A : S → Set} {s : S} (c : C s)
           (f : (r : R s c) → IOˢ I i A (n s c r))
           → IOˢ I i A s
  execˢ c f .forceˢ = execˢ' c f


  mutual
    fmapˢ : (i : Size) → {A B : S → Set} → (f : (s : S) → A s → B s)
            → (s : S)
            → IOˢ I i A s
            → IOˢ I i B s
    fmapˢ i {A} {B} f s p .forceˢ {j} = fmapˢ' j {A} {B} f s (p .forceˢ {j})


    fmapˢ' : (i : Size) → {A B : S → Set} → (f : (s : S) → A s → B s)
            → (s : S)
            → IOˢ' I i A s
            → IOˢ' I i B s
    fmapˢ' i {A} {B} f s (execˢ' c f₁) = execˢ' c (λ r → fmapˢ i {A} {B} f (IOnextˢ I s c r)  (f₁ r))
    fmapˢ' i {A} {B} f s (returnˢ' a) = returnˢ' (f s a)
