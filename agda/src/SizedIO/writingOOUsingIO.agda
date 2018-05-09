{-# OPTIONS --postfix-projections #-}
--{-# OPTIONS --allow-unsolved-metas #-}


module SizedIO.writingOOUsingIO where


open import SizedIO.Object
open import SizedIO.Base
open import Data.Product
open import Data.Nat
open import Data.Fin
open import Data.Bool
open import Function
open import Data.Unit
open import Data.String
open import Data.Sum

open import Unit
open import Data.Bool.Base
open import Relation.Binary.PropositionalEquality
open import SizedIO.Console
open import Size
open import SizedIO.Base
open import NativeIO
open import heap.haskellNativeHeap



{-
module _ (I : IOInterface )
         (let C = Command I)(let R = Response I)
           where

  mutual

    data IOind (A : Set) : Set where

      exec''     : (c : C) (f : (r : R c) → IOind A)  → IOind A
      return'' : (a : A ) → IOind A

-}

{-
module _ {I : Interface }
           where

  translate : ∀{A : Set}
               → Object I
               → IOind (objectInterfToIOInterf I) A
               → (A × Object I)
  translate {A} obj (return'' x) = x , obj
  translate {A} obj (exec'' c p) = obj .objectMethod c ▹ λ {(x , o')
                                      → translate {A} o' (p x)  }
-}


module _ {I : Interface }
           where

  getA : ∀{A : Set}
               → Object I
               → IOind (objectInterfToIOInterf I) A
               → A
  getA {A} obj (return'' x) = x
  getA {A} obj p = let res = translate {I} {A} obj p
                        in proj₁ res


module _ (I : IOInterface )
         (let C = Command I)
         (let R = Response I)
           where

  mutual
    -- IOˢind' A sbegin send
    -- are programs which start in state sbegin and end in state send

    data IOind' (A : Set) : Set where

      exec'''     : (c : C)  →
                   (f : (r : R c) → IOind' A )
                  → IOind' A
      return''' : (a : A ) → IOind' A



module _ {I : Interface }
           where

  translate' : ∀{A : Set}
               → Object I
               → IOind' (objectInterfToIOInterf I) A
               →  (A × Object I)
  translate' {A} obj (return''' x) = (x , obj)
  translate' {A} obj (exec''' c p) = obj .objectMethod c ▹ λ {(x , o')
                                      → translate' {A} o' (p x)  }




module _ {I : Interface }
           where

  getA' : ∀{A : Set}
               → Object I
               → IOind' (objectInterfToIOInterf I) A
               → A
  getA' {A}  obj (return''' x) = x
  getA' {A} obj p = let res = translate' {I} {A} obj p
                    in proj₁ res





module _ (I₁ : IOInterface )
         (let C₁ = Command I₁)
         (let R₁ = Response I₁)
         (I₂ : IOInterface )
           where

  mutual



    record IOindcoind (i : Size)(A : Set) : Set where
      coinductive
      field
        forceIC : {j : Size< i} → IOindcoind+ j A

    IOindcoind+ : (i : Size)(A : Set) → Set
    IOindcoind+ i A = IOind I₂ (IOindcoindShape i A)


    data IOindcoindShape (i : Size)(A : Set) : Set where
      execic : (c₁ : C₁ )
           → ((r₁ : R₁ c₁) → IOindcoind i A )
           → IOindcoindShape i A
      returnic : IO' I₁ i A
                 → IOindcoindShape i A

{-
  delayic : {i : Size}{A : Set}
           → IOindcoindShape i A
           → IOindcoind (↑ i) A
  delayic {i} {A} P .IOindcoind.forceIC {j} = P
-}



module _ {I₁ : IOInterface }(let C₁ = Command I₁)
         (let R₁ = Response I₁)
         {I₂ : IOInterface }
           where
  open IOindcoind

  delayic : {i : Size}{A : Set}
           → IOindcoind+ I₁ I₂ i A
           → IOindcoind I₁ I₂ (↑ i) A
  delayic {i} {A} P .forceIC {j} = P


--Σ (State I₂) (λ s₄ → Σ (A s₃ s₄) (λ x → Object I₂ s₄))) s₁


--→ IOind I₂ (λ s₂ → A s₁ s₂) s₂


open IOindcoind public



module _ (I₁ : IOInterface )
         (let C₁ = Command I₁)
         (let R₁ = Response I₁)
         (I₂ : Interface )
         (let I₂' = objectInterfToIOInterf I₂)
           where
  mutual

    translateIndCoind : ∀{i} → {A : Set}
                 → Object  I₂
                 → IOindcoind I₁ I₂' i A
                 → IO I₁ i (A  × Object I₂ )
    translateIndCoind {i} {A} obj p .force {j}
             = translateIndCoind+ {j} {A} obj (p .forceIC {j})



    translateIndCoind+ : ∀{i} → {A : Set}
                 → Object  I₂
                 → IOindcoind+ I₁ I₂' i A
                 → IO' I₁ i (A  × Object I₂)
    translateIndCoind+ {i} {A} obj p =
                              let q : (IOindcoindShape I₁ I₂' i A
                                         × Object I₂ )
                                  q = translate {I₂} obj p
                              in translateIndCoindShape (proj₂ q) (proj₁ q)


    translateIndCoindShape : ∀{i} → {A : Set}
                 → Object  I₂
                 → IOindcoindShape I₁ I₂' i A
                 → IO' I₁ i (A × Object I₂ )
    translateIndCoindShape {i} {A} obj (execic c₁ p)
           = exec' c₁ (λ r₁ → translateIndCoind {i} {A} obj (p r₁))
    translateIndCoindShape {i} {A} obj (returnic  p)
           = fmap' i (λ a → (a , obj)) p

  mutual
    ioLeft2IODisjoint' : ∀ {i}{B}  → IO' I₁ i B → IO' (I₁ ⊎IOI I₂') i B
    ioLeft2IODisjoint' {i} {B} (exec' c f) = exec' (inj₁ c) λ r → ioLeft2IODisjoint {i} {B} (f r)
    ioLeft2IODisjoint' {i} {B} (return' a) = return' a

    ioLeft2IODisjoint : ∀ {i}{B}  → IO I₁ i B → IO (I₁ ⊎IOI I₂') i B
    force (ioLeft2IODisjoint {i}{B} p) {j}  = ioLeft2IODisjoint' {j} {B} (force p {j})


  mutual

    ioIndCoindToIO : ∀{i} → {A : Set} {B : Set}
                             → IOindcoind I₁ I₂' i B
                             → IO  (I₁ ⊎IOI I₂') i B
    ioIndCoindToIO {i}{A}{B} p .force {j} = ioIndCoindToIO+ {j}{A}{B} (p .forceIC {j})


    ioIndCoindToIO+ : ∀{i} → {A : Set} {B : Set}
                             → IOindcoind+ I₁ I₂' i B
                             → IO'  (I₁ ⊎IOI I₂') i B
    ioIndCoindToIO+ {i}{A}{B} (exec'' c f) = exec' (inj₂ c) (λ l →
                                           ioIndCoindToIO'+ {i}{A}{B} (f l) )
    ioIndCoindToIO+ {i}{A}{B} (return'' a) = ioIndCoindToIOShape{i}{A}{B} a


    ioIndCoindToIO'+ : ∀{i} → {A : Set} {B : Set}
                             → IOindcoind+ I₁ I₂' i B
                             → IO  (I₁ ⊎IOI I₂') i B
    ioIndCoindToIO'+ {i}{A}{B} (exec'' c f) = exec (inj₂ c) (λ l →
                                           ioIndCoindToIO'+ {i}{A}{B} (f l) )
    ioIndCoindToIO'+ {i}{A}{B} (return'' a) .force {j} = ioIndCoindToIOShape {i}{A}{B} a


    ioIndCoindToIOShape : ∀{i} → {A : Set} {B : Set}
                             → IOindcoindShape I₁ I₂' i B
                             → IO'  (I₁ ⊎IOI I₂') i B
    ioIndCoindToIOShape {i} {A}{B}(execic c₁ f) = exec' (inj₁ c₁) (λ r → ioIndCoindToIO {i} {A} {B} (f r))
    ioIndCoindToIOShape {i} (returnic p) = ioLeft2IODisjoint' p




module _ {objInf : Interface}
         (let objIOInf = objectInterfToIOInterf objInf)
           where

        run_startingWith_ : IOindcoind consoleI objIOInf ∞ Unit
              → Object objInf
              → NativeIO Unit
        run prog startingWith obj = translateIOConsole
                                    ((fmap ∞ (λ x → unit) (translateIndCoind consoleI objInf obj prog))) -- (translateIndCoind consoleI objInf obj prog)


        run+_startingWith_ : IOindcoind+ consoleI objIOInf ∞ Unit
              → Object objInf
              → NativeIO Unit
        run+ prog startingWith obj = translateIOConsole ((fmap ∞ (λ x → unit) (delay (translateIndCoind+ consoleI (ioInterfToObjectInterf objIOInf) obj prog))))




execIO : {i : Size}
       {I₁ :  IOInterface} {I₂ :  IOInterface}
       {A : Set}
      (c₁ : I₁ .Command) →
         ((r₁ : I₁ .Response c₁) →
          IOindcoind I₁ I₂ i  A )
        →  IOindcoindShape I₁ I₂ i A
execIO = execic



callMethod : {I₂ :  IOInterface}
             {A : Set}
             (c : Command I₂) →
               ((r : Response I₂ c) → IOind I₂ A ) →
               IOind I₂ A
callMethod = exec''



endIO : {I₁ :  IOInterface} {I₂ :  IOInterface}
       → IOind I₂ (IOindcoind I₁ I₂ ∞ Unit)
endIO {I₁} {I₂}  =
                return'' {I₂}
             (delayic (return'' (returnic (return' unit))))   --(returnic (return' unit)))


endIO+ : {I₁ :  IOInterface} {I₂ :  IOInterface}
--      {A : Set}
       → IOindcoind+ I₁ I₂ ∞ Unit
endIO+ = return'' (returnic (return' unit))



execIO+ : {i : Size}
        {I₁ :  IOInterface} {I₂ :  IOInterface}
--       {A : I₁ .IOState → Set}
       (c₁ : I₁ .Command ) →
       ((r₁ : I₁ .Response c₁) →
        IOindcoind+ I₁ I₂ i Unit )
       → IOindcoind+ I₁ I₂ i Unit
execIO+ {i} c p = return'' (execic c (λ r → delayic (p r)))


execIO+' : {i : Size}
        {I₁ :  IOInterface} {I₂ :  IOInterface}
       (c₁ : I₁ .Command) →
       ((r₁ : I₁ .Response c₁) →
        IOindcoind I₁ I₂ i Unit)
       → IOindcoind+ I₁ I₂ i Unit
execIO+' {i} c p = return'' (execic c (λ r → p r))



callProg : {I : IOInterface }{A : Set} (a : A ) → IOind I A
callProg = return''



{-
run' : IOindcoind consoleI CellInterfaceIO ∞ Unit
          empty
       → NativeIO Unit
run' prog = translateIOConsole
              (fmap ∞ (λ x → unit) unit
                (translateIndCoind ConsoleInterface
                                    (CellInterface String) cellPempty' (prog)))


module _ {objInf : Interface}
         (let objIOInf = objectInterfToIOInterf objInf)
         {objState : objIOInf .IOState }
           where

        run_startingWith_ : IOindcoind ConsoleInterface objIOInf ∞ (λ x y → Unit) unit objState
              → Object objInf objState
              → NativeIO Unit
        run prog startingWith obj = translateIOConsole
           (flatternIO  ConsoleInterface (λ c → c) (λ r → r) (fmap ∞ (λ x y → unit) unit
                                    (translateIndCoind ConsoleInterface objInf obj prog)))


        run+_startingWith_ : IOindcoind+ ConsoleInterface objIOInf ∞ (λ x y → Unit) unit objState
              → Object objInf objState
              → NativeIO Unit
        run+ prog startingWith obj = translateIOConsole
           (flatternIO  ConsoleInterface (λ c → c) (λ r → r) (fmap ∞ (λ x y → unit) unit
                     (delay (translateIndCoind+ ConsoleInterface (ioInterfToObjectInterf objIOInf) obj prog) ) ))
-}
