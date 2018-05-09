module heap.libraryVec where

open import Data.Nat
open import Data.Unit
open import Data.Product

TupleSimpl : (A : Set) → ℕ → Set
TupleSimpl A 0 = ⊤
TupleSimpl A (suc n) = A × TupleSimpl A n

Tuple : (A : Set) → ℕ → Set
Tuple A 0 = ⊤
Tuple A (suc 0) = A
Tuple A (suc n) = A × Tuple A n

headTuple : {A : Set}{n : ℕ}(v : Tuple A (suc n)) →  A
headTuple {A} {zero} v = v
headTuple {A} {suc n} (a , v) = a

tailTuple : {A : Set}{n : ℕ}(v : Tuple A (suc n)) →  Tuple A n
tailTuple {A} {zero} v = _
tailTuple {A} {suc n} (a , v) = v


_∷Vec_ : {A : Set}{n : ℕ}(a : A)(v : Tuple A n) → Tuple A (1 + n)
_∷Vec_ {A} {zero} a v = a
_∷Vec_ {A} {suc n} a v = a , v

snocVec : {A : Set}{n : ℕ}(v : Tuple A n)(a : A) → Tuple A (n + 1)
snocVec {A} {zero} v a = a
snocVec {A} {suc zero} v a = v , a
snocVec {A} {suc (suc n)} (a' , v) a = a' , snocVec v a


consVec : {A : Set}{n : ℕ}(a : A)(v : Tuple A n) → Tuple A (1 + n)
consVec {A} {zero} a v = a
consVec {A} {suc n} a v = a , v
