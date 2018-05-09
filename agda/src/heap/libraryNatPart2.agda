module heap.libraryNatPart2 where

open import Data.String
open import Data.Char
open import Data.Product
open import Data.Bool
open import Data.List
open import Data.Nat
open import Data.Maybe
open import heap.libraryNat
open import heap.libraryVec

checkCharIsDigit : Char → Bool
checkCharIsDigit c = 48 ≦ℕb toNat c ∧ (toNat c ≦ℕb 57)

checkCharListIsNum : List Char → Bool
checkCharListIsNum [] = true
checkCharListIsNum (c ∷ l) = checkCharIsDigit c ∧ checkCharListIsNum l

checkStrIsNum : String → Bool
checkStrIsNum str = checkCharListIsNum (primStringToList str)


-- the following function checks whether str is a string representing a number
--   and if it is between minVal and maxVal
-- if not just error  for an error message is displayed otherwise nothing is returned
strAsNum2ErrorMsgInclRangeaux :
                           (isNum : Bool)
                           (isInRange : Bool)
                           (errorMsgIfNotNum : String)
                           (errorMsgIfNotInRange : String)
                           → Maybe String
strAsNum2ErrorMsgInclRangeaux false isInRange errorMsgIfNotNum errorMsgIfNotInRange = just errorMsgIfNotNum
strAsNum2ErrorMsgInclRangeaux true false errorMsgIfNotNum errorMsgIfNotInRange = just errorMsgIfNotInRange
strAsNum2ErrorMsgInclRangeaux true true errorMsgIfNotNum errorMsgIfNotInRange = nothing

checkIsInRange : (n m k : ℕ) → Bool
checkIsInRange n m k = n ≦ℕb m ∧ m ≦ℕb k

strAsNum2ErrorMsgInclRange :
                           (errorMsgIfNotNum : String)
                           (minVal : ℕ)
                           (maxVal : ℕ)
                           (errorMsgIfNotInRange : String)
                           (str : String)
                           → Maybe String
strAsNum2ErrorMsgInclRange errorMsgIfNotNum minVal maxVal errorMsgIfNotInRange str
   = strAsNum2ErrorMsgInclRangeaux
       (checkStrIsNum str)
       (checkIsInRange (str2NatNonMaybe str) minVal maxVal)
       errorMsgIfNotNum errorMsgIfNotInRange


strPairAsNum2ErrorMsgInclRangeaux :
                           (isNum1st : Bool)
                           (isInRange1st : Bool)
                           (errorMsgIf1stNotNum : String)
                           (errorMsgIf1stNotInRange : String)
                           (isNum2nd : Bool)
                           (isInRangeSedond : Bool)
                           (errorMsgIf2ndNotNum : String)
                           (errorMsgIf2ndNotInRange : String)
                           → Maybe String
--  strPairAsNum2ErrorMsgInclRangeaux isNum1st isInRange1st errorMsgIf1stNotNum errorMsgIf1stNotInRange
--                                isNum2nd isInRangeSedond errorMsgIf2ndNotNum errorMsgIf2ndNotInRange
strPairAsNum2ErrorMsgInclRangeaux false isInRange1st errorMsgIf1stNotNum errorMsgIf1stNotInRange isNum2nd isInRangeSedond errorMsgIf2ndNotNum errorMsgIf2ndNotInRange = just errorMsgIf1stNotNum
strPairAsNum2ErrorMsgInclRangeaux true false errorMsgIf1stNotNum errorMsgIf1stNotInRange isNum2nd isInRangeSedond errorMsgIf2ndNotNum errorMsgIf2ndNotInRange = just errorMsgIf1stNotInRange
strPairAsNum2ErrorMsgInclRangeaux true true errorMsgIf1stNotNum errorMsgIf1stNotInRange false isInRangeSedond errorMsgIf2ndNotNum errorMsgIf2ndNotInRange = just errorMsgIf2ndNotNum
strPairAsNum2ErrorMsgInclRangeaux true true errorMsgIf1stNotNum errorMsgIf1stNotInRange true false errorMsgIf2ndNotNum errorMsgIf2ndNotInRange = just errorMsgIf2ndNotInRange
strPairAsNum2ErrorMsgInclRangeaux true true errorMsgIf1stNotNum errorMsgIf1stNotInRange true true errorMsgIf2ndNotNum errorMsgIf2ndNotInRange = nothing



strPairAsNum2ErrorMsgInclRange :
                           (errorMsgIf1stNotNum : String)
                           (minVal1stNum : ℕ)
                           (maxVal1stNum : ℕ)
                           (errorMsgIf1stNumNotInRange : String)
                           (errorMsgIf2ndNotNum : String)
                           (minVal2ndNum : ℕ)
                           (maxVal2ndNum : ℕ)
                           (errorMsgIf2ndNumNotInRange : String)
                           (str : Tuple String 2)
                           → Maybe String
strPairAsNum2ErrorMsgInclRange errorMsgIf1stNotNum minVal1stNum maxVal1stNum
                             errorMsgIf1stNumNotInRange
                             errorMsgIf2ndNotNum minVal2ndNum maxVal2ndNum
                             errorMsgIf2ndNumNotInRange (str , str' )
                             = strPairAsNum2ErrorMsgInclRangeaux
                               (checkStrIsNum str)
                               (checkIsInRange (str2NatNonMaybe str) minVal1stNum maxVal1stNum)
                               errorMsgIf1stNotNum errorMsgIf1stNumNotInRange
                               (checkStrIsNum str')
                               (checkIsInRange (str2NatNonMaybe str') minVal2ndNum maxVal2ndNum)
                               errorMsgIf2ndNotNum errorMsgIf2ndNumNotInRange



{-
errorMsgIfNotNum minVal maxVal errorMsgIfNotInRange str
   = strAsNum2ErrorMsgInclRangeaux
       (checkStrIsNum str)
       (minVal ≦ℕb str2NatNonMaybe str ∧ str2NatNonMaybe str ≦ℕb maxVal)
       errorMsgIfNotNum errorMsgIfNotInRange
-}


trivialCheckString : String → Maybe String
trivialCheckString str = nothing

trivialCheckString2 : Tuple String 2 → Maybe String
trivialCheckString2 str = nothing


strAsNum2ErrorMsgWoutRangeaux :
                           (isNum : Bool)
                           (errorMsgIfNotNum : String)
                           → Maybe String
strAsNum2ErrorMsgWoutRangeaux false errorMsgIfNotNum = just errorMsgIfNotNum
strAsNum2ErrorMsgWoutRangeaux true errorMsgIfNotNum =  nothing

strAsNum2ErrorMsgWoutRange :
                           (errorMsgIfNotNum : String)
                           (str : String)
                           → Maybe String
strAsNum2ErrorMsgWoutRange errorMsgIfNotNum str =
     strAsNum2ErrorMsgWoutRangeaux (checkStrIsNum str) errorMsgIfNotNum


strPairAsNum2ErrorMsgWoutRangeaux :
                           (isNum1st : Bool)
                           (errorMsgIf1stNotNum : String)
                           (isNum2nd : Bool)
                           (errorMsgIf2ndNotNum : String)
                           → Maybe String
strPairAsNum2ErrorMsgWoutRangeaux false errorMsgIf1stNotNum isNum2nd errorMsgIf2ndNotNum = just errorMsgIf1stNotNum
strPairAsNum2ErrorMsgWoutRangeaux true errorMsgIf1stNotNum false errorMsgIf2ndNotNum = just errorMsgIf2ndNotNum
strPairAsNum2ErrorMsgWoutRangeaux true errorMsgIf1stNotNum true errorMsgIf2ndNotNum = nothing

strPairAsNum2ErrorMsgWoutRange :
                           (errorMsgIf1stNotNum : String)
                           (errorMsgIf2ndNotNum : String)
                           (str : Tuple String 2)
                           → Maybe String
strPairAsNum2ErrorMsgWoutRange errorMsgIf1stNotNum errorMsgIf2ndNotNum ( str , str' ) =
          strPairAsNum2ErrorMsgWoutRangeaux (checkStrIsNum str) errorMsgIf1stNotNum (checkStrIsNum str') errorMsgIf2ndNotNum
