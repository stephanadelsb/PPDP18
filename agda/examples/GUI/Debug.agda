

module GUI.Debug where


open import Data.Nat
open import Data.String renaming (_++_ to _++Str_)

showNat' : ℕ → String
showNat' zero = "0"
showNat' (suc zero) = "1"
showNat' (suc (suc zero)) = "2"
showNat' (suc (suc (suc zero))) = "3"
showNat' (suc (suc (suc (suc zero)))) = "4"
showNat' (suc (suc (suc (suc (suc zero))))) = "5"
showNat' (suc (suc (suc (suc (suc (suc zero)))))) = "6"
showNat' (suc (suc (suc (suc (suc (suc (suc zero))))))) = "7"
showNat' (suc (suc (suc (suc (suc (suc (suc (suc zero)))))))) = "8"
showNat' (suc (suc (suc (suc (suc (suc (suc (suc (suc n))))))))) = ">= 8"

showNat : String → ℕ → String
showNat debugString n = showNat' n ++Str " (" ++Str debugString ++Str ")"
