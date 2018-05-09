
module NativeString where

open import Data.String.Base
open import Data.Maybe.Base
open import Data.Nat.Base

postulate
  readMaybeℕ : String → Maybe ℕ


{-# FOREIGN GHC import qualified Text.Read #-}
{-# COMPILE GHC readMaybeℕ = (\s -> Text.Read.readMaybe (Data.Text.unpack s)) #-}
