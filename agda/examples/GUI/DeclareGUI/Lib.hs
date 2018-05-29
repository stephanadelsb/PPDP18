
module DeclareGUI.Lib where

import GHC.Word as W
import Data.Time.Clock.POSIX

-- backspace is only character in string
-- backspace is first character in string
-- multiple backspace characters are string in succession

removeCharactersBeforeBackspace :: String -> String
removeCharactersBeforeBackspace (a : b : c : d : e : '\b' : '\b' : '\b' : '\b' : '\b' : xs) = removeCharactersBeforeBackspace xs
removeCharactersBeforeBackspace (a : b : c : d : '\b' : '\b' : '\b' : '\b' : xs) = removeCharactersBeforeBackspace xs
removeCharactersBeforeBackspace (a : b : c : '\b' : '\b' : '\b' : xs) = removeCharactersBeforeBackspace xs
removeCharactersBeforeBackspace (a : b : '\b' : '\b' : xs) = removeCharactersBeforeBackspace xs
removeCharactersBeforeBackspace (x : '\b' : xs) = removeCharactersBeforeBackspace xs
removeCharactersBeforeBackspace (x : xs) = x : removeCharactersBeforeBackspace xs
removeCharactersBeforeBackspace [] = []


test =
  ( (removeCharactersBeforeBackspace "abcd\b") == "abc") &&
  ( (removeCharactersBeforeBackspace "abcd\b\b") == "ab") &&
  ( (removeCharactersBeforeBackspace "abcd\b\b\b") == "a") 


safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

lookupList :: Int -> [a] -> Maybe a
lookupList _ []       = Nothing
lookupList 0 (x : _)  = Just x
lookupList i (_ : xs) = lookupList (i - 1) xs


puts :: Show a => a -> IO ()
puts x = putStrLn $ show x


microsecondsNow :: IO Integer
microsecondsNow = (round . (* 1000000)) <$> getPOSIXTime

inBetween :: Int -> Int -> Int -> Bool
inBetween x a b
  = (x >= a) && (x <= b)

toWord :: Int -> W.Word8
toWord x = fromIntegral x --(round x)
