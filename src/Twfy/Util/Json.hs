{-|
Module      : Twfy.Util.Json
Description : JSON Utilities

Convert record field names between haskell/JSON versions
-}
module Twfy.Util.Json
       (
         recordNameToJsonName
       ) where

import Data.Char (isUpper, toLower)

-- | Convert a haskell record field name in camelCase to JSON snake_case.
recordNameToJsonName :: String -- ^ Record name
                     -> String -- ^ JSON name
recordNameToJsonName xs = let suffix = dropWhile (not . isUpper) xs
                              camel = (toLower (head suffix)) : tail suffix
                          in camelToSnake camel

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake xs@(_:[]) = xs
camelToSnake (x:xs) = if isUpper x
                      then '_' : ( (toLower x) : (camelToSnake xs) )
                      else x : (camelToSnake xs)
