{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}

module JSONParser where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Functor
import Data.List
import GHC.Generics
import Numeric 
import Test.QuickCheck hiding (Positive, Negative)

data JValue = JNull 
    | JBool Bool 
    | JString String
    | JNumber { int :: Integer, frac :: [Int], exponent :: Integer}
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Eq, Generic)

instance Show JValue where 
    show value = case value of
        JNull        -> "null"
        JBool True   -> "true"
        JBool False  -> "false"
        JString s    -> showJSONString s
        JNumber s [] 0 -> show s
        JNumber s [] e -> show s ++ "e" ++ show e
        JNumber s f 0 -> show s ++ "." ++ concatMap show f 
        JNumber s f e -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
        JArray a      -> "[" ++ intercalate ", " (map show a) ++ "]"
        JObject o     -> "{" ++ intercalate "," (map showKV o) ++ "}"
        where 
            showKV (k,v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

--  haskell is control is different from json control values
isCharControl :: Char -> Bool
isCharControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of 
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/'  -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isCharControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a


jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary 

jNumberGen :: Gen JValue 
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0,9)) <*> arbitrary

jsonStringGen :: Gen String
jsonStringGen = 
    concat <$> listOf (oneof [ vectorOf 1 arbitraryUnicodeChar
                             , escapedUnicodeChar ])
    where
        escapedUnicodeChar = ("\\u" ++ ) <$> vectorOf 4 (elements hexDigitLetters)
        hexDigitLetters = ['0'.. '9'] ++ ['a'.. 'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen
