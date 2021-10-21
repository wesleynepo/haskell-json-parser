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
import Test.QuickCheck.Arbitrary (Arbitrary)
import Data.Text.Internal.Builder.Int.Digits (digits)

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

jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
    where 
        objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n =  if n < 5
    then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
    else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
    where
        scalarGens      = [jNullGen, jBoolGen, jNumberGen, jStringGen]        
        compositeGens n = [jArrayGen n, jObjectGen n]

instance Arbitrary JValue where
    arbitrary = sized jValueGen
    shrink = genericShrink 

jsonWhitespaceGen :: Gen String
jsonWhitespaceGen = 
    scale (round . sqrt . fromIntegral)
    . listOf
    . elements
    $ [ ' ', '\n', '\r', '\t' ]

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements ->
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _           -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

newtype Parser i o =
  Parser { runParser :: i -> Maybe (i, o)}

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case 
  (x:xs) | predicate x -> Just (xs, x)
  _                    -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

instance Functor (Parser i) where
  fmap f parser = Parser $ fmap (fmap f) . runParser parser

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

instance Applicative (Parser i) where
  pure x = Parser $ pure . (,x)
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing -> Nothing 
    Just (rest, f) -> fmap f <$> runParser po rest

string :: String -> Parser String String 
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

jNull :: Parser String JValue
jNull = string "null" $> JNull 

instance Alternative (Parser i) where
  empty = Parser $ const empty 
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

jBool :: Parser String JValue 
jBool = string "true" $> JBool True
  <|> string "false" $> JBool False

jsonChar :: Parser String Char
jsonChar = string "\\\"" $> '"'
  <|> string "\\\\" $> '\\'
  <|> string "\\/" $> '/'
  <|> string "\\b" $> '\b'
  <|> string "\\f" $> '\f'
  <|> string "\\n" $> '\n'
  <|> string "\\r" $> '\r'
  <|> string "\\t" $> '\t'
  <|> unicodeChar 
  <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c ))
  where 
    unicodeChar = 
      chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> replicateM 4 hexDigit)
    
    hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base = 
  foldl (\num d -> num * fromIntegral base + fromIntegral d)

instance Monad (Parser i) where
  p >>= f = Parser $ \input -> case runParser p input of 
    Nothing -> Nothing
    Just (rest, o) -> runParser (f o) rest

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString' )
  where 
    jString' = do
      optFirst <- optional jsonChar
      case optFirst of 
        Nothing -> "" <$ char '"'
        Just first | not (isSurrogate first) ->
          (first:) <$> jString'
        Just first -> do
          second <- jsonChar
          if isHighSurrogate first && isLowSurrogate second
          then (combineSurrogates first second :) <$> jString'
          else empty

highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound  = 0xDC00
lowSurrogateUpperBound  = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a  =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a     = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
  ((ord a - highSurrogateLowerBound) `shiftL` 10)
  + (ord b - lowSurrogateLowerBound) + 0x10000

prop_genParseJString :: Property 
prop_genParseJString =
  forAllShrink jStringGen shrink $ \js ->
    case runParser jString (show js) of
      Nothing -> False
      Just (_, o) -> o == js

jUInt :: Parser String Integer
jUInt = (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digitas
  <|> fromIntegral <$> digit

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

jFrac :: Parser String [Int]
jFrac = char '.' *> digitas

digitas :: Parser String [Int]
digitas = some digit

jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt 

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i 
signInt _          i = i 

jExp :: Parser String Integer
jExp = (char  'e' <|> char 'E')
  *> (signInt <$> optional (char '-' <|> char '-') <*> jUInt)

jInt :: Parser String JValue
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = (\i f -> JNumber i f 0) <$> jInt' <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp = (\ ~(JNumber i f _) e -> JNumber i f e) <$> jIntFrac <*> jExp

jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

prop_genParseJNumber :: Property 
prop_genParseJNumber = 
  forAllShrink jNumberGen shrink $ \jn ->
    case runParser jNumber (show jn) of
      Nothing ->False
      Just (_, o) -> o == jn 