{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import Data.Char (toLower, digitToInt)
import Data.List (intercalate)
import ParserCombinators
import Task2(fromCharToString)
import Prelude hiding (exponent)
import GHC.Base (ord)
-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--

-- json ::= element
json :: Parser JValue
json = element

-- value ::= object | array | string' | number | true | false | null'
value :: Parser JValue
value = choice [object, array, string', number, true, false, null']

-- true ::= "true"
true :: Parser JValue
true = do
  _ <- string "true"
  pure (JBool True)

-- false ::= "false"
false :: Parser JValue
false = do
  _ <- string "false"
  pure (JBool False)

-- null' ::= "null"
null' :: Parser JValue
null' = do
  _ <- string "null"
  pure JNull

-- object ::= '{' ws '}' | '{' members '}'
object :: Parser JValue
object = choice [membersObject, wsObject]

-- wsObject ::= '{' ws '}'
wsObject :: Parser JValue
wsObject = do
  _ <- char '{'
  _ <- ws
  _ <- char '}'
  pure (JObject [])

-- membersObject ::= '{' members '}'
membersObject :: Parser JValue
membersObject = do
  _ <- char '{'
  membersVal <- members
  _ <- char '}'
  pure (JObject membersVal)

-- members ::= member | member ',' members
members :: Parser [(String, JValue)]
members = sepBy member (char ',')

-- member ::= ws stringVal ws ':' element
member :: Parser (String, JValue)
member = do
  _ <- ws
  str <- stringVal
  _ <- ws
  _ <- char ':'
  elem' <- element
  pure (str, elem')

-- array = '[' elements ']' | '[' ws ']' 
array :: Parser  JValue
array = choice [elementsArray, wsArray]

-- elementsArray ::= '[' elements ']'
elementsArray :: Parser JValue
elementsArray = do
  _ <- char '['
  elemsVal <- elements
  _ <- char ']'
  pure (JArray elemsVal)

-- wsArray ::= '[' ws ']'
wsArray :: Parser JValue
wsArray = do
  _ <- char '['
  _ <- ws
  _ <- char ']'
  pure (JArray [])

-- elements ::= element | element ',' elements
elements :: Parser [JValue]
elements = sepBy element (char ',')

-- element ::= ws value ws
element :: Parser JValue
element = do
  _ <- ws
  val <- value
  _ <- ws
  pure val

-- string' ::= stringVal
string' :: Parser JValue
string' = fmap JString stringVal

-- stringVal ::= '"' characters '"'
stringVal :: Parser String
stringVal = do
  _ <- char '\"'
  charactersStr <- characters
  _ <- char '\"'
  pure charactersStr

-- characters ::= "" | character characters
characters :: Parser String
characters = choice [myMany character, string ""]

-- character ::= '0020'..'10FFFF' - '"' - '\' | '\' escape
character :: Parser String
character = choice [fromCharToString normalChar, slashAndEscape]

-- normalChar ::= '0020'..'10FFFF' - '"' - '\' 
normalChar :: Parser Char
normalChar = satisfy (\c -> ord c >= 0x0020 && ord c <= 0x10FFFF && c /= '\"' && c /= '\\')

-- slashAndEscape ::= '\' escape
slashAndEscape :: Parser String
slashAndEscape = do
  slash <- fromCharToString(char '\\')
  esc <- fromCharToString escape
  pure (slash ++ esc)

-- escape ::= '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' | 'u' hex hex hex hex
escape :: Parser Char
escape = choice (unicode : fmap char ['"', '\\', '/', 'b', 'f' , 'n', 'r', 't'])

-- unicode ::= 'u' hex hex hex hex
unicode :: Parser Char
unicode = do
  _ <- char 'u'
  hex1 <- hex
  hex2 <- hex
  hex3 <- hex
  hex4 <- hex
  pure $ toEnum $ (digitToInt hex1 * 4096) + (digitToInt hex2 * 256) + (digitToInt hex3 * 16) + digitToInt hex4

-- hex ::= digit | 'A'..'F' | 'a'..'f'
hex :: Parser Char
hex = choice ([digit] ++ fmap char ['A'..'F'] ++ fmap char ['a'..'f'])

-- number ::= integer fraction exponent
number :: Parser JValue
number = do
  integerStr <- integer
  fractionStr <- fraction
  exponentStr <- exponent
  case reads (integerStr ++ fractionStr ++ exponentStr) of
    [(num, "")] -> pure (JNumber num)
    _ -> error "Invalid number"

-- integer ::= '-' onenine digits | '-' digit | onenine digits | digit 
integer :: Parser String
integer = choice [minusOnenine, minusDigit, onenineDigits, fromCharToString digit]

-- onenineDigits ::= onenine digits
onenineDigits :: Parser String
onenineDigits = do
  onenineChar <- fromCharToString onenine
  digitsStr <- digits
  pure (onenineChar ++ digitsStr)

-- minusDigit ::= '-' digit
minusDigit :: Parser String
minusDigit = do
  minusChar <- char '-'
  digitChar <- digit
  pure [minusChar, digitChar]

-- minusOnenine ::= '-' onenine digits
minusOnenine :: Parser String
minusOnenine = do
  minusChar <- fromCharToString (char '-')
  onenineChar <- fromCharToString onenine
  digitsStr <- digits
  pure (minusChar ++ onenineChar ++ digitsStr)

-- digits ::= digit | digit digits
digits :: Parser String
digits = do
  many1 digit

-- digit ::= '0' | onenine
digit :: Parser Char
digit = choice [char '0', onenine]

-- onenine ::= '1'..'9'
onenine :: Parser Char
onenine = choice (char <$> ['1'..'9'])

-- fraction ::= '.' digits | ""
fraction :: Parser String
fraction = choice [dotDigits, string ""]

-- dotDigits ::= '.' digits
dotDigits :: Parser String
dotDigits = do
  dotChar <- fromCharToString (char '.')
  digitsStr <- digits
  pure (dotChar ++ digitsStr)

-- exponent ::= 'E' sign digits | 'e' sign digits | ""
exponent :: Parser String
exponent = choice [upperE, lowE, string ""]

-- upperE ::= 'E' sign digits
upperE :: Parser String
upperE = do
  charE <- fromCharToString (char 'E')
  signVal <- sign
  digitsStr <- digits
  pure (charE ++ signVal ++ digitsStr)

-- lowE ::= 'e' sign digits
lowE :: Parser String
lowE = do
  charE <- fromCharToString (char 'e')
  signVal <- sign
  digitsStr <- digits
  pure (charE ++ signVal ++ digitsStr)

-- sign ::= '+' | '-' | ""
sign :: Parser String
sign = choice ((fromCharToString <$> fmap char ['+', '-']) ++ [string ""])

-- ws ::= '0020' ws | '000A' ws | '000D' ws | '0009' ws | ""
ws :: Parser String
ws = choice [many1 (choice [char ' ', char '\n', char '\r', char '\t']), string ""]

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
