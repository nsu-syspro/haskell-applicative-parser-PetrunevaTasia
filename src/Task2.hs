{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--

-- date ::= dotFormat | hyphenFormat | usFormat
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]

-- dotFormat ::= day "." month "." year
dotFormat :: Parser Date
dotFormat = do
  dayInt <- day
  _ <- char '.'
  monthInt <- month
  _ <- char '.'
  Date dayInt monthInt <$> year

-- hyphenFormat ::= day "-" month "-" year
hyphenFormat :: Parser Date
hyphenFormat = do
  dayInt <- day
  _ <- char '-'
  monthInt <- month
  _ <- char '-'
  Date dayInt monthInt <$> year

-- usFormat ::= monthName " " usDay " " year
usFormat :: Parser Date
usFormat = do
  monthNameInt <- monthName
  _ <- char ' '
  usDayInt <- usDay
  _ <- char ' '
  Date usDayInt monthNameInt <$> year

-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
usDay :: Parser Day
usDay = fmap Day (choice (fromStringToInt <$> fmap string ["30", "31"] ++ [oneAndDigit, twoAndDigit, fromCharToString nonZeroDigit]))

-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
day :: Parser Day
day = fmap Day (choice (fromStringToInt <$> fmap string ["30", "31"] ++ [oneAndDigit, twoAndDigit, zeroAndNonZeroDigit]))

-- oneAndDigit ::= "1" digit
oneAndDigit :: Parser String
oneAndDigit = do
  first <- string "1"
  second <- fromCharToString digit
  pure (first ++ second)

-- twoAndDigit ::= "2" digit 
twoAndDigit :: Parser String
twoAndDigit = do
  first <- string "2"
  second <- fromCharToString digit
  pure (first ++ second)

-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
month :: Parser Month
month = fmap Month (choice (fromStringToInt <$> zeroAndNonZeroDigit : fmap string ["10", "11","12"]))

-- zeroAndNonZeroDigit ::= "0" nonZeroDigit
zeroAndNonZeroDigit :: Parser String
zeroAndNonZeroDigit = do
  first <- string "0"
  second <- fromCharToString nonZeroDigit
  pure (first ++ second)

-- year ::= number
year :: Parser Year
year = fmap Year number

fromStringToInt :: Parser String -> Parser Int
fromStringToInt ps = read <$> ps

fromCharToString :: Parser Char -> Parser String
fromCharToString ps = (:[]) <$> ps

-- number ::= digit | number digit
number :: Parser Int
number = do
  digits <- many1 digit
  pure (read digits)

-- digit ::= "0" | nonZeroDigit
digit :: Parser Char
digit = choice [char '0', nonZeroDigit]

-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
nonZeroDigit :: Parser Char
nonZeroDigit = choice (fmap char ['1'..'9'])

-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
monthName :: Parser Month
monthName = fmap changeMonthNameOnInt (choice (fmap string ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]))

changeMonthNameOnInt :: String -> Month
changeMonthNameOnInt str = Month $ case str of
  "Jan" -> 1; "Feb" -> 2; "Mar" -> 3; "Apr" -> 4; "May" -> 5; "Jun" -> 6
  "Jul" -> 7; "Aug" -> 8; "Sep" -> 9; "Oct" -> 10 ; "Nov" -> 11; "Dec" -> 12
  _ -> error "Month not found"