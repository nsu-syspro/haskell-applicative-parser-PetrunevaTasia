{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse p str = runParser p (Position 0 str)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p str = case parse p str of
  Parsed x _ -> Just x
  Failed _ -> Nothing

instance Functor Parser where
  fmap f pa = Parser $ \input->
    let resPA = runParser pa input
    in case resPA of
      Parsed resA inputA -> Parsed (f resA) inputA
      Failed list        -> Failed list

instance Applicative Parser where
  pure x = Parser (Parsed x)
  fa <*> a = Parser $ \input ->
    let resFA = runParser fa input
    in case resFA of
      Parsed func inputFA -> runParser (fmap func a) inputFA
      Failed list         -> Failed list

instance Alternative Parser where
  empty = Parser $ \_ -> Failed []
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Failed errsL ->
        case r input of
          Failed errsR    -> Failed $ nub $ errsL ++ errsR
          Parsed y inputR -> Parsed y inputR
      Parsed x inputL -> Parsed x inputL
      

instance Monad Parser where
  pa >>= f = Parser $ \input ->
    case runParser pa input of
      Parsed a input' -> runParser (f a) input'
      Failed errs     -> Failed errs

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \(Position pos str) -> case str of
    [] -> Failed [Position pos EndOfInput]
    (hd : tl)
      | predicate hd -> Parsed hd (Position (pos + 1) tl)
      | otherwise    -> Failed [Position pos (Unexpected hd)]