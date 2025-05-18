{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module ParserCombinators where

import Parser

import Control.Applicative

-- | Parses single character
--
-- Usage example:
--
-- >>> parse (char 'b') "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (char 'b') "abc"
-- Failed [Position 0 (Unexpected 'a')]
--
char :: Char -> Parser Char
char c =  satisfy (== c)

-- | Parses given string
--
-- Usage example:
--
-- >>> parse (string "ba") "bar"
-- Parsed "ba" (Position 2 "r")
-- >>> parse (string "ba") "abc"
-- Failed [Position 0 (Unexpected 'a')]
--
string :: String -> Parser String
string = traverse char

-- | Skips zero or more space characters
--
-- Usage example:
--
-- >>> parse spaces "  bar"
-- Parsed () (Position 2 "bar")
-- >>> parse spaces "bar"
-- Parsed () (Position 0 "bar")
-- >>> parse (spaces *> string "bar") "bar"
-- Parsed "bar" (Position 3 "")
--
spaces :: Parser ()
spaces = () <$ many (char ' ')

-- | Tries to consecutively apply each of given list of parsers until one succeeds.
-- Returns the *first* succeeding parser as result or 'empty' if all of them failed.
--
-- Usage example:
--
-- >>> parse (choice [char 'a', char 'b']) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (choice [char 'a', char 'b']) "foo"
-- Failed [Position 0 (Unexpected 'f')]
-- >>> parse (choice [string "ba", string "bar"]) "bar"
-- Parsed "ba" (Position 2 "r")
--
choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = asum

-- Discover and implement more useful parser combinators below
--
-- - <https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html>
-- - <https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Char.html>


some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure [])

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan
    where scan = liftA2 (:) p ((s *> scan) <|> pure [])

myMany :: Parser String -> Parser String
myMany = many' ""

many' :: String -> Parser String -> Parser String
many' acc p =
  do
     result <- p
     many' (acc ++ result) p
    <|> return acc