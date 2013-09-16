-- |
-- Module      : Parser
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parse all the things

module Parser
       ( parseDigitPair
       , parseAssociation
       , parseAssociationDB
       , parse
       , runParse
       )
       where


import Digit
import Association

import Control.Monad (void)

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import qualified Data.Vector as Vector

import Control.Applicative hiding ((<|>), many, optional)


-- -----------------------------------------------------------------------------
-- * Our very handy char parsers

-- | Consumes the first two characters from the input and tries to convert them
-- to a DigitPair.
parseDigitPair :: GenParser Char state DigitPair
parseDigitPair = do
  left  <- anyChar
  right <- anyChar
  case maybeDigitPair left right of
    Just ds -> return ds
    _       -> parserFail $ left : right : " is not a valid digit pair"


-- | Attempts to parse an association with two digits and a description
-- delimited by a colon
parseAssociation :: GenParser Char state Association
parseAssociation = do
  nums <- spaces *> parseDigitPair
  char ':'
  desc <- manyTill anyChar endOfInput
  return $ makeAssociation nums desc

-- | Builds a Vector of Associations
parseAssociationDB :: GenParser Char state AssociationDB
parseAssociationDB = Vector.fromList <$> many1 parseAssociation

-- | Attempt to parse a vector of Associations from a file
runParse :: String -> Either ParseError AssociationDB
runParse = parse parseAssociationDB "(file)"

-- -----------------------------------------------------------------------------
-- * Helper parsers (not exported)

-- | Match lines ending in "\n", "\r", or "\r\n"
eol :: Monad m => ParsecT String u m Char
eol =   char '\n'
    <|> char '\r' *> option '\n' (char '\n')

-- | Void parser to detect the end of input or a line ending. This makes
-- trailing newlines optional in the associations text file.
endOfInput :: Monad m => ParsecT String u m ()
endOfInput = eof <|> void eol
