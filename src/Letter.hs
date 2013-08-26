-- |
-- Module      : Association
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Letters. But only the ones we need.

module Letter
       ( Letter
       , LetterPair
       , fromDigit
       , digitsToLetters
       )
       where

import Digit


-- | Each Digit can be represented as one of these Letters in the Dominic
-- memory system.
data Letter = A | B | C | D | E | S | G | H | N | O
  deriving (Show, Eq)

-- | We will need to project DigitPairs as LetterPairs
data LetterPair = LetterPair Letter Letter
  deriving Eq

instance Show LetterPair where
  show (LetterPair c d) = show c ++ show d


-- | Mapping of Digits to Letters.
fromDigit :: Digit -> Letter
fromDigit One   = A
fromDigit Two   = B
fromDigit Three = C
fromDigit Four  = D
fromDigit Five  = E
fromDigit Six   = S
fromDigit Seven = G
fromDigit Eight = H
fromDigit Nine  = N
fromDigit Zero  = O


-- | Helper to convert a DigitPair to a LetterPair
digitsToLetters :: DigitPair -> LetterPair
digitsToLetters (DigitPair n m) = LetterPair (fromDigit n) (fromDigit m)
