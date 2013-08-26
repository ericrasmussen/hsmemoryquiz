-- |
-- Module      : Digit
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Modeling the digits 0-9 as unique types.

module Digit
       ( Digit (..)
       , DigitPair (..)
       , fromChar
       , maybeDigitPair
       )
       where

import Control.Applicative ((<$>), (<*>))


-- -----------------------------------------------------------------------------
-- * Digits as data

-- | Model the digits we care about as a sum type
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Enum)

instance Show Digit where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"

-- | Helper to map char representations of digits to Just Digits
fromChar :: Char -> Maybe Digit
fromChar '0' = Just Zero
fromChar '1' = Just One
fromChar '2' = Just Two
fromChar '3' = Just Three
fromChar '4' = Just Four
fromChar '5' = Just Five
fromChar '6' = Just Six
fromChar '7' = Just Seven
fromChar '8' = Just Eight
fromChar '9' = Just Nine
fromChar _   = Nothing

-- | Represent pairs of digits we'll use in the Dominic system
data DigitPair = DigitPair Digit Digit
  deriving Eq

instance Show DigitPair where
  show (DigitPair n m) = show n ++ show m

-- | Helper to map digit chars to a Just DigitPair
maybeDigitPair :: Char -> Char -> Maybe DigitPair
maybeDigitPair c d = DigitPair <$> fromChar c <*> fromChar d

