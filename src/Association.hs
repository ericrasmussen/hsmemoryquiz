{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Module      : Association
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Contains our memory Association type and related functions.

module Association
       ( AssociationDB
       , Association (..)
       , Mnemonic
       , RenderAssociation
       , Projection
       , view
       , checkAnswer
       , makeAssociation
       )
       where

import Digit
import Letter
import Data.Char   (toUpper)
import Data.List   (isInfixOf)
import Data.Vector (Vector)


-- -----------------------------------------------------------------------------
-- * Data structures

-- | Associates a pair of digits with a mnemonic in the Dominic system
data Association = Association {
    digits   :: DigitPair
  , mnemonic :: Mnemonic
  } deriving Eq

-- | Render the association in the form <digits>: <mnemonic>
instance Show Association where
  show (Association ds m) = concat [show ds, ": ", m]


-- -----------------------------------------------------------------------------
-- * Type aliases

type Mnemonic          = String
type AssociationDB     = Vector Association
type RenderAssociation = Association -> String


-- -----------------------------------------------------------------------------
-- * Functions for viewing/projecting Associations

-- | Typeclass to render and check answers for Associations projected as other
-- types.
class (Show a) => Projection a where
  view        :: Association -> a
  checkAnswer :: a -> String -> Bool

instance Projection DigitPair where
  view (Association ds _) = ds
  checkAnswer ds          = \s -> show ds == s

instance Projection LetterPair where
  view (Association ds _) = digitsToLetters ds
  checkAnswer ls          = \s -> show ls == upper s

instance Projection Mnemonic where
  view (Association _ m) = m
  checkAnswer m          = \s -> upper s `isInfixOf` upper m


-- | Smart constructor for Associations
makeAssociation :: DigitPair -> Mnemonic -> Association
makeAssociation ds m = Association ds trimmed
  where trimmed = dropWhile (==' ') m


-- -----------------------------------------------------------------------------
-- * Unexported helper functions

-- | Helper function to uppercase a String
upper :: String -> String
upper = map toUpper


