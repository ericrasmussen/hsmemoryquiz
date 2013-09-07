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
       , Result
       , Response
       , AnswerChecker
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

-- these are quiz-y
type Result            = Either String String
type Response          = String
type AnswerChecker     = Association -> Response -> Result

-- -----------------------------------------------------------------------------
-- * Functions for working with Associations

-- | Smart constructor for Associations
makeAssociation :: DigitPair -> Mnemonic -> Association
makeAssociation ds m = Association ds trimmed
  where trimmed = dropWhile (==' ') m


-- | Typeclass to render and check answers for Associations projected as other
-- types.
class (Show a) => Projection a where
  view        :: Association -> a
  checkAnswer :: a -> Response -> Result

instance Projection DigitPair where
  view (Association ds _) = ds
  checkAnswer ds          = toResult (==digits) digits where digits = show ds

instance Projection LetterPair where
  view (Association ds _) = digitsToLetters ds
  checkAnswer ls          = toResult ((==letters) . upper) letters
    where letters = show ls

instance Projection Mnemonic where
  view (Association _ m) = m
  -- TODO: use a better test here ("" `isInfixOf` "foo" == True)
  checkAnswer m          = toResult ((`isInfixOf` upper m) . upper) m


-- | Checks a response against a predicate and converts it to a Result
toResult :: (Response -> Bool) -> String -> Response -> Result
toResult p a s = if p s
                 then Right "Correct!"
                 else Left $ "We were looking for: " ++ a

-- -----------------------------------------------------------------------------
-- * Unexported helper functions

-- | Helper function to uppercase a String
upper :: String -> String
upper = map toUpper


