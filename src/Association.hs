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
import Helpers
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

-- aliases relating to Associations
type Mnemonic          = String
type AssociationDB     = Vector Association
type RenderAssociation = Association -> String

-- aliases relating to checking answers
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
  checkAnswer m          = toResult (closeEnough m) m


-- | Checks a response against a predicate and converts it to a Result
toResult :: (Response -> Bool) -> String -> Response -> Result
toResult p a s = if p s
                 then Right "Correct!"
                 else Left $ "We were looking for: " ++ a


-- | Check if a supplied answer is "close enough" to the real answer. In this
-- case, that means the user's input is at least three characters and is an
-- infix of the real answer
closeEnough :: String -> String -> Bool
closeEnough a s | length s > 2 = upper s `isInfixOf` upper a
                | otherwise    = False
