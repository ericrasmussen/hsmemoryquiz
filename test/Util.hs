{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Util
       ( Arbitrary
       )
       where

import Digit
import Letter
import Association

import Control.Applicative

import Test.QuickCheck

import qualified Data.Vector as Vector


-- -----------------------------------------------------------------------------
-- * Arbitrary instances for our data types

instance Arbitrary Digit where
  arbitrary = elements [Zero .. Nine]

instance Arbitrary Letter where
  arbitrary = elements [A .. O]

instance Arbitrary DigitPair where
  arbitrary = DigitPair <$> arbitrary <*> arbitrary

instance Arbitrary LetterPair where
  arbitrary = LetterPair <$> arbitrary <*> arbitrary

instance Arbitrary Association where
  arbitrary = makeAssociation <$> arbitrary
                              <*> (arbitrary `suchThat` noNewlines)

instance Arbitrary AssociationDB where
  arbitrary = Vector.fromList <$> listOf1 arbitrary


-- -----------------------------------------------------------------------------
-- * Helper functions to constrain generated data

-- | Helper to ensure generated Association descriptions do not contain newlines
noNewlines :: String -> Bool
noNewlines s = not $ any (`elem` "\r\n") s


