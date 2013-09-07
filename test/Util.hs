{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Util
       ( Arbitrary
       )
       where

import Quiz
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

instance Arbitrary QuizState where
  arbitrary = QuizState <$> arbitrary <*> arbitrary

instance Arbitrary Question where
  arbitrary = do
    q <- arbitrary :: Gen String
    r <- arbitrary :: Gen (Either String String)
    let eval = \s -> r
    return $ Question { question = q, evaluator = eval }

instance Arbitrary Registry where
  arbitrary = do
    q   <- arbitrary :: Gen Question
    as  <- arbitrary :: Gen AssociationDB
    ind <- arbitrary :: Gen (Quiz Int)
    let gen = \a -> q
    return $ makeRegistry gen as ind

instance Arbitrary (Quiz Int) where
  arbitrary = elements [indexRand, indexOrdered, indexReversed]

-- required for using Quiz Int as a property
instance Show (Quiz Int) where
  show _ = "<quiz int>"

-- -----------------------------------------------------------------------------
-- * Helper functions to constrain generated data

-- | Helper to ensure generated Association descriptions do not contain newlines
noNewlines :: String -> Bool
noNewlines s = not $ any (`elem` "\r\n") s


