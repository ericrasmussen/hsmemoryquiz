module Util
       ( Arbitrary
       , leadingDigits
       )
       where

import Digit
import Letter
import Parser
import Association

import Control.Applicative

import Test.QuickCheck

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


-- -----------------------------------------------------------------------------
-- * Helper functions to constrain generated data

-- | Helper to ensure generated Association descriptions do not contain newlines
noNewlines :: String -> Bool
noNewlines s = not hasNewline
  where hasNewline = elem '\n' s || elem '\r' s

-- | Helper to ensure the first two characters in a string are in '0'..'9'
leadingDigits :: String -> Bool
leadingDigits (s:s':_) = s `elem` ['0'..'9'] && s' `elem` ['0'..'9']
leadingDigits _        = False

