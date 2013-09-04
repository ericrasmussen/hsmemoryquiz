import Test.Hspec
import Test.QuickCheck

import Text.Parsec.Prim

import Digit
import Letter
import Parser
import Association

import Control.Applicative

import Data.Char   (toUpper)
import Data.List   (isInfixOf)


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
-- * Constraints on generated data

-- | Helper to ensure generated Association descriptions do not contain newlines
noNewlines :: String -> Bool
noNewlines s = not hasNewline
  where hasNewline = elem '\n' s || elem '\r' s

-- | Helper to ensure the first two characters in a string are in '0'..'9'
leadingDigits :: String -> Bool
leadingDigits (s:s':_) = s `elem` ['0'..'9'] && s' `elem` ['0'..'9']
leadingDigits _        = False


main = hspec $ do

  describe "test association parser" $ do
    it "parses an association" $
      property $ \x -> case (parse parseAssociation "" (show x)) of
        Left  e -> False
        Right r -> r == x

  describe "test digit pair parser" $ do
    it "parses a valid digit pair or fails" $ do
      property $ \s -> let firstTwo = take 2 s in
        case (parse parseDigitPair "" s) of
          Right (DigitPair n m) -> show n ++ show m == firstTwo
          Left  _               -> not $ leadingDigits firstTwo

  describe "Association smart constructor" $ do
    it "trims mnemonic whitespace" $ do
      property $ \ds m -> mnemonic (makeAssociation ds m) == dropWhile (==' ') m

  describe "Association projections" $ do
    it "can project a DigitPair" $ do
      property $ \a -> (view a :: DigitPair) == digits a

    it "can check DigitPair answers" $ do
      property $ \a s -> case checkAnswer (view a :: DigitPair) s of
        Left  err -> show (digits a) `isInfixOf` err
        Right r   -> show (digits a) == s

    it "can verify the digits projection matches the original" $ do
      property $ \a -> let ds = show (digits a) in
        case checkAnswer (view a :: DigitPair) ds of
          Left  _ -> False
          Right _ -> True

    it "can project a LetterPair" $ do
      property $ \a -> (view a :: LetterPair) == (digitsToLetters . digits) a

    it "can check LetterPair answers" $ do
      let toLetters as = map toUpper $ show . digitsToLetters . digits $ as
      property $ \a s -> case checkAnswer (view a :: LetterPair) s of
        Left  err -> toLetters a `isInfixOf` err
        Right _   -> toLetters a == s

    it "can verify the letters projection matches the original" $ do
      property $ \a -> let ls = show . digitsToLetters $ digits a in
        case checkAnswer (view a :: LetterPair) ls of
          Left  _ -> False
          Right _ -> True

    it "can project a Mnemonic" $ do
      property $ \a -> (view a :: Mnemonic) == mnemonic a

    it "can check mnemonic answers" $ do
      let upper = map toUpper
      property $ \a s -> case checkAnswer (view a :: Mnemonic) s of
        Left  err -> mnemonic a `isInfixOf` err
        Right _   -> upper s `isInfixOf` upper (mnemonic a)

    it "can verify the mnemonic projection matches the original" $ do
      property $ \a -> case checkAnswer (view a :: Mnemonic) (mnemonic a) of
        Left  _ -> False
        Right _ -> True


  describe "Letters" $ do
    it "can be created from a digit" $ do
      let mapping = zip [A .. O] ([One .. Nine] ++ [Zero])
      property $ \d -> case lookup (fromDigit d) mapping of
        Just digit -> d == digit
        Nothing    -> False


