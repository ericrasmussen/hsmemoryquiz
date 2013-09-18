module AssociationSpec
       where

import Test.Hspec
import Test.QuickCheck

import Util()
import Digit
import Letter
import Association

import Data.Char   (toUpper)
import Data.List   (isInfixOf)


spec :: Spec
spec = do
  describe "Association smart constructor" $
    it "trims mnemonic whitespace" $
      property $ \ds m -> mnemonic (makeAssociation ds m) == dropWhile (==' ') m

  describe "Association projections" $ do
    it "can project a DigitPair" $
      property $ \a -> (view a :: DigitPair) == digits a

    it "can check DigitPair answers" $
      property $ \a s -> case checkAnswer (view a :: DigitPair) s of
        Left  err -> show (digits a) `isInfixOf` err
        Right r   -> show (digits a) == s

    it "can verify the digits projection matches the original" $
      property $ \a -> let ds = show (digits a) in
        case checkAnswer (view a :: DigitPair) ds of
          Left  _ -> False
          Right _ -> True

    it "can project a LetterPair" $
      property $ \a -> (view a :: LetterPair) == (digitsToLetters . digits) a

    it "can check LetterPair answers" $ do
      let toLetters as = map toUpper $ show . digitsToLetters . digits $ as
      property $ \a s -> case checkAnswer (view a :: LetterPair) s of
        Left  err -> toLetters a `isInfixOf` err
        Right _   -> toLetters a == s

    it "can verify the letters projection matches the original" $
      property $ \a -> let ls = show . digitsToLetters $ digits a in
        case checkAnswer (view a :: LetterPair) ls of
          Left  _ -> False
          Right _ -> True

    it "can project a Mnemonic" $
      property $ \a -> (view a :: Mnemonic) == mnemonic a

    it "can check mnemonic answers" $ do
      let upper = map toUpper
      property $ \a s -> case checkAnswer (view a :: Mnemonic) s of
        Left  err -> mnemonic a `isInfixOf` err
        Right _   -> upper s `isInfixOf` upper (mnemonic a)

    it "can verify the mnemonic projection matches the original" $
      property $ \a -> case checkAnswer (view a :: Mnemonic) (mnemonic a) of
        -- only strings <= length 2 should ever fail
        Left  _ -> length (mnemonic a) <= 2
        Right _ -> True

