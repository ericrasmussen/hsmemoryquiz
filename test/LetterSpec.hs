module LetterSpec
       where

import Util()

import Test.Hspec
import Test.QuickCheck

import Digit
import Letter


-- Letters start at A (corresponding to One) and end at O (corresponding to
-- Zero). We don't have Letter -> Digit functions to test with because they
-- aren't needed in the application, but this is convenient for the tests.
digits :: [Digit]
digits = [One .. Nine] ++ [Zero]


spec :: Spec
spec = do
  describe "Letters" $ do
    it "can be created from a digit" $ do
      let mapping = zip [A .. O] digits
      property $ \d -> case lookup (fromDigit d) mapping of
        Just digit -> d == digit
        Nothing    -> False

    it "correspond to the list of digits" $
      [A .. O] `shouldBe` map fromDigit digits

  describe "LetterPairs" $
    it "can be created from a DigitPair" $
      property $ \ds -> let (DigitPair  n m) = ds
                            (LetterPair c d) = digitsToLetters ds
                        in fromDigit n == c && fromDigit m == d

