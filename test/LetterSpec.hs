module LetterSpec
       where

import Test.Hspec
import Test.QuickCheck

import Util
import Digit
import Letter


spec :: Spec
spec = do
  describe "Letters" $
    it "can be created from a digit" $ do
      let mapping = zip [A .. O] ([One .. Nine] ++ [Zero])
      property $ \d -> case lookup (fromDigit d) mapping of
        Just digit -> d == digit
        Nothing    -> False


