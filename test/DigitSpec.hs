module DigitSpec
       where

import Util()

import Test.Hspec
import Test.QuickCheck

import Digit


spec :: Spec
spec = do
  describe "Digits" $ do
    it "can be converted to and from chars" $
      property $ \d -> Just d == (fromChar . head . show) d

    it "can only be produced from the chars 0-9" $
      property $ \c -> case fromChar c of
        Just d  -> c `elem` ['0'..'9']
        Nothing -> True

    it "corresponds to the list of chars 0-9" $
      map Just [Zero .. Nine] `shouldBe` map fromChar ['0'..'9']

  describe "DigitPairs" $ do
    it "can be converted to and from chars" $
      property $ \digits -> case show digits of
        (c:d:[]) -> maybeDigitPair c d == Just digits
        _        -> False

    it "can only be produced from two chars each in 0-9" $
      property $ \(c, d) -> case maybeDigitPair c d of
        Just ds -> all (`elem` ['0'..'9']) $ show ds
        Nothing -> True

