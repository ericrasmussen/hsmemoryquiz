module ParserSpec
       where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec.Prim

import Util
import Digit
import Letter
import Parser
import Association


spec :: Spec
spec = do
  describe "test association parser" $
    it "parses an association" $
      property $ \x -> case parse parseAssociation "" (show x) of
        Left  e -> False
        Right r -> r == x

  describe "test digit pair parser" $
    it "parses a valid digit pair or fails" $
      property $ \s -> let firstTwo = take 2 s in
        case parse parseDigitPair "" s of
          Right (DigitPair n m) -> show n ++ show m == firstTwo
          Left  _               -> not $ leadingDigits firstTwo

