import Test.Hspec
import Test.QuickCheck

import Text.Parsec.Prim

import Digit
import Parser
import Association

import Control.Applicative



instance Arbitrary Digit where
  arbitrary = elements [Zero .. Nine]

instance Arbitrary DigitPair where
  arbitrary = DigitPair <$> arbitrary <*> arbitrary

instance Arbitrary Association where
  arbitrary = makeAssociation <$> arbitrary
                              <*> (arbitrary `suchThat` noNewlines)


-- helper to ensure generated Association descriptions do not contain newlines
noNewlines :: String -> Bool
noNewlines s = not hasNewline
  where hasNewline = elem '\n' s || elem '\r' s

-- helper to ensure the first two characters in a string are in '0'..'9'
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
