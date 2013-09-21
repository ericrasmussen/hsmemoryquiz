module HelpersSpec
       where

import Util()
import Helpers
import Test.Hspec
import Test.QuickCheck
import Data.List (isInfixOf)


spec :: Spec
spec = do
  describe "formatFraction" $
    it "formats integers x and y as a string 'x/y'" $
      property $ \x y -> formatFraction x y == show x ++ "/" ++ show y

  describe "formatPercentage" $ do
    it "always ends in a % symbol" $
      property $ \x y -> last (formatPercentage x y) == '%'

    it "takes two integers and formats them as a percentage" $
      property $ \x y -> case formatPercentage x y of
        "0%"   -> y == 0 || x == 0
        "100%" -> x == y
        p      -> let f  = read (init p) :: Float
                      f' = 100 * (fromIntegral x / fromIntegral y)
                  in  (f - f') < 0.1

  describe "upper" $
    it "uppercases a string once (idempotence)" $
      property $ \x -> (upper . upper) x == upper x

  describe "lower" $
    it "lowercases a string once (idempotence)" $
      property $ \x -> (lower . lower) x == lower x

  describe "invalidCommand" $
    it "formats an error message using an arbitrary string" $
      property $ \s -> ("\"" ++ s ++ "\"") `isInfixOf` invalidCommand s
