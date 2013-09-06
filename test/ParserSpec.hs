module ParserSpec
       where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec.Prim

import qualified Data.Vector as V

import Util

import Digit
import Parser


spec :: Spec
spec = do
  describe "Association parsers" $ do
    it "parses an association" $
      property $ \assoc -> case parse parseAssociation "" (show assoc) of
        Left  _      -> False
        Right result -> result == id assoc

    it "parses a vector of associations" $
      property $ \assocs -> let as = unlines . map show . V.toList $ assocs in
        case parse parseAssociationDB "" as of
          Left  _      -> False
          Right result -> result == assocs


  describe "test digit pair parser" $
    it "parses a valid digit pair or fails" $
      property $ \s -> let firstTwo = take 2 s in
        case parse parseDigitPair "" s of
          Right (DigitPair n m) -> show n ++ show m == firstTwo
          Left  _               -> not $ leadingDigits firstTwo

