module ParserSpec
       where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec.Prim

import qualified Data.Vector as V

import Util()

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
        case runParse as of
          Left  _      -> False
          Right result -> result == id assocs


  describe "DigitPair parser" $
    it "parses two digit chars as a digit pair" $
      property $ \s -> let firstTwo = take 2 s in
        case parse parseDigitPair "" s of
          Right pair -> show pair == firstTwo
          Left  _    -> case firstTwo of
            [] -> True
            _  -> not $ all (`elem` ['0'..'9']) firstTwo

