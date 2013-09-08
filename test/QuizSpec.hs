module QuizSpec
       where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Quiz
import Util()

import Data.Ix (inRange)

import qualified Data.Vector as V

{-

exported functions we still need to test:

playGame

-}

spec :: Spec
spec = do
  describe "QuizState" $
    it "can be created anew with newQuizState" $ do
      score newQuizState `shouldBe` 0
      total newQuizState `shouldBe` 0

  describe "Registry" $
    it "can be created with makeRegistry" $
      property $ \ (assoc, q, db, ind) ->
        let reg = makeRegistry (\ a -> q) db ind
            assocsEq   = associations reg == db
            questionEq = question (genQuestion reg $ assoc) == question q
            indexEq    = show ind == show (getIndex reg) -- currently useless
        in assocsEq && questionEq && indexEq

  describe "Indexing strategies" $
    it "always produces an Int within the Vector's bounds" $
      monadicIO $ do
        reg <- pick arbitrary
        qs  <- pick arbitrary
        res <- run $ runQuizInt reg qs (getIndex reg)
        let maxInt = V.length $ associations reg
        case res of
          Nothing -> assert False
          Just n  -> assert $ inRange (0, maxInt) n

  describe "makeQuestionGen" $
    it "can create a reusable question generator" $
      property $ \ (toQuestion, checkAnswer, assoc) ->
        let gen = makeQuestionGen toQuestion checkAnswer
        in  question (gen assoc) == toQuestion assoc


-- Convenience function to run a Quiz Int computation and return Maybe Int in IO
runQuizInt :: Registry -> QuizState -> Quiz Int -> IO (Maybe Int)
runQuizInt registry scoreBoard k = do
  res <- runQuiz registry scoreBoard k
  case res of
    (Left  _, _) -> return Nothing
    (Right r, _) -> return $ Just r
