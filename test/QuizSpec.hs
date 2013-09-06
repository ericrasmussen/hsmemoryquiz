module QuizSpec
       where

import Test.Hspec
import Test.QuickCheck

import Quiz
import Util()


{-

exported functions we still need to test:

runQuiz
playGame
makeQuestionGen
getRand
getOrdered
getReversed

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


