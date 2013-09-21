module QuizSpec
       where


import Quiz
import Util()
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic


spec :: Spec
spec = do
  describe "QuizState" $ do
    it "can be created anew with newQuizState" $ do
      score newQuizState `shouldBe` 0
      total newQuizState `shouldBe` 0

    it "can be modified when scoring a response" $
      property $ \ x y b -> let st  = scoreResponse b (QuizState x y)
                                mod = if b then 1 else 0
                            in x + mod == score st && y + 1 == total st

  describe "Registry" $ do
    it "can be created with makeRegistry" $
      property $ \ (assoc, q, db, ind) ->
        let reg = makeRegistry (\ a -> q) db ind
            assocsEq   = associations reg == db
            questionEq = question (genQuestion reg $ assoc) == question q
        in assocsEq && questionEq

  describe "Questions" $ do
    it "makeQuestionGen creates a question generator" $
      property $ \ (toQuestion, checkAnswer, assoc) ->
        let gen = makeQuestionGen toQuestion checkAnswer
        in  question (gen assoc) == toQuestion assoc

    it "checkResponse uses a question's evaluator" $
      property $ \ q r -> checkResponse q r == evaluator q r
