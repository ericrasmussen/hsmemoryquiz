module QuizSpec
       where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Quiz
import Util()


{-

exported functions we still need to test:

runQuiz
playGame
makeQuestionGen

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
    it "always stays within bounds" $
      monadicIO $ do
        reg <- pick arbitrary
        qs  <- pick arbitrary
        res <- run $ runQuizInt reg qs (getIndex reg)
        case res of
          Nothing -> assert False   -- out of bounds or other error
          Just n  -> assert $ n >= 0  -- need a better way to test this


-- Convenience function to run a Quiz Int computation and return Maybe Int in IO
runQuizInt :: Registry -> QuizState -> Quiz Int -> IO (Maybe Int)
runQuizInt registry scoreBoard k = do
  res <- runQuiz registry scoreBoard k
  case res of
    (Left  _, _) -> return Nothing
    (Right r, _) -> return $ Just r
