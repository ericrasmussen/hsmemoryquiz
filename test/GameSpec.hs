module GameSpec
       where


import Quiz
import Util()
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Ix (inRange)
import qualified Data.Vector as V


spec :: Spec
spec = do
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


-- Convenience function to run a Quiz Int computation and return Maybe Int in IO
runQuizInt :: Registry -> QuizState -> Quiz Int -> IO (Maybe Int)
runQuizInt registry scoreBoard k = do
  res <- runQuiz registry scoreBoard k
  case res of
    (Left  _, _) -> return Nothing
    (Right r, _) -> return $ Just r
