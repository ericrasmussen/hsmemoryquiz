{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (catch)

import Quiz
import Digit
import Config
import Parser
import Association


import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector


import Control.Monad.Error
import Control.Monad.State


-- | A convenient but hacky way to run our game. Currently the Right case is
-- useless because game termination only happens by throwing an error when
-- someone types "quit" at the prompt. A possible TODO is having a user
-- optionally supply a number of times to be tested so that quitting early
-- would be an actual exception condition.
runGame :: QuizState -> IO ()
runGame q = do
  -- TODO: have the config include a strategy for accessing associations so
  -- getRand won't be hardcoded here
  res <- runQuiz q playGame
  case res of
    (Left  e, _) -> putStrLn e
    (Right _, q) -> putStrLn $ "Final score: " ++ show q

-- | Parses command line args, attempts to build a QuizState, and either runs
-- the game or displays the error.
main = do
  cfg    <- cmdArgs config
  quizSt <- fromConfig cfg
  case quizSt of
    Left e  -> putStrLn e
    Right r -> runGame r




