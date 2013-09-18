-- |
-- Module      : Game
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Functions for running a game, playing game rounds, and interacting with the
-- user.


module Game
       ( runGame
       , indexRand
       , indexOrdered
       , indexReversed
       ) where


import Quiz
import Instances
import Association

import System.Random

import Data.List (isPrefixOf)

import Data.Vector ((!?))
import qualified Data.Vector as V

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict



-- -----------------------------------------------------------------------------
-- * Game basics

-- | A data type for distinguishing ongoing and stopped games
data Game = Continue | Stop


-- | Launches into a game until the user quits or an exception occurs, and
-- prints the results
runGame :: Registry -> IO ()
runGame registry = do
  res <- runQuiz registry newQuizState playGame
  case res of
    (Left  e, q) -> putStrLn $ formatError e q
    (Right _, q) -> putStrLn $ formatSuccess q


-- | Run a continuous game in our Quiz monad
playGame :: Quiz ()
playGame = do
  assoc <- nextAssociation
  res   <- playRound assoc
  case res of
    Continue -> playGame
    Stop     -> return ()

-- | Play a single round in our Quiz monad
playRound :: Association -> Quiz Game
playRound assoc = do
  env <- ask
  let question = genQuestion env $ assoc
  answer <- prompt question
  let result = checkResponse question answer
  if ":q" `isPrefixOf` answer
    then return Stop
    else updateResult result >> return Continue

-- | After the user answers a question, we use updateResult to print the result
-- and update the score.
updateResult :: Result -> Quiz ()
updateResult res = do
  st <- get
  let (wasCorrect, msg) = elimResult res
  put $ scoreResponse wasCorrect st
  outputStrLn msg

-- | Check the result to determine whether the user was correct (Right) or
-- incorrect (Left), and return it with the result message that will be
-- displayed to the user.
elimResult :: Result -> (Bool, String)
elimResult = either (makeTuple False) (makeTuple True)
  where makeTuple = (,)


-- -----------------------------------------------------------------------------
-- * Strategies for choosing Associations

-- | Get the next Association using getIndex from the Registry, guarding
-- against poor implementations of getIndex.
nextAssociation :: Quiz Association
nextAssociation = do
  env <- ask
  ind <- getIndex env
  let maybeAssoc = (associations env) !? ind
  case maybeAssoc of
    Just r  -> return r
    Nothing -> throwError "Programmer error: out of bounds access attempt"


-- | Always choose a random Association
indexRand :: Quiz Int
indexRand = do
  env <- ask
  let maxInt = V.length (associations env) - 1
  liftIO $ getStdRandom $ randomR (0, maxInt)

-- | Gets the next Association in order (resets to 0 after reaching the max)
indexOrdered :: Quiz Int
indexOrdered = do
  st  <- get
  env <- ask
  let len = V.length (associations env)
  return $ total st `mod` len

-- | Gets the next Association in reverse (resets to the max after reaching 0)
indexReversed :: Quiz Int
indexReversed = do
  st  <- get
  env <- ask
  let len = V.length (associations env)
  let asked = total st `mod` len
  return $ (len - 1) - asked


-- -----------------------------------------------------------------------------
-- * Helper functions private to this module

-- | Display the final score after a successful game run
formatSuccess :: QuizState -> String
formatSuccess q = "Final score: " ++ show q

-- | If an error was caught during a game run, display the error and the final
-- score
formatError :: String -> QuizState -> String
formatError e q =  "Caught: " ++ e ++ "\nFinal score: " ++ show q

-- | Display a question in a prompt format
questionPrompt :: Question -> String
questionPrompt Question { question=q } = "> " ++ q ++ ": "

-- | Prompt for a line and exit on exception.
-- Note that Interrupt exceptions (ctrl-c) must be handled separately with
-- handleInterrupt, but we also need to check the result of getInputLine for the
-- Nothing case, which indicates an EOF exception.
prompt :: Question -> Quiz String
prompt q = handleInterrupt (throwError "interrupt") $ do
  res <- getInputLine (questionPrompt q)
  case res of
    Nothing -> throwError "EOF"
    Just s  -> return s


