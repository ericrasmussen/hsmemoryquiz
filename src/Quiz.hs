{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Quiz
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Newtype wrapper for our quiz stack, along with the QuizState that keeps score
-- throughout the game.


module Quiz
       ( Quiz
       , runQuiz
       , playGame
       , QuizState (..)
       , Question  (..)
       , makeQuestion
       , AnswerChecker
       )
       where

import Digit
import Letter
import Association

import System.Random

import System.IO (hFlush, stdout)

import Numeric (showFFloat)

import Control.Applicative

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Vector ((!))
import qualified Data.Vector as V


-- -----------------------------------------------------------------------------
-- * Quiz types and functions on quizzes

-- | The exported Quiz type that wraps our ErrorT, StateT, and IO stack
newtype Quiz a = Quiz {
  getQuiz :: ErrorT String (StateT QuizState IO) a
  } deriving (Monad, MonadIO, MonadError String, MonadState QuizState)


-- | Takes a starting QuizState and evaluates a Quiz block
runQuiz :: QuizState -> Quiz a -> IO (Either String a, QuizState)
runQuiz scoreBoard k = runStateT (runErrorT (getQuiz k)) scoreBoard



-- | Keep track of the score (number of correct answers) and the total questions
-- asked. Also contains a generator that knows how to create a Question from a
-- given Association.
data QuizState = QuizState {
    score        :: Int
  , total        :: Int
  -- TODO: ideally these last two would be in a Reader
  , genQuestion  :: Association -> Question
  , associations :: AssociationDB
  }


-- | Make our QuizState pretty for console output
-- The resulting string will be in the form: score/total (percentage)
instance Show QuizState where
  show QuizState {score=s, total=t} = fraction ++ " (" ++ percentage ++ ")"
    where fraction   = formatFraction   s t
          percentage = formatPercentage s t


-- -----------------------------------------------------------------------------
-- * Questions!

type Response      = String

type Result        = Either String String

type AnswerChecker = Association -> String -> Response

-- | Our primary question type with a showable question and a predicate to check
-- an answer
data Question = Question {
    question  :: String
  , evaluator :: Response -> Result
  }

instance Show Question where
  show Question { question=q } = q


makeQuestion :: RenderAssociation
             -> (Association -> Response -> Bool)
             -> RenderAssociation
             -> (Association -> Question)
makeQuestion from to correction = \assoc ->
  Question {
      question  = from assoc
    , evaluator = \s -> if (to assoc) s
                          then Right "Correct!"
                          else Left $
                               "We were looking for: " ++ (correction assoc)
    }

-- | Helper to test a given Response against a Question's check answer predicate
checkResponse :: Question -> Response -> Result
checkResponse (Question {evaluator=eval}) response = eval response

-- | Run a continuous game in our Quiz monad
playGame :: Quiz ()
playGame = do
  st <- get
  let maxInt = V.length (associations st)
  rand <- getRand maxInt
  -- TODO: switch to a total lookup function and handle the Nothing edge case by
  -- throwing an error
  let randAssoc = (associations st) ! rand
  playRound randAssoc >> playGame

-- | Run a single round in our Quiz monad
playRound :: Association -> Quiz ()
playRound assoc = do
  st <- get
  let question = genQuestion st $ assoc
  liftIO $ prompt question
  answer <- liftIO getLine
  -- TODO: really shouldn't use errors for a normal condition here
  when (answer == "quit") $ throwError ("See you later! Score: " ++ show st)
  res <- communicateResult question answer
  put $ scoreResponse res st


-- | Communicates the result to the user and returns an int
communicateResult :: Question -> Response -> Quiz Bool
communicateResult q a = either (printWith False) (printWith True) (checkResponse q a)
  where printWith b s = do
          liftIO $ putStrLn s
          return b

-- | The total questions asked is incremented by one for each answered question,
-- and the score will be incremented by 1 if the answer was correct.
scoreResponse :: Bool -> QuizState -> QuizState
scoreResponse correct (QuizState {score=s, total=t, genQuestion=g, associations=a}) =
  QuizState { score=s+modifier, total=t+1, genQuestion=g, associations=a }
    where modifier = if correct then 1 else 0

-- | Get a random number for some max bound in the Quiz monad
-- TODO: generalize this so other strategies can be swapped in, like going in
-- order or reverse
getRand :: Int -> Quiz Int
getRand max = liftIO $ getStdRandom $ randomR (0, max)


-- -----------------------------------------------------------------------------
-- * Helper functions private to this module

-- | Formats integers x and y as a string in the form "x/y"
formatFraction :: Int -> Int -> String
formatFraction x y = show x ++ "/" ++ show y

-- | Divides two Integers as doubles and formats the result as a percentage in
-- the form "x.yz%"
formatPercentage :: Int -> Int -> String
formatPercentage x y = showFFloat (Just 2) percentage "%"
  where percentage =
          if y == 0 then 0 else 100.0 * (fromIntegral x / fromIntegral y)

-- | Basic question prompt
prompt :: Question -> IO ()
prompt q = putStr (questionPrompt q) >> hFlush stdout

-- | Display a question in a prompt format
questionPrompt :: Question -> String
questionPrompt Question { question=q } = "> " ++ q ++ ": "
