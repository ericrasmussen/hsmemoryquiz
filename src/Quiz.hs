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
       , QuizState (..)
       , newQuizState
       , Registry (..)
       , makeRegistry
       , Question  (..)
       , makeQuestionGen
       , checkResponse
       , scoreResponse
       )
       where

import Helpers
import Instances
import Association
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict


-- -----------------------------------------------------------------------------
-- * Quiz types and functions on quizzes

-- | The exported Quiz type that wraps our ErrorT, InputT, ReaderT, StateT, and
-- IO stack
newtype Quiz a = Quiz {
  getQuiz :: ErrorT String (InputT (ReaderT Registry (StateT QuizState IO))) a
  } deriving ( Monad
             , MonadIO
             , MonadException
             , MonadHaskeline
             , MonadError String
             , MonadState QuizState
             , MonadReader Registry
             )

-- | Takes a starting Registry and Quiz State, then uses it to evaluate a Quiz
-- block
runQuiz :: Registry -> QuizState -> Quiz a -> IO (Either String a, QuizState)
runQuiz registry scoreBoard k = do
  let err = runErrorT $ getQuiz k
  let inp = runInputTwithDefaults err
  let env = runReaderT inp registry
  runStateT env scoreBoard


-- -----------------------------------------------------------------------------
-- * The quiz state to be used in our Quiz's MonadState instance

-- | Keep track of the score (correct answers) and total (questions asked)
data QuizState = QuizState {
    score :: Int
  , total :: Int
  }

-- | Make our QuizState pretty for console output. The resulting string will be
-- in the form: score/total (percentage)
instance Show QuizState where
  show QuizState {score=s, total=t} = fraction ++ " (" ++ percentage ++ ")"
    where fraction   = formatFraction   s t
          percentage = formatPercentage s t

-- | Creates a fresh QuizState starting at 0/0 questions answered
newQuizState :: QuizState
newQuizState = QuizState { score = 0, total = 0 }

-- | The total questions asked is incremented by one for each answered question,
-- and the score will be incremented by 1 if the answer was correct.
scoreResponse :: Bool -> QuizState -> QuizState
scoreResponse correct st@(QuizState { score=s, total=t }) =
  st { score=s+modifier, total=t+1 }
    where modifier = if correct then 1 else 0


-- -----------------------------------------------------------------------------
-- * The environment to be used in our Quiz's MonadReader instance

-- | Read-only registry for application settings
data Registry = Registry {
    genQuestion  :: Association -> Question
  , associations :: AssociationDB
  , getIndex     :: Quiz Int
}

instance Show Registry where
  show r = "Registry: " ++ show (associations r)

-- | Constructs a Registry
makeRegistry :: (Association -> Question)
             -> AssociationDB
             -> Quiz Int
             -> Registry
makeRegistry gen assocs ind = Registry {
    genQuestion  = gen
  , associations = assocs
  , getIndex     = ind
  }

-- -----------------------------------------------------------------------------
-- * Questions!

-- | Our primary question type with a showable question and a predicate to check
-- a response
data Question = Question {
    question  :: String
  , evaluator :: Response -> Result
  }

instance Show Question where
  show Question { question=q } = q


-- | Create a reusable function to generate Questions from Associations
makeQuestionGen :: RenderAssociation         -- project an association
                -> AnswerChecker             -- check answer against association
                -> (Association -> Question) -- generate a question
makeQuestionGen toQuestion checkAnswer assoc = Question {
    question  = toQuestion  assoc
  , evaluator = checkAnswer assoc
  }

-- | Helper to test a given Response against a Question's check answer predicate
-- (currently only uses the Question's evaluator, but wrapping it in a function
-- is a more flexible API if we change it later)
checkResponse :: Question -> Response -> Result
checkResponse (Question {evaluator=eval}) = eval
