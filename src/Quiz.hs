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
       )
       where

import Instances
import Association

import Numeric (showFFloat)

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict



-- -----------------------------------------------------------------------------
-- * Quiz types and functions on quizzes

-- | The exported Quiz type that wraps our ErrorT, StateT, and IO stack
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

-- | Takes a starting QuizState and evaluates a Quiz block
runQuiz :: Registry -> QuizState -> Quiz a -> IO (Either String a, QuizState)
runQuiz registry scoreBoard k = do
  let err = runErrorT $ getQuiz k
  let inp = runInputTwithDefaults err
  let env = runReaderT inp registry
  runStateT env scoreBoard


-- | Read-only registry for application settings
data Registry = Registry {
    genQuestion  :: Association -> Question
  , associations :: AssociationDB
  , getIndex     :: Quiz Int
}

instance Show Registry where
  show r = concat ["Registry: ", show $ associations r]

-- | Keep track of the score (number of correct answers) and the total questions
-- asked. Also contains a generator that knows how to create a Question from a
-- given Association.
data QuizState = QuizState {
    score :: Int
  , total :: Int
  }


-- | Make our QuizState pretty for console output
-- The resulting string will be in the form: score/total (percentage)
instance Show QuizState where
  show QuizState {score=s, total=t} = fraction ++ " (" ++ percentage ++ ")"
    where fraction   = formatFraction   s t
          percentage = formatPercentage s t

-- | Creates a fresh QuizState starting at 0/0 questions answered
newQuizState :: QuizState
newQuizState = QuizState { score = 0, total = 0 }

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
-- an answer
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
makeQuestionGen toQuestion checkAnswer = \assoc ->
  Question {
      question  = toQuestion  assoc
    , evaluator = checkAnswer assoc
    }

-- | Helper to test a given Response against a Question's check answer predicate
checkResponse :: Question -> Response -> Result
checkResponse (Question {evaluator=eval}) response = eval response




-- -----------------------------------------------------------------------------
-- * Helper functions private to this module

-- | Formats integers x and y as a string in the form "x/y"
formatFraction :: Int -> Int -> String
formatFraction x y = show x ++ "/" ++ show y

-- | Divides two Integers as doubles and formats the result as a percentage in
-- the form "x.yz%". Although you can't divide by 0, we handle the x / 0 case by
-- displaying "0%".
formatPercentage :: Int -> Int -> String
formatPercentage x 0 = "0%"
formatPercentage x y = showFFloat (Just decimals) percentage "%"
  where percentage = 100.0 * (fromIntegral x / fromIntegral y)
        decimals   = if isWhole percentage then 0 else 2

-- | Helper to check if a percentage (fractional) is a whole number
isWhole :: RealFrac a => a -> Bool
isWhole x = floor x == ceiling x

