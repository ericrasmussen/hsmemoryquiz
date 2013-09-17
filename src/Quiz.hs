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
       , runGame
       , QuizState (..)
       , newQuizState
       , Registry (..)
       , makeRegistry
       , Question  (..)
       , makeQuestionGen
       , indexRand
       , indexOrdered
       , indexReversed
       )
       where

import Instances
import Association

import Data.List (isPrefixOf)

import System.Random

import Numeric (showFFloat)

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Vector ((!?))
import qualified Data.Vector as V


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

data Game = Continue | Stop



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

-- | Run a continuous game in our Quiz monad
playGame :: Quiz ()
playGame = do
  assoc <- nextAssociation
  res   <- playRound assoc
  case res of
    Continue -> playGame
    Stop     -> return ()


-- | Run a single round in our Quiz monad
playRound :: Association -> Quiz Game
playRound assoc = do
  st  <- get
  env <- ask
  let question = genQuestion env $ assoc
  answer <- prompt question
  if ":q" `isPrefixOf` answer
    then return Stop
    else do
      res <- communicateResult question answer
      put $ scoreResponse res st
      return Continue

-- | A convenient way to run our game.
runGame :: Registry -> IO ()
runGame registry = do
  res <- runQuiz registry newQuizState playGame
  case res of
    (Left  e, q) -> putStrLn $ "Caught: " ++ e ++ "\nFinal score: " ++ show q
    (Right _, q) -> putStrLn $ "Final score: " ++ show q


-- | Communicates the result to the user and returns an int
communicateResult :: Question -> Response -> Quiz Bool
communicateResult q a = either (printWith False) (printWith True) (checkResponse q a)
  where printWith b s = outputStrLn s >> return b

-- | The total questions asked is incremented by one for each answered question,
-- and the score will be incremented by 1 if the answer was correct.
scoreResponse :: Bool -> QuizState -> QuizState
scoreResponse correct st@(QuizState { score=s, total=t }) =
  st { score=s+modifier, total=t+1 }
    where modifier = if correct then 1 else 0

-- | Get a random number for some max bound in the Quiz monad
indexRand :: Quiz Int
indexRand = do
  env <- ask
  let maxInt = V.length (associations env) - 1
  liftIO $ getStdRandom $ randomR (0, maxInt)

-- | Run through the associations in order
indexOrdered :: Quiz Int
indexOrdered = do
  st  <- get
  env <- ask
  let len = V.length (associations env)
  return $ total st `mod` len

-- | Run through the associations in reverse
indexReversed :: Quiz Int
indexReversed = do
  st  <- get
  env <- ask
  let len = V.length (associations env)
  let asked = total st `mod` len
  return $ (len - 1) - asked

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


