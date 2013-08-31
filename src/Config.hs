{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

-- |
-- Module      : Config
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parses command line args to build a Config object and attempts to build a
-- QuizState from the user specified values.


module Config
       ( config
       , cmdArgs
       , fromConfig
       )
       where

import Quiz
import Digit
import Letter
import Parser
import Association

import Data.Char   (toLower)

import System.Console.CmdArgs

import Control.Applicative

import Control.Monad.Error      (throwError, liftIO, MonadError, MonadIO)

import System.IO.Error          (tryIOError)


-- -----------------------------------------------------------------------------
-- * The CmdArgs based processor

-- | Records to represent command line args
data Config = Config {
    from :: String
  , to   :: String
  , path :: String
  } deriving (Show, Data, Typeable)

-- | Additional help messages for specific flags
helpArgs = Config {
    from = "" &= help "You can be quizzed on digits, letters, or mnemonics"
  , to   = "" &= help "You can provide answers as digits, letters, or mnemonics"
  , path = "" &= help "The location of your associations text file"
  }

-- | Add additional detail to the --help output
config = helpArgs
         &= help    "Quiz yourself on your Dominic memory associations"
         &= summary "MemoryQuiz (C) Eric Rasmussen 2013"
         &= program "MemoryQuiz"


-- -----------------------------------------------------------------------------
-- * Attempt to build a QuizState from command line args

-- | Takes a config object (built from command line flags) and will either
-- return a QuizState or a String error
fromConfig :: Config -> IO (Either String QuizState)
fromConfig cfg = do
  let questionGen  = questionGenerator (from cfg) (to cfg)
  contents         <- readFileSafe (path cfg)
  let assocs       = parseFileSafe contents
  return $ eitherQuizState questionGen (leftToString assocs)


-- | Attempts to build a QuizState from the given question generator and DB
eitherQuizState :: Either String (Association -> Question)
                -> Either String AssociationDB
                -> Either String QuizState
eitherQuizState questionGen assocs = QuizState 0 0 <$> questionGen <*> assocs


-- | Will try to build a question viewer based on the command line arg
eitherView :: String -> Either String RenderAssociation
eitherView s = case lower s of
  "digits"   -> Right $ \assoc -> show (view assoc :: DigitPair)
  "letters"  -> Right $ \assoc -> show (view assoc :: LetterPair)
  "mnemonic" -> Right $ \assoc -> show (view assoc :: Mnemonic)
  _          -> Left  $ invalidCommand s

-- | Will try to build an answer checker based on the command line arg
eitherAnswer :: String -> Either String (Association -> String -> Bool)
eitherAnswer s = case lower s of
  "digits"   -> Right $ \assoc -> checkAnswer (view assoc :: DigitPair)
  "letters"  -> Right $ \assoc -> checkAnswer (view assoc :: LetterPair)
  "mnemonic" -> Right $ \assoc -> checkAnswer (view assoc :: Mnemonic)
  _          -> Left  $ invalidCommand s

-- | Attempts to build a question generator from command line args
questionGenerator :: String -> String -> Either String (Association -> Question)
questionGenerator from to = makeQuestion
                            <$> eitherView from
                            <*> eitherAnswer to
                            <*> eitherView to


-- | Helper function to lowercase a String
lower :: String -> String
lower = map toLower


-- -----------------------------------------------------------------------------
-- * Dealing with exceptions

-- | Parses the given file contents if they were read successfully
parseFileSafe :: Either String String -> Either String AssociationDB
parseFileSafe contents = case contents of
  Left  e -> Left e
  Right r -> leftToString $ runParse r

-- | Read a file and wrap IO Exceptions in Either
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe fp = do
  result <- tryIOError (readFile fp)
  return $ leftToString result

-- | Converts Left cases to String
leftToString :: Show a => Either a b -> Either String b
leftToString = either (Left . show) (Right . id)

-- | Formats a standard error message for an unrecognized command line arg
invalidCommand :: String -> String
invalidCommand s = "Option \"" ++ s ++ "\" not recognized"

