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
-- Registry (the environment in our Reader monad instance) from the user
-- specified values.


module Config
       ( config
       , cmdArgs
       , fromConfig
       )
       where

import Quiz
import Game
import Digit
import Letter
import Parser
import Helpers
import Association
import Control.Applicative
import System.Console.CmdArgs
import System.IO.Error (tryIOError)


-- -----------------------------------------------------------------------------
-- * Data and functions used by CmdArgs for command-line flag handling

-- | Records to represent command line args
data Config = Config {
    from  :: String
  , to    :: String
  , path  :: String
  , index :: String
  } deriving (Show, Data, Typeable)

-- | Additional help messages for specific flags
helpArgs = Config {
    from = "" &= help "You can be quizzed on digits, letters, or mnemonics"
  , to   = "" &= help "You can provide answers as digits, letters, or mnemonics"
  , path = "" &= help "The location of your associations text file"
  , index = "random"
           &= help "How to generate questions: random, ordered, reversed"
  }

-- | Add additional detail to the --help output
config = helpArgs
         &= help    "Practice your Dominic memory associations"
         &= summary "hsmemoryquiz 0.1 (C) Eric Rasmussen 2013"
         &= program "hsmemoryquiz"


-- -----------------------------------------------------------------------------
-- * Attempt to build a Registry from the command line args

-- | Takes a config object (user input via command-line flags) and will either
-- return a Registry or a String error
fromConfig :: Config -> IO (Either String Registry)
fromConfig cfg = do
  contents         <- readFileSafe (path cfg)
  let assocs       = parseFileSafe contents
  let questionGen  = questionGenerator (from cfg) (to cfg)
  let ind          = eitherIndex (index cfg)
  return $ eitherRegistry questionGen (leftToString assocs) ind


-- | Attempts to build a Registry from the given question generator and DB
eitherRegistry :: Either String (Association -> Question)
               -> Either String AssociationDB
               -> Either String (Quiz Int)
               -> Either String Registry
eitherRegistry questionGen assocs ind =
  makeRegistry <$> questionGen <*> assocs <*> ind


-- | Takes input (originally from a command-line flag) and attempts to return
-- a function that can render an association in one of its projection types
-- (digits, letters, or mnemonic)
eitherView :: String -> Either String RenderAssociation
eitherView s = case lower s of
  "digits"    -> Right $ \assoc -> show (view assoc :: DigitPair)
  "letters"   -> Right $ \assoc -> show (view assoc :: LetterPair)
  "mnemonics" -> Right $ \assoc -> show (view assoc :: Mnemonic)
  _           -> Left  $ invalidCommand s

-- | Will try to build an answer checker for the answer type specified by the
-- user (originally in a command-line flag)
eitherAnswer :: String -> Either String (Association -> String -> Result)
eitherAnswer s = case lower s of
  "digits"    -> Right $ \assoc -> checkAnswer (view assoc :: DigitPair)
  "letters"   -> Right $ \assoc -> checkAnswer (view assoc :: LetterPair)
  "mnemonics" -> Right $ \assoc -> checkAnswer (view assoc :: Mnemonic)
  _           -> Left  $ invalidCommand s

-- | Will try to choose an indexing strategy for choosing subsequent
-- Associations to test the user with during a quiz
eitherIndex :: String -> Either String (Quiz Int)
eitherIndex s = case lower s of
  "random"   -> Right indexRand
  "ordered"  -> Right indexOrdered
  "reversed" -> Right indexReversed
  _          -> Left  $ invalidCommand s

-- | Attempts to build a question generator from the user's chosen question
-- (from) and answer (to) types
questionGenerator :: String -> String -> Either String (Association -> Question)
questionGenerator from to = makeQuestionGen
                            <$> eitherView from
                            <*> eitherAnswer to


-- -----------------------------------------------------------------------------
-- * Dealing with exceptions

-- | Parses the given file contents if they were read successfully
parseFileSafe :: Either String String -> Either String AssociationDB
parseFileSafe contents = case contents of
  Left  e -> Left e
  Right r -> leftToString $ runParse r

-- | Read a file, catch IO Exceptions, and wrap them in Either instead
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe fp = do
  result <- tryIOError (readFile fp)
  return $ leftToString result

-- | Converts the Left cases of any Either type to String
leftToString :: Show a => Either a b -> Either String b
leftToString = either (Left . show) (Right . id)

