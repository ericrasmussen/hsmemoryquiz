{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, RankNTypes #-}

-- |
-- Module      : Instances
-- Copyright   : (c) 2013 Eric Rasmussen, based on code (c) 2009 Antoine Latter
--               (originally from System.Haskeline.Class with a BSD3 license)
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Newtype wrapper for our quiz stack, along with the QuizState that keeps score
-- throughout the game.


{-

NOTE: portions of this code are (c) 2009 Antoine Latter from the haskeline-class
module, which no longer works with haskeline >= 0.7.

The original license is reprinted below:

Copyright (c) 2009, Antoine Latter

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
    * The names of the authors may not be used to endorse or promote
products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Instances
       ( MonadHaskeline(..)
       , H.handleInterrupt
       , H.withInterrupt
       , H.defaultSettings
       , H.InputT
       , runInputTwithDefaults
       , module System.Console.Haskeline.MonadException
       ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import System.Console.Haskeline.MonadException

import qualified System.Console.Haskeline as H


-- -----------------------------------------------------------------------------
-- * Define the MonadHaskeline instances we'll need for our Quiz stack

class MonadException m => MonadHaskeline m where
  getInputLine :: String -> m (Maybe String)
  getInputChar :: String -> m (Maybe Char)
  outputStr    :: String -> m ()
  outputStrLn  :: String -> m ()

instance MonadException m => MonadHaskeline (H.InputT m) where
  getInputLine = H.getInputLine
  getInputChar = H.getInputChar
  outputStr    = H.outputStr
  outputStrLn  = H.outputStrLn

instance (Error e, MonadHaskeline m) => MonadHaskeline (ErrorT e m) where
  getInputLine = lift . getInputLine
  getInputChar = lift . getInputChar
  outputStr    = lift . outputStr
  outputStrLn  = lift . outputStrLn


-- -----------------------------------------------------------------------------
-- * State and Reader instances for HaskelineT, also for our Quiz Stack

instance MonadState s m => MonadState s (H.InputT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (H.InputT m) where
  ask       = lift ask
  local f m = H.mapInputT (local f) m

-- -----------------------------------------------------------------------------
-- * Working with InputT

-- Runs InputT with the default settings and withInterrupt to catch ctrl-c
runInputTwithDefaults :: MonadException m => H.InputT m a -> m a
runInputTwithDefaults = H.runInputT H.defaultSettings . H.withInterrupt


