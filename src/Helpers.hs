-- |
-- Module      : Helpers
-- Copyright   : (c) 2013 Eric Rasmussen
--
-- License     : BSD-style
-- Maintainer  : eric@chromaticleaves.com
-- Stability   : experimental
-- Portability : GHC
--
-- Helper/utility functions used by the other modules


module Helpers
       ( formatFraction
       , formatPercentage
       , upper
       , lower
       , invalidCommand
       )
       where

import Numeric (showFFloat)
import Data.Char (toLower, toUpper)


-- -----------------------------------------------------------------------------
-- * Help all the modules

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
        decimals   = if isWholeNumber percentage then 0 else 2

-- | Helper to check if a percentage (fractional) is a whole number
isWholeNumber :: RealFrac a => a -> Bool
isWholeNumber x = floor x == ceiling x

-- | Helper function to uppercase a String
upper :: String -> String
upper = map toUpper

-- | Helper function to lowercase a String
lower :: String -> String
lower = map toLower

-- | Formats a standard error message for an unrecognized command line arg
-- TODO: add a nicer message for the empty string
invalidCommand :: String -> String
invalidCommand s = "Option \"" ++ s ++ "\" not recognized"
