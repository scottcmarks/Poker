{-|
Module      : Main
Description : Executable main for poker
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Process the command line
sending the resulting @(env, cmd)@ pair to 'runRIOLogging'.
-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           RIO          (IO, uncurry, (>>=))

import           Poker.Driver (getAppAndCommand, runRIOLogging)

-- | Main
--
main :: IO ()
main = getAppAndCommand >>= uncurry runRIOLogging
