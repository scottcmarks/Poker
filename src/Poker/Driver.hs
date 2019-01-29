{-|
Module      : Poker.Driver
Description : Re-export command line processor and runner
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Re-export command line processor and runner
-}
{-# LANGUAGE NoImplicitPrelude #-}


module Poker.Driver
  ( getAppAndCommand
  , runRIOLogging
  ) where
import           Poker.Driver.CommandLine (getAppAndCommand)
import           Poker.Driver.Run         (runRIOLogging)
