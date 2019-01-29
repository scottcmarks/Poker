{-|
Module      : Database.Persist.Sql.PokerExtras
Description : Extension to Database.Persist.Sql for poker
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Extension to Database.Persist.Sql.

Process the command line and dispatch to a "Run" routine
of type 'RIO.RIO' 'Poker.Driver.Types.App'.

Despite the name, this module uses nothing Poker-schema-specific.

-}

module Database.Persist.Sql.PokerExtras
  ( module Database.Persist.Sql.PokerExtras.Types
  , module Database.Persist.Sql.PokerExtras.Internal
  ) where

import           Database.Persist.Sql.PokerExtras.Internal
import           Database.Persist.Sql.PokerExtras.Types
