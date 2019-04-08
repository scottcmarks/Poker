{-|
Module      : Database.Persist.Sql.PokerExtras.Types
Description : Extra types used by the Poker extension to Database.Persist.Sql
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Types used by the Poker extension to Database.Persist.Sql
Despite the name, this module uses nothing Poker-schema-specific
-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Database.Persist.Sql.PokerExtras.Types
    ( SqlActionT
    , SqlRIO
    , HasConnectionString(..)
    ) where

import           Control.Monad.Logger         (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Database.Persist.Sql         (SqlPersistT)
import           RIO                          (Lens', RIO, Text)


-- | Does this thing have a connStr?
--
class HasConnectionString env where
    -- | Lens to access the database file path from an @env@ data structure
    connStrL :: Lens' env Text


-- | Computations to be performed by runSqlite
--
type SqlActionT m = SqlPersistT (NoLoggingT (ResourceT m))

-- | SQL Actions in the 'RIO' env monad
--
type SqlRIO env = SqlActionT (RIO env)
