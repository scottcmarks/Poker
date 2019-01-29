{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Types used by the Poker extension to Database.Persist.Sql
-- Despite the name, this module uses nothing Poker-schema-specific
--
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
