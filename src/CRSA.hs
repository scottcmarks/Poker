{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE OverloadedStrings   #-}

module CRSA
where

import RIO


import Control.Monad.Trans.Resource ( ResourceT, MonadUnliftIO )
import Conduit (
                 ConduitT
               , (.|)
               )
import Database.Persist.Sql (
    Entity
  , selectSource
  , runMigration
  )





import Database.Persist.Sqlite (
    runSqlite
  )

import SqlExtras (
    SqlActionT
  , SqlPersistEntity
  , noFilters
  , noSelectOpts
  )
  
import SqlRIO (
    SqlRIO
  , recordPeeler
  )


import App
import Poker


----- Sql(ite) + Record_Poker (migrateAll) -----

runSqliteMigrating :: (MonadUnliftIO m) => Text -> SqlActionT m a -> m a
runSqliteMigrating dbFilePath actions =
  runSqlite dbFilePath  $ do
    runMigration migrateAll
    actions

runSql :: (MonadUnliftIO m) => Text -> SqlActionT m a -> m a
-- runSql = runSqlite -- or runSqliteMigrating in dev version
runSql = runSqliteMigrating -- or runSqlite in release version


----- Sql(ite) + Record_Poker (migrateall) + RIO -----

runSqliteActions :: (HasDbFilePath env) => SqlRIO env m -> RIO env m
runSqliteActions actions = do
  dbFilePath <- view dbFilePathL
  runSql dbFilePath actions


----- Conduit + Sql(ite) + RIO + App  -----

-- Conduit synonym types for abbreviations
type CRSA  i o   = ConduitT i o (ResourceT (SqlRIO App))


sqlSource :: (SqlPersistEntity record) => CRSA () (Entity record) ()
sqlSource = selectSource noFilters noSelectOpts

recordSource :: (SqlPersistEntity record) => CRSA () record ()
recordSource = sqlSource .| recordPeeler
