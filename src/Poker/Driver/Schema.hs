{-|
Module      : Poker.Driver.Schema
Description : DB access and @record@ utilities
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Command implementations utilities using the Poker SQL DB Schema.

-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -dth-dec-file           #-}
{-# LANGUAGE TypeFamilies               #-}

module Poker.Driver.Schema
  ( -- * SQL runners
    runSqlite'
  , runSqlRIO
  , runCRSA
  , SqlPersistEntity
    -- * @record@ output functions
  , showRecord
  , listHeader
  , listRecord
  , csvHeader
  , csv
  , showNote
  , listNote
    -- * access the DB via 'Conduit.ConduitT'
  , sqlSource
  , recordSource
    -- * cache DB data in 'Data.Map.Map's
  , actionsTableMap
  , rolesTableMap
  , notesTableMap
  , action_roleTableMap
    -- * construct an App from an AppOptions
  , fromAppOptions
    -- * run a RIO App with logging per verbosity
  , runRIOLogging
    -- * following foreign keys
  , deref
  , (^->)
    -- * 'Map' through lens
  , (<^.>), (⋈)
  ) where

import           Conduit                 (mapC, runConduitRes, sinkList, (.|))
import           Control.Lens            (Getting, (.~))
import qualified Data.List               as DL (length, maximum)
import           Data.Map                (elems, fromList, (!), (!?))
import           Data.Maybe              (fromJust)
import           Data.Text               (unpack)
import           Database.Persist.Sql    (BaseBackend, Entity, Key,
                                          PersistEntity, PersistEntityBackend,
                                          SqlBackend, SqlPersistT, entityKey,
                                          entityVal, fromSqlKey, get,
                                          runMigration, selectSource)
import           Database.Persist.Sqlite (runSqlite)
import           Fmt                     (padLeftF, padRightF, (+|), (+||),
                                          (|+), (|++|), (||+))
import           RIO                     (Bool (..), Functor, IO, Int, Map,
                                          MonadIO, MonadUnliftIO, RIO, String,
                                          Text, Void, fromString, logFuncL,
                                          logInfo, logOptionsHandle, map,
                                          mempty, pure, runRIO, set, stderr, to,
                                          undefined, view, when, withLogFunc,
                                          ($), (&), (&&&), (***), (++), (.),
                                          (<&>), (==), (>>), (^.))
import           RIO.Process             (mkDefaultProcessContext)

import           Data.Map.PokerExtras    (inv, inv2, (⊚))
import           Poker.App               (App (..), AppOptions, CRSA,
                                          DBCache (..), HasConnectionString,
                                          ProcessingRole (..), RA,
                                          Record_Action (..), Record_ActionId,
                                          Record_Action_Role (..),
                                          Record_Action_RoleId,
                                          Record_Note (..), Record_NoteId,
                                          Record_Poker (..), Record_Role (..),
                                          Record_RoleId, SA, SqlActionT, SqlRIO,
                                          action, actionStr, action_id,
                                          action_role_id, appConnectionString,
                                          appDBCache, appLogFunc,
                                          appProcessContext, appVerbose,
                                          connStrL, dbCacheAction_Role,
                                          dbCacheActions, dbCacheIsHeader,
                                          dbCacheIsMerged, dbCacheL,
                                          dbCacheLookupNoteId, dbCacheNotes,
                                          dbCacheRoles, migrateAll, moneyInt,
                                          noFilters, noSelectOpts, note,
                                          noteStr, note_id, role, roleStr,
                                          role_id, verboseL)



-- | Put a header for the listAllrecords cmd
--
listHeader :: Text -> Int -> IO ()
listHeader db n =
    let column_heads = "Date        Amount  Note#\n"
    in case n of
          0 -> db|+" has no records!\n"
          1 -> db|+" has one record:\n"     +|column_heads
          _ -> db|+" has "+|n|+" records:\n"+|column_heads

-- | Put a record for the listAllrecords cmd
--
showRecord :: Record_Poker -> String
showRecord (Record_Poker d a nId) =
    d|++|padLeftF 7 ' ' (a^.moneyInt)|++|padLeftF 6 ' ' (fromSqlKey nId)

-- | Put a record for the listAllrecords cmd
listRecord:: Record_Poker -> IO ()
listRecord r = ""+|showRecord r|+"\n"

-- | Header for records in csv format
--
csvHeader :: String
csvHeader = "\"Date\"   ,\"Amount\"  ,\"Note\""

-- | Convert a record to csv format
--
csv :: Map Record_NoteId String -> Record_Poker -> String
csv note_ids (Record_Poker d a nId) =
    ""+|d|+ ","+|padLeftF 5 ' ' (a^.moneyInt)|+","+||(note_ids!nId)||+ ""

showNote :: Record_Note -> String
showNote = (^. note . noteStr)

listNote :: Record_Note -> IO ()
listNote n = showNote n |+ "\n"

-- | Specialize PersistEntity to be processed by the SqlBackend back end
--
class ( PersistEntity record
      , PersistEntityBackend record ~ BaseBackend (BaseBackend SqlBackend)
      ) =>
      SqlPersistEntity record

-- | All the record types from the schema
--   are instances of SqlPersistEntity
--
instance SqlPersistEntity Record_Action

instance SqlPersistEntity Record_Note

instance SqlPersistEntity Record_Role

instance SqlPersistEntity Record_Action_Role

instance SqlPersistEntity Record_Poker


--   Sql(ite) + Record_Poker (migrateAll) -----

-- | Run actions after runMigration
runSqliteMigrating :: (MonadUnliftIO m) => Text -> SqlActionT m a -> m a
runSqliteMigrating connStr actions =
    runSqlite connStr $ runMigration migrateAll >> actions

-- | Run actions in SqlActionT
runSqlite' :: (MonadUnliftIO m) => Text -> SqlActionT m a -> m a
-- runSqlite' = runSqlite -- or runSqliteMigrating in dev version
runSqlite' = runSqliteMigrating -- or runSqlite in release version

--  Sql(ite) + Record_Poker (migrateall) + RIO -----

-- | Run a 'SqlRIO' action with 'connStr' to define a 'RIO' action
--   in the RIO env m monad
runSqlRIO :: (HasConnectionString env) => SqlRIO env a -> RIO env a
runSqlRIO actions = do
    connStr <- view connStrL
    runSqlite' connStr actions


-- | Run a 'RIO' action with logging per @verbose@
runRIOLogging :: MonadUnliftIO m => App -> RA r -> m r
runRIOLogging app cmd = do
    let verbose = app ^. verboseL
    lo <- logOptionsHandle stderr verbose
    withLogFunc lo ( \lf -> do
        runRIO (set logFuncL lf app) $ do
            connStr <- view connStrL
            when verbose $ logInfo $ fromString $ "connStr=" ++ unpack connStr
            cmd)


-- | The action to run SQL actions in an @env@
runSql :: MonadUnliftIO m =>
    App
 -- ^ environment
 -> SA r
 -- ^ SQL actions
 -> m r
runSql app = runRIOLogging app . runSqlRIO

-- | The action to run a SQL conduit for its return value in an @env@
runCRSA :: MonadUnliftIO m =>
    App
 -- ^ environment
 -> CRSA () Void r
 -- ^ conduit
 -> m r
runCRSA app = runSql app . runConduitRes


-- | Polytypic source for Entity records from DB per our 'App' (env)
--
sqlSource :: (SqlPersistEntity record) => CRSA () (Entity record) ()
sqlSource = selectSource noFilters noSelectOpts

-- | Polytypic source for schema records from DB per our 'App' (env)
--   Just 'sqlSource' with the ('Entity' ...) stripped off
recordSource :: (SqlPersistEntity record) => CRSA () record ()
recordSource = sqlSource .| mapC entityVal

-- | Access the DB to build a 'Map' for a @record@ table
buildDbTableMap :: (SqlPersistEntity record) =>
    CRSA () c (Map (Key record) record)
buildDbTableMap = sqlSource .| mapC pairFn .| sinkList <&> fromList
                where pairFn = entityKey &&& entityVal

-- | Access the DB to build a 'Map' that caches the actions table
actionsTableMap :: CRSA () c (Map Record_ActionId Record_Action)
actionsTableMap = buildDbTableMap :: CRSA () c (Map Record_ActionId Record_Action)

-- | Access the DB to build a 'Map' that caches the roles table
rolesTableMap :: CRSA () c (Map Record_RoleId Record_Role)
rolesTableMap = buildDbTableMap :: CRSA () c (Map Record_RoleId Record_Role)

-- | Access the DB to build a 'Map' that caches the notes table
notesTableMap :: CRSA () c (Map Record_NoteId Record_Note)
notesTableMap = buildDbTableMap :: CRSA () c (Map Record_NoteId Record_Note)

-- | Access the DB to build a 'Map' that caches the action_role table
action_roleTableMap :: CRSA () c (Map Record_Action_RoleId Record_Action_Role)
action_roleTableMap = buildDbTableMap :: CRSA () c (Map Record_Action_RoleId Record_Action_Role)


-- | Build an App environment from AppOptions
--   Does an early access to the DB to get the maps to
--   be curried in to closures for record predicates
fromAppOptions :: MonadUnliftIO m => AppOptions -> m App
fromAppOptions opts = do
    pc <- mkDefaultProcessContext
    let connStr = view connStrL opts
        bootstrApp = App
                { appLogFunc          = mempty
                , appProcessContext   = pc
                , appVerbose          = False
                , appConnectionString = connStr
                , appDBCache          = undefined
                }

    runCRSA bootstrApp $ do
        actions     <- actionsTableMap
        roles       <- rolesTableMap
        notes       <- notesTableMap
        action_role <- action_roleTableMap

        let makeIsRole = makeIsRole' notes action_role roles
            isHeader = makeIsRole "header"
            isMerged = makeIsRole "merged"

            lookupNoteId = lookupNoteId' notes

            width t = DL.maximum $ map DL.length $ elems t
            aw = width aidas
            nw = width nidns
            rw = width ridrs
            (%)w = padRightF w ' '
            listNoteEx n = "" +|nw%ns|+"  "+|aw%as|+"  "+|rw%rs|+"\n"
              where ns      =          n ^. note . noteStr
                    (as,rs) = arids ! (n ^. action_role_id)


            nidns = notes       ⋈ note   . noteStr
            nsnid = inv nidns
            aidas = actions     ⋈ action . actionStr
            asaid = inv aidas
            ridrs = roles       ⋈ role   . roleStr
            rsrid = inv ridrs
            arar  = action_role ⋈ to ((^. action_id) &&& (^. role_id))
            arid  = inv2 arar
            arids = arar        ⋈ to ((aidas !) *** (ridrs !))
            sarid = inv2 arids


        pure $ bootstrApp
            & verboseL .~
                opts ^. verboseL
            & dbCacheL .~
                DBCache { dbCacheActions             = actions
                        , dbCacheRoles               = roles
                        , dbCacheNotes               = notes
                        , dbCacheAction_Role         = action_role
                        , dbCacheIsHeader            = isHeader
                        , dbCacheIsMerged            = isMerged
                        , dbCacheLookupNoteId        = lookupNoteId
                        , dbCacheListNoteEx          = listNoteEx
                        , dbCacheNIdNs               = nidns
                        , dbCacheNsNId               = nsnid
                        , dbCacheAIdAs               = aidas
                        , dbCacheAsAId               = asaid
                        , dbCacheRIdRs               = ridrs
                        , dbCacheRsRId               = rsrid
                        , dbCacheARId                = arid
                        , dbCacheSARId               = sarid
                        }
      where
        makeIsRole' notes action_role roles s = (rr ==) . pr
          where
            rr = srr ! ProcessingRole s :: Record_RoleId
            srr = inv $ roles ⋈ role    :: Map ProcessingRole Record_RoleId

            ar = action_role ⋈ role_id  :: Map Record_Action_RoleId Record_RoleId
            ra = notes ⋈ action_role_id :: Map Record_NoteId Record_Action_RoleId
            nr = ra ⊚ ar                :: Map Record_NoteId Record_RoleId
            pr = (nr !) . (^. note_id)  :: Record_Poker -> Record_RoleId

        lookupNoteId' notes = (nn !?)
          where
            nn = (inv $ notes ⋈ note)


deref :: (SqlPersistEntity a, MonadIO c) =>
      Key a
   -> SqlPersistT c a
deref k = get k <&> fromJust

(^->) :: (MonadIO c, SqlPersistEntity a) =>
      s
   -> Getting (Key a) s (Key a)
   -> SqlPersistT c a
v ^-> l = v ^. l & deref
infixl 8 ^->

-- | map a getter over a functor
-- E.g.  Map a s ⋈ Lens' s b => Map a b
--
(⋈) :: Functor f =>
    f s
 -- ^ target, usually a Traversable
 -> Getting b s b
 -- ^ getter, usually a Lens'
 -> f b
t ⋈ l = t <&> (^. l)
infixl 8 ⋈

-- | Ascii version of ⋈
--
(<^.>) :: Functor f => f s -> Getting b s b -> f b
(<^.>)  = (⋈)
infixl 8 <^.>
