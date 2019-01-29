{-|
Module      : Poker.App
Description : Types for driver routines
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Define all the data types that define program global options
and command options and arguments.
-}
{-# LANGUAGE NoImplicitPrelude #-}

module Poker.App
  (
    -- * Option and argument types
    AppOptions(..)
  , DBCache(..)
  , App(..)
  , HasVerbose(..)
  , HasConnectionString(..)
  , HasIsHeader(..)
  , HasIsMerged(..)
  , HasDBCache(..)
  , HasActions (..)
  , HasRoles (..)
  , HasNotes (..)
  , HasAction_Role (..)
  , HasLookupNoteId (..)
  , HasListNoteEx (..)
  , HasNIdNs (..)
  , HasNsNId (..)
  , HasAIdAs (..)
  , HasAsAId (..)
  , HasRIdRs (..)
  , HasRsRId (..)
  , HasARId (..)
  , CashOptions(..)
  , ListOptions(..)
  , MergeOptions(..)
  , AddOptions(..)
  , NotesOptions(..)
  , Force(..)
  , ExplanatoryNote(..)  , noteStr
  , AccountingAction(..) , actionStr
  , Money(..)            , moneyInt
  , ProcessingRole(..)   , roleStr
   -- * Re-exported DB schema types and migration routine
   -- and record field lenses
  , Record_Poker(..)       , Record_PokerId
  , amount, date, note_id
  , Record_Action(..)      , Record_ActionId
  , action
  , Record_Role(..)        , Record_RoleId
  , role
  , Record_Note(..)        , Record_NoteId
  , note, action_role_id
  , Record_Action_Role(..) , Record_Action_RoleId
  , action_id, role_id

  , migrateAll

  -- * Types for building up the @SQL@ conduit transformer stack.
  , RA
  , SA
  , CRSA
  -- * SQL actions and related functions
  , SqlActionT
  , SqlRIO
  , noFilters
  , noSelectOpts
  , getSqlSingle
  , sum
  ) where

import           Conduit                          (ConduitT)
import           Control.Monad.Trans.Resource     (ResourceT)
import           Data.Time.Calendar               (Day (..))
import           RIO                              (Bool, HasLogFunc (..), IO,
                                                   Lens', LogFunc, Map,
                                                   Maybe (..), RIO, Show,
                                                   String, Text, lens, (.))
import           RIO.Process                      (HasProcessContext (..),
                                                   ProcessContext)

import           Data.Map.PokerExtras             (Map2)
import           Database.Persist.Sql.PokerExtras (HasConnectionString (..),
                                                   SqlActionT, SqlRIO, connStrL,
                                                   getSqlSingle, noFilters,
                                                   noSelectOpts, sum)
import           Poker.DB                         (AccountingAction (..),
                                                   ExplanatoryNote (..),
                                                   Money (..),
                                                   ProcessingRole (..),
                                                   Record_Action (..),
                                                   Record_ActionId,
                                                   Record_Action_Role (..),
                                                   Record_Action_RoleId,
                                                   Record_Note (..),
                                                   Record_NoteId,
                                                   Record_Poker (..),
                                                   Record_PokerId,
                                                   Record_Role (..),
                                                   Record_RoleId, action,
                                                   actionStr, action_id,
                                                   action_role_id, amount, date,
                                                   migrateAll, moneyInt, note,
                                                   noteStr, note_id, role,
                                                   roleStr, role_id)

-- | Command line global options
data AppOptions
  = AppOptions
     {
    -- | global verbosity setting
       appOptionsVerbose          :: !Bool
    -- | global DB file path setting
     , appOptionsConnectionString :: !Text
     }
  deriving (Show)

-- | Processing subcommand (list, chunks) line arguments
newtype MergeOptions
  = MergeOptions
     {
    -- | merge options merge skipping (raw) option
       mergeOptionsRaw :: Bool
     }
  deriving (Show)

-- | DB modification condition
data Force
  = DryRun
  | Confirm
  | Forced
  deriving (Show)


-- | 'Run.cash' command options
data CashOptions
  = ShowCash
     {
    -- | skip merge when computing current cash
       showCashRaw :: !Bool
     }
  | UpdateCash
     {
    -- | skip merge when computing previous cash
       updateCashRaw    :: !Bool
    -- | confirmation option
     , updateCashForce  :: !Force
    -- | amount
     , updateCashAmount :: !Money
    -- | note
     , updateCashNoteId :: !Record_NoteId
    -- | date
     , updateCashDate   :: !Day
     }
  deriving (Show)

-- | 'Run.list' and 'Run.export' command options
data ListOptions
  = ListOptions
     {
    -- | omit listing header line
       listOptionsNoHeader :: !Bool
    -- | skip merge when listing
     , listOptionsRaw      :: !Bool
     } deriving (Show)

-- | 'Run.add' command options
data AddOptions
  = AddOptions
     {
    -- | confirmation option
       recordValuesForce  :: !Force
    -- | amount
     , recordValuesAmount :: !Money
    -- | note
     , recordValuesNoteId :: !Record_NoteId
    -- | date
     , recordValuesDate   :: !Day
     } deriving (Show)


-- | 'Run.notes' command options
data NotesOptions
  = ShowNotes
     {
    -- | show action and role
       showNotesAll  :: !Bool
     }
  | NewNote
     {
    -- | confirmation option
       newNoteForce  :: !Force
    -- | note
     , newNoteNote   :: !ExplanatoryNote
    -- | action
     , newNoteAction :: !Record_ActionId
    -- | type
     , newNoteRole   :: !Record_RoleId
     }
  deriving (Show)


-- | DB small tables cached as maps and closures
data DBCache
  = DBCache
      {
     -- | cache of the Actions table
        dbCacheActions             :: Map Record_ActionId      Record_Action
     -- | cache of the Roles table
      , dbCacheRoles               :: Map Record_RoleId        Record_Role
     -- | cache of the Notes table
      , dbCacheNotes               :: Map Record_NoteId        Record_Note
     -- | cache of the Action_Role table
      , dbCacheAction_Role         :: Map Record_Action_RoleId Record_Action_Role
     -- | header record predicate
      , dbCacheIsHeader            :: Record_Poker   -> Bool
     -- | merged record predicate
      , dbCacheIsMerged            :: Record_Poker   -> Bool
     -- | lookup note to find noteid
      , dbCacheLookupNoteId        :: ExplanatoryNote
                                      -> Maybe Record_NoteId
     -- | list note using formatting based on table entry sizes
      , dbCacheListNoteEx          :: Record_Note -> IO ()
     -- | lookup note id to find note string
      , dbCacheNIdNs               :: Map Record_NoteId String
     -- | lookup note string to find note id
      , dbCacheNsNId               :: Map String Record_NoteId
     -- | lookup action id to find action string
      , dbCacheAIdAs               :: Map Record_ActionId String
     -- | lookup action string to find action id
      , dbCacheAsAId               :: Map String Record_ActionId
     -- | lookup role id to find role string
      , dbCacheRIdRs               :: Map Record_RoleId String
     -- | lookup role string to find role id
      , dbCacheRsRId               :: Map String Record_RoleId
     -- | lookup action_role id to find (action id, role id) pair
      , dbCacheARId                :: Map2 Record_ActionId Record_RoleId
                                           Record_Action_RoleId
     -- | lookup action string, role string to find action_role id
      , dbCacheSARId               :: Map2 String String Record_Action_RoleId
      }

-- | 'Poker.Driver.Run' command execution environment as @env@
data App
  = App
      {
     -- | log function
        appLogFunc          :: !LogFunc
     -- | process context
      , appProcessContext   :: !ProcessContext
     -- | verbosity
      , appVerbose          :: !Bool
     -- | DB file path
      , appConnectionString :: !Text
     -- | cache of small tables in DB
      , appDBCache          :: DBCache
      }


-- | Lens accessing the log function from 'App'
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})


-- | Lens accessing the process context from 'App'
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})


-- | Does this have a @verbose@ switch?
class HasVerbose env where
  -- | Lens accessing the verbose flag from an @env@ data structure
  verboseL :: Lens' env Bool

-- | Lens accessing the verbose flag from 'App'
instance HasVerbose App where
  verboseL = lens appVerbose (\x y -> x {appVerbose = y})

-- | Lens accessing the verbose flag from 'AppOptions'
instance HasVerbose AppOptions where
  verboseL = lens appOptionsVerbose (\x y -> x {appOptionsVerbose = y})


-- | Lens access the database file path from 'App'
instance HasConnectionString App where
  connStrL = lens appConnectionString (\x y -> x {appConnectionString = y})

-- | Lens access the database file path from 'AppOptions'
instance HasConnectionString AppOptions where
  connStrL = lens appOptionsConnectionString
                  (\x y -> x {appOptionsConnectionString = y})


-- | Does this have an @dbCache@?
class HasDBCache env where
  -- | Lens accessing the dbCache from an @env@ data structure
  dbCacheL :: Lens' env DBCache

-- | Lens accessing the dbCache from 'App'
instance HasDBCache App where
  dbCacheL = lens appDBCache (\x y -> x {appDBCache = y})



-- | Does this have an @isHeader@ predicate?
class HasIsHeader env where
    -- | Lens accessing the isHeader predicate from an @env@ data structure
    isHeaderL :: Lens' env (Record_Poker -> Bool)

-- | Lens accessing the isHeader predicate from 'App'
instance HasIsHeader DBCache where
   isHeaderL = lens dbCacheIsHeader (\x y -> x {dbCacheIsHeader = y})

-- | Lens accessing the isHeader predicate from 'App'
instance HasIsHeader App where
   isHeaderL = dbCacheL . isHeaderL


-- | Does this have an @isHeader@ predicate?
class HasIsMerged env where
    -- | Lens accessing the isMerged predicate from an @env@ data structure
    isMergedL :: Lens' env (Record_Poker -> Bool)

-- | Lens accessing the isMerged predicate from 'DBCache'
instance HasIsMerged DBCache where
    isMergedL = lens dbCacheIsMerged (\x y -> x {dbCacheIsMerged = y})

instance HasIsMerged App where
    isMergedL = dbCacheL . isMergedL


-- | Does this have a @actions@ table cache?
class HasActions env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  actionsL :: Lens' env (Map Record_ActionId Record_Action)

-- | Lens accessing the @actions@ table cache from 'DBCache'
instance HasActions DBCache where
  actionsL = lens dbCacheActions (\x y -> x {dbCacheActions = y})

-- | Lens accessing the @actions@ table cache from 'App'
instance HasActions App where
  actionsL = dbCacheL . actionsL


-- | Does this have a @roles@ table cache?
class HasRoles env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  rolesL :: Lens' env (Map Record_RoleId Record_Role)

-- | Lens accessing the @roles@ table cache from 'DBCache'
instance HasRoles DBCache where
  rolesL = lens dbCacheRoles (\x y -> x {dbCacheRoles = y})

-- | Lens accessing the @roles@ table cache from 'App'
instance HasRoles App where
  rolesL = dbCacheL . rolesL


-- | Does this have a @notes@ table cache?
class HasNotes env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  notesL :: Lens' env (Map Record_NoteId Record_Note)

-- | Lens accessing the @notes@ table cache from 'DBCache'
instance HasNotes DBCache where
  notesL = lens dbCacheNotes (\x y -> x {dbCacheNotes = y})

-- | Lens accessing the @notes@ table cache from 'App'
instance HasNotes App where
  notesL = dbCacheL . notesL


-- | Does this have a @action_role@ table cache?
class HasAction_Role env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  action_roleL :: Lens' env (Map Record_Action_RoleId Record_Action_Role)

-- | Lens accessing the @action_role@ table cache from 'DBCache'
instance HasAction_Role DBCache where
  action_roleL = lens dbCacheAction_Role (\x y -> x {dbCacheAction_Role = y})

-- | Lens accessing the @action_role@ table cache from 'App'
instance HasAction_Role App where
  action_roleL = dbCacheL . action_roleL

-- | Does this have a @role@ id to @role@ string 'Map' cache?
class HasListNoteEx env where
  -- | Lens accessing the function to list note using formatting
  -- based on table entry sizes
  listNoteExL :: Lens' env (Record_Note -> IO ())

  -- | Lens accessing the function to list note using formatting
  -- based on table entry sizes from 'DBCache'
instance HasListNoteEx DBCache where
  listNoteExL = lens dbCacheListNoteEx (\x y -> x {dbCacheListNoteEx = y})

  -- | Lens accessing the function to list note using formatting
  -- based on table entry sizes from 'App'
instance HasListNoteEx App where
  listNoteExL = dbCacheL . listNoteExL


-- | Does this have a @note@ id to @note@ string 'Map' cache?
class HasNIdNs env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  nidnsL :: Lens' env (Map Record_NoteId String)

-- | Lens accessing the @note@ id to @note@ string 'Map' cache
--   from 'DBCache'
instance HasNIdNs DBCache where
  nidnsL = lens dbCacheNIdNs (\x y -> x {dbCacheNIdNs = y})

-- | Lens accessing the @note@ id to @note@ string 'Map' cache
--   from 'App'
instance HasNIdNs App where
  nidnsL = dbCacheL . nidnsL


-- | Does this have a @note@ string to @note@ id 'Map' cache?
class HasNsNId env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  nsnidL :: Lens' env (Map String Record_NoteId)

-- | Lens accessing the @note@ string to @note@ id 'Map' cache
--   from 'DBCache'
instance HasNsNId DBCache where
  nsnidL = lens dbCacheNsNId (\x y -> x {dbCacheNsNId = y})

-- | Lens accessing the @note@ string to @note@ id 'Map' cache
--   from 'App'
instance HasNsNId App where
  nsnidL = dbCacheL . nsnidL


-- | Does this have a @action@ id to @action@ string 'Map' cache?
class HasAIdAs env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  aidasL :: Lens' env (Map Record_ActionId String)

-- | Lens accessing the @action@ id to @action@ string 'Map' cache
--   from 'DBCache'
instance HasAIdAs DBCache where
  aidasL = lens dbCacheAIdAs (\x y -> x {dbCacheAIdAs = y})

-- | Lens accessing the @action@ id to @action@ string 'Map' cache
--   from 'App'
instance HasAIdAs App where
  aidasL = dbCacheL . aidasL


-- | Does this have a @action@ id to @action@ string 'Map' cache?
class HasAsAId env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  asaidL :: Lens' env (Map String Record_ActionId)

-- | Lens accessing the @action@ id to @action@ string 'Map' cache
--   from 'DBCache'
instance HasAsAId DBCache where
  asaidL = lens dbCacheAsAId (\x y -> x {dbCacheAsAId = y})

-- | Lens accessing the @action@ id to @action@ string 'Map' cache
--   from 'App'
instance HasAsAId App where
  asaidL = dbCacheL . asaidL


-- | Does this have a @role@ id to @role@ string 'Map' cache?
class HasRIdRs env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  ridrsL :: Lens' env (Map Record_RoleId String)

-- | Lens accessing the @role@ id to @role@ string 'Map' cache
--   from 'DBCache'
instance HasRIdRs DBCache where
  ridrsL = lens dbCacheRIdRs (\x y -> x {dbCacheRIdRs = y})

-- | Lens accessing the @role@ id to @role@ string 'Map' cache
--   from 'App'
instance HasRIdRs App where
  ridrsL = dbCacheL . ridrsL


-- | Does this have a @role@ id to @role@ string 'Map' cache?
class HasRsRId env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  rsridL :: Lens' env (Map String Record_RoleId)

-- | Lens accessing the @role@ id to @role@ string 'Map' cache
--   from 'DBCache'
instance HasRsRId DBCache where
  rsridL = lens dbCacheRsRId (\x y -> x {dbCacheRsRId = y})

-- | Lens accessing the @role@ id to @role@ string 'Map' cache
--   from 'App'
instance HasRsRId App where
  rsridL = dbCacheL . rsridL


-- | Does this have a @action@ string, @role@ string to action_role_id 'Map' cache?
class HasARId env where
  -- | Lens accessing the isMerged predicate from an @env@ data structure
  aridL :: Lens' env (Map2 Record_ActionId Record_RoleId Record_Action_RoleId)

-- | Lens accessing the @action@ string, @role@ string to action_role_id 'Map' cache
--   from 'DBCache'
instance HasARId DBCache where
  aridL = lens dbCacheARId (\x y -> x {dbCacheARId = y})

-- | Lens accessing the @action@ string, @role@ string to action_role_id 'Map' cache
--   from 'App'
instance HasARId App where
  aridL = dbCacheL . aridL




-- | Does this have an @lookupNoteId@ function?
class HasLookupNoteId env where
    -- | Lens accessing the lookupNoteId predicate from an @env@ data structure
    lookupNoteIdL :: Lens' env (ExplanatoryNote -> Maybe Record_NoteId)

-- | Lens accessing the lookupNoteId predicate from 'DBCache'
instance HasLookupNoteId DBCache where
    lookupNoteIdL = lens dbCacheLookupNoteId (\x y -> x {dbCacheLookupNoteId = y})

instance HasLookupNoteId App where
    lookupNoteIdL = dbCacheL . lookupNoteIdL



-- *  Sql(ite) + Conduit + Database.Persist.Sql.PokerExtras  + RIO + App  --
-- Types for building up the @SQL@ conduit transformer stack.

-- | Synonym for 'RIO' with our definition of 'App'
--
type RA = RIO App

-- | Synonym for 'SqlActionT' context for 'RIO' 'App'
--
type SA = SqlRIO App

-- | Synonym for 'ConduitT' running in the ResourceT SA monad
--
type CRSA i o = ConduitT i o (ResourceT SA)
