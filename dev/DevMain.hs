
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DevMain where


import           Conduit              (ConduitT, ResourceT, runConduitRes,
                                       sinkList, (.|))
import           Data.Map             (Map, (!))
import           RIO                  (Bool (..), MonadUnliftIO, Read, String,
                                       Text, Void, flip, id, to, ($), (&&&),
                                       (***), (.), (>>=), (^.))
import           RIO.Process          (ProcessContext, mkDefaultProcessContext)
import           System.IO.Unsafe     (unsafePerformIO)
import           Text.Read            (ReadS, read, reads)

import           Data.Map.PokerExtras (Map2, inv, inv2)
import           Poker.App            (App, AppOptions (..), CRSA, RA,
                                       Record_Action, Record_ActionId,
                                       Record_Action_Role, Record_Action_RoleId,
                                       Record_Note, Record_NoteId, Record_Poker,
                                       Record_Role, Record_RoleId, SA, action,
                                       actionStr, action_id, note, noteStr,
                                       role, roleStr, role_id)
import           Poker.Driver.Process (actionRoleSource, actionSource,
                                       noteSource, rawRecordSource, roleSource)
import           Poker.Driver.Schema  (action_roleTableMap, actionsTableMap,
                                       fromAppOptions, notesTableMap,
                                       rolesTableMap, runRIOLogging, runSqlRIO,
                                       (⋈))

verbose :: Bool
verbose = True

connStr :: Text
connStr = "Poker.db"

options :: AppOptions
options =
    AppOptions
    {
        appOptionsVerbose          = verbose
    ,   appOptionsConnectionString = connStr
    }

pc :: ProcessContext
pc = unsafePerformIO mkDefaultProcessContext

app :: MonadUnliftIO m => m App
app = fromAppOptions options

runApp :: MonadUnliftIO m => RA a -> m a
runApp cmd = app >>= flip runRIOLogging cmd

rc :: MonadUnliftIO m
  => ConduitT () Void (ResourceT m) r
  -> m r
rc = runConduitRes

rs :: CRSA () Void a -> RA a
rs = runSqlRIO . rc

ra :: MonadUnliftIO m => CRSA () Void a -> m a
ra = runApp . rs

runSA :: MonadUnliftIO m => SA a -> m a
runSA = runApp . runSqlRIO

getRecords ::MonadUnliftIO m => m [Record_Poker]
getRecords = ra $ rawRecordSource .| sinkList

records :: [Record_Poker]
records = unsafePerformIO getRecords

getNotes :: MonadUnliftIO m => m [Record_Note]
getNotes = ra $ noteSource .| sinkList

notes :: [Record_Note]
notes = unsafePerformIO getNotes

getActions :: MonadUnliftIO m => m [Record_Action]
getActions = ra $ actionSource .| sinkList

actions :: [Record_Action]
actions = unsafePerformIO getActions

getRoles :: MonadUnliftIO m => m [Record_Role]
getRoles = ra $ roleSource .| sinkList

roles :: [Record_Role]
roles = unsafePerformIO getRoles

getActionRoles :: MonadUnliftIO m => m [Record_Action_Role]
getActionRoles = ra $ actionRoleSource .| sinkList

actionRoles :: [Record_Action_Role]
actionRoles = unsafePerformIO getActionRoles


actionsTable :: Map Record_ActionId Record_Action
actionsTable = unsafePerformIO $ ra actionsTableMap

rolesTable :: Map Record_RoleId Record_Role
rolesTable     = unsafePerformIO $ ra rolesTableMap

notesTable :: Map Record_NoteId Record_Note
notesTable     = unsafePerformIO $ ra notesTableMap

action_roleTable :: Map Record_Action_RoleId Record_Action_Role
action_roleTable  = unsafePerformIO $ ra action_roleTableMap


nidns :: Map Record_NoteId String
nidns = notesTable       ⋈ note   . noteStr

nsnid :: Map String Record_NoteId
nsnid = inv nidns


aidas :: Map Record_ActionId String
aidas = actionsTable     ⋈ action . actionStr

asaid :: Map String Record_ActionId
asaid = inv aidas


ridrs :: Map Record_RoleId String
ridrs = rolesTable       ⋈ role   . roleStr

rsrid :: Map String Record_RoleId
rsrid = inv ridrs


arid :: Map Record_Action_RoleId (Record_ActionId, Record_RoleId)
arid = action_roleTable ⋈ to ((^. action_id) &&& (^. role_id))

idar :: Map2 Record_ActionId Record_RoleId Record_Action_RoleId
idar = inv2 arid


arids :: Map Record_Action_RoleId (String, String)
arids = arid ⋈ to ((aidas !) *** (ridrs !))

sarid :: Map2 String String Record_Action_RoleId
sarid = inv2 arids


idTypeOf :: a -> a -> a
idTypeOf _t = id

-- | two readers that take a dummy parameter for type inference

readTypeOf :: Read a => a -> String -> a
readTypeOf _t = read

readsTypeOf :: Read a => a -> ReadS a
readsTypeOf _t = reads
