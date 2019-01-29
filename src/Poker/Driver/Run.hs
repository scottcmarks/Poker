{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-|
Module      : Poker.Driver.Run
Description : Command implementation
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Definitions of command implementations and the runner 'runRIOLogging'.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poker.Driver.Run
  ( countRecords
  , list
  , exportAsCsv
  , chunks
  , cash
  , add
  , notes
  , getDefaultNoteIdFromDB
  , fromAppOptions
  , runRIOLogging
  ) where

import           Conduit                  (ConduitT, mapC, mapM_C,
                                           runConduitRes, sumC, (.|))
import           Data.Char                (toLower)
import           Data.Conduit.Combinators as CC (length)
import           Data.Map                 (elems, filter, findMin, (!))
import           Data.Text                (unpack)
import           Data.Time.Calendar       (Day, showGregorian)
import           Database.Persist.Sql     (Filter, count, insert)
import           Fmt                      ((+|), (+||), (|+), (||+))
import           RIO                      (Bool (..), Either (..), Eq, IO, Int,
                                           Map, Maybe (..), MonadIO, Show,
                                           String, Text, Void, const,
                                           fromString, fst, liftIO, map, mapM_,
                                           maybe, otherwise, pure, return,
                                           unless, unlines, view, when, ($),
                                           (++), (-), (.), (<$>), (=<<), (==),
                                           (^.))
import           System.Console.Haskeline (defaultSettings, getInputChar,
                                           runInputT)
import           System.IO                (print, putStr, putStrLn)

import           Data.Map.PokerExtras     (lookup2, (⊚))
import           Poker.App                (AccountingAction (..),
                                           AddOptions (..), AppOptions (..),
                                           CRSA, CashOptions (..),
                                           ExplanatoryNote (..), Force (..),
                                           HasConnectionString (..),
                                           HasIsHeader (..), HasIsMerged (..),
                                           HasLookupNoteId (..),
                                           HasVerbose (..), ListOptions,
                                           MergeOptions (..), Money,
                                           NotesOptions (..), RA,
                                           Record_ActionId,
                                           Record_Action_RoleId,
                                           Record_Note (..), Record_NoteId,
                                           Record_Poker (..), Record_PokerId,
                                           Record_RoleId, SA, action, action_id,
                                           action_roleL, action_role_id,
                                           actionsL, aidasL, amount, aridL,
                                           listNoteExL, listOptionsNoHeader,
                                           listOptionsRaw, moneyInt, nidnsL,
                                           noFilters, note, noteStr, notesL,
                                           ridrsL, sum)
import           Poker.Driver.Process     (HChunk (..), processedHChunkSource,
                                           processedRecordSource,
                                           rawHChunkSource, rawRecordSource)
import           Poker.Driver.Schema      (csv, csvHeader, fromAppOptions,
                                           listHeader, listNote, listRecord,
                                           runRIOLogging, runSqlRIO, showRecord,
                                           (⋈))

getMergeOptions :: MergeOptions -> RA (Bool, Bool)
getMergeOptions opts = do
  verbose <- view verboseL
  let raw = mergeOptionsRaw opts
  return (verbose, raw)

getListOptions :: ListOptions -> RA (Bool, Bool, Bool)
getListOptions opts = do
  verbose <- view verboseL
  let raw = listOptionsRaw opts
      noHeader = listOptionsNoHeader opts
  return (verbose, raw, noHeader)

-- | Extract first note from DB auxiliary tables caches in App
--   filtered by specifying the "Profit" action
getFirstNoteId :: RA Record_NoteId
getFirstNoteId = do
    notes'      <- view notesL
    action_role <- view action_roleL
    actions     <- view actionsL
    pure $
        fst . findMin . filter (== AccountingAction "Profit")
            $ notes'      ⋈ action_role_id
            ⊚ action_role ⋈ action_id
            ⊚ actions     ⋈ action

-- | Retrieve the first (per notes.id sort) note field from the notes table
--
getDefaultNoteIdFromDB :: AppOptions -> IO Record_NoteId
getDefaultNoteIdFromDB appOptions = do
    app <- fromAppOptions appOptions
    runRIOLogging app getFirstNoteId

data ChunkProcessing = ChunkRaw        { _crIsHeader :: Record_Poker -> Bool }
                     | ChunkProcessed  { _cpIsHeader :: Record_Poker -> Bool
                                       , _cpIsMerged :: Record_Poker -> Bool }

chunkProcessing :: Bool -> RA ChunkProcessing
chunkProcessing True = do
    isHeader <- view isHeaderL
    pure $ ChunkRaw isHeader
chunkProcessing False = do
    isHeader <- view isHeaderL
    isMerged <- view isMergedL
    pure $ ChunkProcessed isHeader isMerged

chunkSource :: ChunkProcessing -> CRSA () (HChunk Record_Poker) ()
chunkSource (ChunkRaw isHeader) =
    rawHChunkSource isHeader
chunkSource (ChunkProcessed isHeader isMerged ) =
    processedHChunkSource isHeader isMerged

data RecordProcessing = RecordRaw
                      | RecordProcessed { _rpIsHeader :: Record_Poker -> Bool
                                        , _rpIsMerged :: Record_Poker -> Bool }

recordProcessing :: Bool -> RA RecordProcessing
recordProcessing True =
    pure RecordRaw
recordProcessing False = do
    isHeader <- view isHeaderL
    isMerged <- view isMergedL
    pure $ RecordProcessed isHeader isMerged

pokerRecordSource :: RecordProcessing -> CRSA () Record_Poker ()
pokerRecordSource RecordRaw =
    rawRecordSource
pokerRecordSource (RecordProcessed isHeader isMerged ) =
    processedRecordSource isHeader isMerged

countActions :: RecordProcessing -> SA Int
countActions RecordRaw =
    count (noFilters :: [Filter Record_Poker])
countActions rp =
    runConduitRes $ pokerRecordSource rp  .| CC.length

-- | Count of records in the Record_Poker (data) table
countRecords :: MergeOptions -> RA ()
countRecords opts = do
  (_, raw) <- getMergeOptions opts
  rp <- recordProcessing raw
  n <- runSqlRIO $ countActions rp
  liftIO $ print n

-- | Sink all items through a @put@ function
sinkC :: MonadIO m  =>
    (a -> IO ())
 -- ^ The @put@ function taking an item to compute an action in IO ()
  -> ConduitT a Void m ()
sinkC = mapM_C <$> (liftIO .)

listActions :: RecordProcessing -> (Record_Poker -> IO ()) -> SA ()
listActions rp putRecord = do
    runConduitRes $ pokerRecordSource rp .| sinkC putRecord

-- | List all records, with header
list :: ListOptions -> RA ()
list opts = do
    (_, raw, noHeader) <- getListOptions opts
    connStr <- view connStrL
    rp <- recordProcessing raw
    runSqlRIO $ do
        unless noHeader $ do
            n <- countActions rp
            liftIO $ listHeader connStr n
        listActions rp listRecord

-- export all records in csv file format
exportAsCsvActions :: RecordProcessing -> Map Record_NoteId String -> SA ()
exportAsCsvActions rp nn =
    listActions rp $ putStrLn . csv nn

putStrLnM :: (MonadIO m) => String -> m ()
putStrLnM = liftIO . putStrLn

-- | Export records -- currently on .csv format
exportAsCsv :: ListOptions -> RA ()
exportAsCsv opts = do
  (_, raw, noHeader) <- getListOptions opts
  unless noHeader $ putStrLnM csvHeader
  rp <- recordProcessing raw
  ns <- view notesL
  runSqlRIO $ exportAsCsvActions rp (ns ⋈ note.noteStr)

listHChunk :: HChunk Record_Poker -> IO ()
listHChunk (HChunk (Just header) body) =
  ""+|showRecord header|+"\n"+|unlines (map showRecord body)|+"\n"
listHChunk (HChunk Nothing body) = ""+|unlines (map showRecord body)|+"\n"

chunkActions :: ChunkProcessing -> SA ()
chunkActions cp = runConduitRes $ chunkSource cp .| sinkC listHChunk

-- | Chunk all records by header predicate
chunks :: MergeOptions -> RA ()
chunks opts = do
  (_, raw) <- getMergeOptions opts
  cp <- chunkProcessing raw
  runSqlRIO $ chunkActions cp

-- | Sum up all amounts
cashAmount :: Bool -> RA Money
cashAmount raw =
  runSqlRIO $
  if raw
    then sum "amount" (noFilters::[Filter Record_Poker]) -- typed for inferencing
    else runConduitRes $ rawRecordSource .| mapC ( ^. amount ) .| sumC

promptyN :: String -> IO Bool
promptyN str = isJustY <$> runInputT defaultSettings getWithPrompt
  where
    getWithPrompt = getInputChar $ str ++ " [y/N]: "
    isJustY = maybe False $ ( 'y' == ) . toLower

data AuthorizationResult = DryRunDidNotAsk | Confirmed | Denied | ForcedDidNotAsk
   deriving (Show, Eq)

authorized :: AuthorizationResult -> Bool
authorized(DryRunDidNotAsk) = False
authorized(Confirmed)       = True
authorized(Denied)          = False
authorized(ForcedDidNotAsk) = True

authorize :: Bool -> Force -> RA () -> RA AuthorizationResult
authorize verbose forced put = case forced of
  DryRun -> do
      put
      pure DryRunDidNotAsk
  Confirm -> do
      put
      answer <- liftIO $ promptyN "Update DB"
      pure $ if answer then Confirmed else Denied
  Forced -> do
      when verbose put
      pure ForcedDidNotAsk

success :: b -> RA (Either Text b)
success k = pure $ Right $ k

failure :: String -> RA (Either Text b)
failure msg = pure $ Left $ fromString msg

updateIfAuthorized
  :: Bool
     -> Force
     -> RA ()
     -> SA b
     -> RA (Either Text b)
updateIfAuthorized verbose forced putInsert update = do
    authorization <- authorize verbose forced putInsert
    if authorized authorization
        then success =<< runSqlRIO update
        else failure $ failureMsg authorization
  where
    failureMsg auth
        | auth == Denied = "\nOK, database not updated."
        | verbose        = "\nDatabase not updated"
        | otherwise      = ""

maybeNote ::
    ExplanatoryNote
 -> RA (Either Text b)
 -> (Record_NoteId -> RA ( Either Text b))
 -> RA (Either Text b)
maybeNote note' nothingAction justAction = do
    lookup' <- view lookupNoteIdL
    case lookup' note' of
      Just noteId -> justAction noteId
      Nothing     -> nothingAction

maybeActionRole ::
    Record_ActionId
 -> Record_RoleId
 -> RA (Either Text b)
 -> (Record_Action_RoleId -> RA ( Either Text b))
 -> RA (Either Text b)
maybeActionRole actionId roleId nothingAction justAction = do
    arid <- view aridL
    case lookup2 actionId roleId arid of
      Just action_RoleID -> justAction action_RoleID
      Nothing            -> nothingAction

insertRecord_Poker ::
    Bool
 -- ^ verbose
 -> Force
 -- ^ level of confirmation required
 -> Money
 -- ^ amount in new record
 -> Record_NoteId
 -- ^ note in new record
 -> Day
 -- ^ date in new record
 -> RA ( Either Text Record_PokerId )
insertRecord_Poker verbose forced amount' noteId date =
   updateIfAuthorized verbose forced putInsert insertRecord_PokerActions
  where
    dateStr' = showGregorian date
    insertRecord_PokerActions :: SA Record_PokerId
    insertRecord_PokerActions =
        insert $ Record_Poker date amount' noteId
    putInsert = do
        nidns <- view nidnsL
        liftIO $
          ""+||dateStr'||+" "+||amount'^.moneyInt||+" "+||nidns!noteId||+
          " ("+||forced||+")"

-- | Compute current cash, or update
cash :: CashOptions -> RA ()
cash (ShowCash raw) = liftIO . print =<< (^. moneyInt) <$> cashAmount raw
cash (UpdateCash raw forced a nId d) = do
    previousAmount <- cashAmount raw
    let change = a - previousAmount
    verbose <- view verboseL
    when verbose $
        liftIO $ "update from: "+||previousAmount||+
                 " by " +||change||+
                 " to " +||a||+""
    mKey <- insertRecord_Poker verbose forced change nId d
    case mKey of
      Right _ ->
          when verbose $ putStrLnM "insert succeeded"
      Left msg ->
          putStrLnM $ unpack msg

-- | Add a new record to the db
add ::
    AddOptions
 -- ^ The values for the new record
 -> RA ()
add (AddOptions forced a nId d) = do
    verbose <- view verboseL
    when verbose $ putStrLnM $ "add: "
    mKey <- insertRecord_Poker verbose forced a nId d
    case mKey of
      Right _ ->
          when verbose $ putStrLnM "insert succeeded"
      Left msg ->
          putStrLnM $ unpack msg

insertRecord_Note ::
    Bool
 -- ^ verbose
 -> Force
 -- ^ level of confirmation required
 -> ExplanatoryNote
 -- ^ note in new record
 -> Record_ActionId
 -- ^ action to be linked to by new record
 -> Record_RoleId
 -- ^ role to be linked to by new record
 -> RA ( Either Text ( Record_NoteId ) )
insertRecord_Note verbose forced n aId rId = do
    aidas <- view aidasL
    ridrs <- view ridrsL
    maybeNote n (checkActionRole aidas ridrs) failAlready
  where
    ns = n^.noteStr
    failAlready = const $ failure $ ""+||ns||+" is already in the database."
    checkActionRole aidas ridrs = maybeActionRole aId rId failActionRole update
      where
        as = aidas!aId
        rs = ridrs!rId
        failActionRole = failure $
            "Could not find "+||as||+" paired with "+||rs||+" in the database"
        update arId =
            updateIfAuthorized verbose forced putInsert insertNote
          where
            putInsert = liftIO $ ""+|ns|+": "+|as|+ ", "+|rs|+
                                 "  (" +||forced||+ ")\n"
            insertNote =  insert $ Record_Note n arId

-- | List the notes table
notes :: NotesOptions -> RA ()
notes (ShowNotes False) = do
    notesTable       <- view notesL
    mapM_ (liftIO . listNote) (elems notesTable)
notes (ShowNotes True) = do
    notesTable       <- view notesL
    listNoteEx       <- view listNoteExL
    mapM_ (liftIO . listNoteEx) (elems notesTable)
notes (NewNote force n aId rId) = do
    verbose <- view verboseL
    when verbose $ liftIO $ putStr "insert note: "
    mKey <- insertRecord_Note verbose force n aId rId
    case mKey of
      Right _  -> when verbose $ putStrLnM "insert succeeded"
      Left msg -> putStrLnM $ unpack msg
