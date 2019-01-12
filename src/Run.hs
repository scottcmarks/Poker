{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}


module Run
where

import App
import RIO


import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Control.Monad.Trans.Resource ( ResourceT, MonadUnliftIO )
import Conduit ( ConduitT
               , Void
               , (.|)
               , runConduitRes
               , sinkList
               , mapC
               , awaitForever
               )
import ConduitExtras (
                       idC
                     , takeChunksC
                     , sinkC
                     )
import Data.List ( find )
import Data.Map ( fromList, (!) )
import Database.Persist.Sql (
                              Entity
                            , Filter
                            , entityVal
                            , entityKey
                            , fromSqlKey
                            , selectSource
                            , count
                            , runMigration
                            )
import Database.Persist.Sqlite ( runSqlite )
import Fmt ( (|+)
           , (|++|)
           , (+|)
           ,  padLeftF
           )
import SqlExtras ( SqlActionT
                 , SqlPersistEntity
                 , noFilters
                 , noSelectOpts
                 )
import SqlRIO ( recordPeeler
              , SqlRIO
              )
import System.IO ( putStrLn )


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

recordSource ::  (SqlPersistEntity record) => CRSA () record ()
recordSource = sqlSource .| recordPeeler



listActions ::  (Record_Poker -> IO ()) -> SqlRIO env ()
listActions putRecord =
  let source = selectSource noFilters noSelectOpts
      sink   = sinkC putRecord
   in runConduitRes $ source .| recordPeeler .| sink


-- Put a header for the listAllrecords cmd
listHeader :: Text -> Int -> IO ()
listHeader db n =
  let column_heads = "Date        Amount  Note#\n"
   in case n of
           0 ->  db|+" has no records!\n"
           1 ->  db|+" has one record:\n"     +|column_heads
           _ ->  db|+" has "+|n|+" records:\n"+|column_heads


-- Put a record for the listAllrecords cmd
showRecord :: Record_Poker → String
showRecord (Record_Poker date amount note_id) =
  date|++|padLeftF 7 ' ' amount|++|padLeftF 6 ' ' (fromSqlKey note_id)

-- Put a record for the listAllrecords cmd
listRecord :: Record_Poker → IO ()
listRecord r = ""+|showRecord r|+"\n"

-- List all records, with header
listAllRecords :: ListOptions -> RIO App ()
listAllRecords opts = do
  verbose <- view verboseL
  dbFilePath <- view dbFilePathL
  let raw = listOptionsRaw opts
  when verbose $ do
    logInfo $ fromString $ "raw=" ++ show raw  -- FIXME: ??  "raw="+|raw|+""  ??
  runSqliteActions $ do
    n <- count ([] :: [Filter Record_Poker])  -- type needed
    liftIO $ listHeader dbFilePath n
    listActions $ listRecord


-- export all records in csv file format
exportAsCsv :: RIO App ()
exportAsCsv = runSqliteActions $ listActions (putStrLn . csv)


countRecords :: RIO App ()
countRecords = runSqliteActions $ do
  n <- count ([] :: [Filter Record_Poker])  -- type needed
  liftIO $ ""+|n|+"\n"

processChunksC :: CRSA [Record_Poker] [Record_Poker] ()
processChunksC = idC                                   -- FIXME: stub

listChunk :: [Record_Poker] -> IO ()
listChunk chunk = mapM_ listRecord chunk >> putStrLn ""

listChunksC :: (MonadIO m) => ConduitT [Record_Poker] Void m ()
listChunksC =  awaitForever $ liftIO . listChunk

noteKeyToTypeKeyMap  :: SqlRIO App (Map (Key Record_Note) (Key Record_Type))
noteKeyToTypeKeyMap = 
  let
    pairRNT (Record_Note_to_Type nid tid) = (nid, tid)
    ntpl = recordSource .| mapC pairRNT .| sinkList
   in fromList <$> runConduitRes ntpl :: SqlActionT (RIO App) (Map (Key Record_Note) (Key Record_Type))

isTypeKey :: String -> SqlRIO App (Record_Poker -> Bool)
isTypeKey s = do
    ntMap <- noteKeyToTypeKeyMap
    let
      record_PokerType_id = ((ntMap !) . record_PokerNote_id) :: Record_Poker -> Key Record_Type
      etTypeSource = sqlSource .| mapC (\e -> (record_TypeType $ entityVal e, entityKey e))

    etpl <- runConduitRes $ etTypeSource .| sinkList
    let
      type_idKey st = maybe (error ("No key for type " ++ s)) snd
                           (find ((== st) .fst) etpl)
    pure ((type_idKey s == ) . record_PokerType_id)


chunkActions :: SqlRIO App ()
chunkActions = do
    isHeader <- isTypeKey "header"
    runConduitRes $ recordSource
                   .| takeChunksC isHeader
                   .| processChunksC
                   .| listChunksC

-- Chunk all records, with header
chunkRecords :: RIO App ()
chunkRecords = do
  verbose <- view verboseL
  when verbose $ logInfo "verbose"
  runSqliteActions chunkActions


-- dispatch to runCmd, with debugging logging when verbose
run :: RIO App () -> RIO App ()
run runCmd = do
  AppOptions { appOptionsVerbose    = verbose
             , appOptionsDbFilePath = dbFilePath
             } <- view optionsL
  when verbose $ do
    logInfo $ fromString $ "dbFilePath=" ++ dbFilePath
  runCmd
