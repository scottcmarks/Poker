{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Run
where

import RIO
import Control.Monad.IO.Class (liftIO)
import Conduit ((.|), runConduitRes )
import Data.Conduit.Combinators ( lengthE )
import Database.Persist.Sql ( Filter, count )
import Fmt ( (|+), (+|) )
import System.IO ( putStrLn )

import ConduitExtras ( sinkC )
import SqlExtras ( noFilters )
import SqlRIO ( SqlRIO )


import App
import CRSA
import Poker
import Process

getOptions :: MergeOptions -> RIO App (Bool, Bool)
getOptions opts = do
  verbose <- view verboseL
  let raw = processOptionsRaw opts
  when verbose $ logInfo $ fromString $ "raw=" ++ show raw
  return (verbose, raw)

chunkSource :: Bool -> CRSA () [Record_Poker] ()
chunkSource raw = do
  isHeader <- isType "header"
  if raw
  then do rawChunkSource       isHeader
  else do isMerged <- isType "merged"
          processedChunkSource isHeader isMerged 

pokerRecordSource :: Bool -> CRSA () Record_Poker ()
pokerRecordSource raw =
  if raw
  then do rawRecordSource
  else do isHeader <- isType "header"
          isMerged <- isType "merged"
          processedRecordSource isHeader isMerged


countActions :: Bool -> SqlRIO App Int
countActions raw =
  if raw
  then count (noFilters :: [Filter Record_Poker])    -- ask the db
  else runConduitRes $ chunkSource False .| lengthE  -- count chunk elements

countRecords :: MergeOptions -> RIO App ()
countRecords opts = do
  (_, raw) <- getOptions opts
  n <- runSqliteActions $ countActions raw
  liftIO $ ""+|n|+"\n"


-- List all records, with header
listActions :: Bool -> (Record_Poker -> IO ()) -> SqlRIO App ()
listActions raw putRecord = runConduitRes $ pokerRecordSource raw .| sinkC putRecord

list :: MergeOptions -> RIO App ()
list opts = do
  (_, raw) <- getOptions opts
  dbFilePath <- view dbFilePathL
  runSqliteActions $ do
    n <- countActions raw
    liftIO $ listHeader dbFilePath n
    listActions raw listRecord


-- export all records in csv file format
exportActions :: Bool -> SqlRIO App ()
exportActions raw = listActions raw (putStrLn . csv)

exportAsCsv :: MergeOptions -> RIO App ()
exportAsCsv opts = do
  (_, raw) <- getOptions opts
  runSqliteActions $ exportActions raw


chunkActions :: Bool -> SqlRIO App ()
chunkActions raw = runConduitRes $ chunkSource raw .| sinkC listChunk

-- Chunk all records by header predicate
chunks :: MergeOptions -> RIO App ()
chunks opts = do
  (_, raw) <- getOptions opts
  runSqliteActions $ chunkActions raw



-- dispatch to runCmd, with debugging logging when verbose
run :: RIO App () -> RIO App ()
run runCmd = do
  AppOptions { appOptionsVerbose    = verbose
             , appOptionsDbFilePath = dbFilePath
             } <- view optionsL
  when verbose $ do
    logInfo $ fromString $ "dbFilePath=" ++ dbFilePath
  runCmd
