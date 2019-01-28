{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Process
where

import RIO hiding (concat)


import Conduit ( (.|), mapC, sinkList )

import Data.Conduit.Combinators( concat )  
import Data.List ( find )
import Data.Map ( fromList, (!) )
import Data.Maybe ( fromJust )
import Database.Persist.Sql (
    Entity
  , entityVal
  , entityKey
  , toSqlKey
  )

import Fmt ( (|+), (+|) )


import ConduitExtras ( takeChunksC )

import CRSA
import Poker
import SqlExtras



buildDbAlist :: SqlPersistEntity record => (Entity record -> (k, v)) ->
                                           CRSA () c [(k, v)]
buildDbAlist pairFn = sqlSource .| mapC pairFn .| sinkList

noteKeyToTypeKeyAlist :: CRSA () c [(Key Record_Note, Key Record_Type)]
noteKeyToTypeKeyAlist = buildDbAlist $
  entityVal >>> record_Note_to_TypeNote_id &&& record_Note_to_TypeType_id

typeToTypeKeyAlist :: CRSA () c [(String, Key Record_Type)]
typeToTypeKeyAlist = buildDbAlist $
  record_TypeType . entityVal &&& entityKey
                         
isType :: String -> CRSA () c (Record_Poker -> Bool)
isType s = do
  ntMap <- fromList <$>  noteKeyToTypeKeyAlist
  ttkal <-               typeToTypeKeyAlist
  let
    record_type_id = (ntMap !) . record_PokerNote_id
    type_id = snd $ fromJust $ find ((== s) . fst) ttkal
  pure ((type_id == ) . record_type_id)
  
fuzzedNoteKey :: Key Record_Note
fuzzedNoteKey = toSqlKey (negate 999999)

isFuzzedType :: Key Record_Note -> Bool
isFuzzedType = (fuzzedNoteKey ==)

isFuzzed :: Record_Poker -> Bool
isFuzzed = isFuzzedType . record_PokerNote_id

fuzzed :: Record_Poker
fuzzed = (Record_Poker undefined undefined fuzzedNoteKey)

mergeAccum :: (Record_Poker -> Bool) ->
              (Record_Poker -> Bool) ->
              Record_Poker -> (Int, [Record_Poker]) -> (Int, [Record_Poker])
mergeAccum isHeader isMerged r@(Record_Poker _ amount _) (total, res)
  | isFuzzed r = error "fuzz record for error testing\n"
  | isHeader r = (0             , r { record_PokerAmount = (total+amount)} : res)
  | isMerged r = ((total+amount),                                            res)
  | otherwise  = (total         , r                                        : res)

merge :: (Record_Poker -> Bool) ->
         (Record_Poker -> Bool) ->
         [Record_Poker] -> [Record_Poker]
merge isHeader isMerged records =
  case foldr (mergeAccum isHeader isMerged) (0, []) records
    of (0  , res) -> res
       (tot, res) -> error $ "Unmerged adjustment tot="+|tot|+"\nres:\n"+|unlines(map showRecord res)|+ "\n"   -- fail if unmerged total
  
  
rawRecordSource :: CRSA () Record_Poker ()
rawRecordSource = recordSource

mergeChunksC :: (Record_Poker -> Bool) ->
                (Record_Poker -> Bool) ->
                CRSA [Record_Poker] [Record_Poker] ()
mergeChunksC isHeader isMerged = mapC (merge isHeader isMerged)


rawChunkSource :: (Record_Poker -> Bool) ->
               CRSA () [Record_Poker] ()
rawChunkSource isHeader =
  rawRecordSource .| takeChunksC isHeader


processedChunkSource :: (Record_Poker -> Bool) ->
                        (Record_Poker -> Bool) ->
                         CRSA () [Record_Poker] ()
processedChunkSource isHeader isMerged =
  rawChunkSource isHeader .| mergeChunksC isHeader isMerged


processedRecordSource :: (Record_Poker -> Bool) ->
                         (Record_Poker -> Bool) ->
                         CRSA () Record_Poker ()
processedRecordSource isHeader isMerged =
  processedChunkSource isHeader isMerged .| concat
