{-|
Module      : Poker.Driver.Process
Description : Process the Poker DB record stream, including header chunking.
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Define processing by chunks.
-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Poker.Driver.Process
  ( HChunk(..)
  , rawHChunkSource
  , processedHChunkSource
  , rawRecordSource
  , processedRecordSource
  , actionSource
  , roleSource
  , actionRoleSource
  , noteSource
  , fuzzed
  ) where

import           RIO                      hiding (concat)

import           Conduit                  (mapC, (.|))
import           Control.Lens.Combinators (from)
import           Control.Lens.Operators   ((+~))
import           Data.Conduit.Combinators (concat)
import           Data.Time.Calendar       (Day, fromGregorian)
import           Database.Persist.Sql     (toSqlKey)

import           Data.Conduit.HChunk      (HChunk (..), takeHChunksC)
import           Poker.App                (CRSA, Money, Record_Action (..),
                                           Record_Action_Role (..),
                                           Record_Note (..), Record_NoteId,
                                           Record_Poker (..), Record_Role (..),
                                           amount, moneyInt, note_id)




import           Poker.Driver.Schema      (recordSource)

fuzzedDate :: Day
fuzzedDate = fromGregorian 9999 99 99

fuzzedAmount :: Money
fuzzedAmount = -999 ^. (from moneyInt)

fuzzedNoteKey :: Record_NoteId
fuzzedNoteKey = toSqlKey (-9999)

isFuzzedRole :: Record_NoteId -> Bool
isFuzzedRole = (fuzzedNoteKey ==)

isFuzzed :: Record_Poker -> Bool
isFuzzed = isFuzzedRole . view note_id

-- | Special fuzz record for testing
--
fuzzed :: Record_Poker
fuzzed = Record_Poker fuzzedDate fuzzedAmount fuzzedNoteKey

-- | HChunk merging algorithm
--
mergeAccum ::
     (Record_Poker -> Bool)
  -- ^ "merged" record predicate
  -> Record_Poker
  -- ^ foldr item
  -> (Record_Poker, [Record_Poker])
  -- ^ foldr accumulator  (header, body)
  -> (Record_Poker, [Record_Poker])
mergeAccum isMerged r (h, b)
    | isFuzzed r = error "fuzz record for error testing\n"
    | isMerged r = (h & amount +~ r ^. amount,     b)
    | otherwise  = (h                        , r : b)

-- | Process HChunks, merging "merged" records into the "header" record, if any
mergeHChunksC ::
     (Record_Poker -> Bool)
  -- ^ "merged" record predicate
  -> CRSA (HChunk Record_Poker) (HChunk Record_Poker) ()
mergeHChunksC isMerged = mapC merge
  where
    merge hChunk =
      case hChunk of
        (HChunk (Just header) body) ->
          case foldr (mergeAccum isMerged) (header, []) body of
            (h, b) -> HChunk (Just h) b
        (HChunk Nothing _) -> hChunk

-- | Supply the record type for type inferencing
rawRecordSource :: CRSA () Record_Poker ()
rawRecordSource = recordSource

-- | Pass the records from the raw DB records source
--   through a chunking conduit parameterized by a
--   header-record-selecting predicate
rawHChunkSource ::
     (Record_Poker -> Bool)
  -- ^ Header record predicate
  -> CRSA () (HChunk Record_Poker) ()
rawHChunkSource isHeader = rawRecordSource .| takeHChunksC isHeader

-- | Pass the chunks from the raw chunk source
--   through a processing conduit parameterized by a
--   merged-record-selecting predicate
processedHChunkSource ::
     (Record_Poker -> Bool)
  -- ^ Header record predicate
  -> (Record_Poker -> Bool)
  -- ^ Meged record predicate
  -> CRSA () (HChunk Record_Poker) ()
processedHChunkSource isHeader isMerged =
  rawHChunkSource isHeader .| mergeHChunksC isMerged

-- | Pass the chunks from the processed chunk source
--   through a flattening conduit to produce
--   a processed record stream
processedRecordSource ::
     (Record_Poker -> Bool)
  -- ^ Header record predicate
  -> (Record_Poker -> Bool)
  -- ^ Merged record predicate
  -> CRSA () Record_Poker ()
processedRecordSource isHeader isMerged =
  processedHChunkSource isHeader isMerged .| mapC yieldChunk .| concat
  where
    yieldChunk (HChunk header body) = maybeToList header ++ body

-- | Source of the contents of the @notes@ table
noteSource :: CRSA () Record_Note ()
noteSource = recordSource

-- | Source of the contents of the @roles@ table
roleSource :: CRSA () Record_Role ()
roleSource = recordSource

-- | Source of the contents of the @actions@ table
actionSource :: CRSA () Record_Action ()
actionSource = recordSource

-- | Source of the contents of the @action_role@ table
actionRoleSource :: CRSA () Record_Action_Role ()
actionRoleSource = recordSource
