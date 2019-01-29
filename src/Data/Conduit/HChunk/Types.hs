{-|
Module      :  Data.Conduit.HChunk.Types
Description : Executable main for poker
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Generic header chunk type for chunking conduit.
-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Conduit.HChunk.Types
  ( HChunk(..)
  )
where

import           RIO (Maybe, Show)

-- | Chunks with optional header
--
data HChunk r = HChunk
    { hChunkHeader :: Maybe r
    , hChunkBody   :: [r]
    } deriving (Show)
