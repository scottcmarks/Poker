{-|
Module      : Data.Conduit.HChunk.Types
Description : Generic header chunk type for chunking conduit.
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Generic header chunk type for chunking conduit.
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Conduit.HChunk.Types
  ( HChunk(HChunk)
  )
where

import           RIO (Eq, Maybe, Show)

-- | Chunks with optional header
--
data HChunk r =  HChunk
    { header :: Maybe r
    , body   :: [r]
    }
  deriving (Eq,Show)
