{-|
Module      : Data.Conduit.HChunk.Internal
Description : Executable main for poker
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Chunking conduit internal module
-}

module Data.Conduit.HChunk.Internal
  ( takeHChunksC
  ) where

import           Conduit                   (ConduitT, await, leftover,
                                            peekForever, sinkList, takeWhileC,
                                            yield, (.|))

import           Data.Conduit.HChunk.Types (HChunk (..))

-- | Process the input, collecting items into chunks
--   Headers separate chunks and are included with the following items
takeHChunksC ::
     Monad m
  => (a -> Bool)
  -- ^ test the item for being a header
  -> ConduitT a (HChunk a) m ()
takeHChunksC p = peekForever takeHChunkC
  where
    takeHChunkC = HChunk <$> header <*> body >>= yield
      where
        header = await >>= maybe (return Nothing) go
          where
            go x =
              if p x
                then return $ Just x
                else leftover x >> return Nothing
        body = takeWhileC (not . p) .| sinkList
