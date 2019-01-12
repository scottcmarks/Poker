module ConduitExtras ( takeChunksC
                     , idC
                     , sinkC
                     )

where

import Conduit

takeWhenC :: (Monad m) => (i -> Bool) -> ConduitT i i m ()
takeWhenC f = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x -> if f x then yield x else leftover x

takeUntilC :: (Monad m) => (i -> Bool) -> ConduitT i i m ()
takeUntilC f = takeWhileC (not.f)

takeChunkC :: (Monad m) => (a -> Bool) -> ConduitT a a m ()
takeChunkC f = takeWhenC f >> takeUntilC f

takeChunksC :: (Monad m) => (a -> Bool) -> ConduitT a [a] m ()
takeChunksC f = loop
  where
    loop = do
      chunk <- takeChunkC f .| sinkList
      case chunk of [] -> return ();  _ -> do yield chunk ; loop

idC :: (Monad m) => ConduitT a a m ()
idC = mapC id

-- Generic record list actions
sinkC :: MonadIO m => (a -> IO ()) -> ConduitT a Void m ()
sinkC put = mapM_C $ liftIO.put

