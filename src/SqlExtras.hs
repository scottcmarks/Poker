{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE TypeFamilies        #-}


module SqlExtras( SqlActionT
                , SqlPersistEntity
                , noFilters
                , noSelectOpts
                )
where

import Control.Monad.Logger ( NoLoggingT )
import Control.Monad.Trans.Reader ( ReaderT )
import Control.Monad.Trans.Resource ( ResourceT )
import Database.Persist.Sql (
                              PersistEntity
                            , PersistEntityBackend
                            , BaseBackend
                            , SqlBackend
                            , Filter
                            , SelectOpt
                            )


class (
    PersistEntity record
  , PersistEntityBackend record ~ BaseBackend (BaseBackend SqlBackend)
  ) => SqlPersistEntity record


type SqlActionT m = ReaderT SqlBackend (NoLoggingT (ResourceT m))



-- Typed empty lists for type inferencing
noFilters :: [Filter record]
noFilters = []

noSelectOpts :: [SelectOpt record]
noSelectOpts = []
