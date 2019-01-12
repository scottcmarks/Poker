{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}


module SqlRIO
where

import RIO

import Conduit ( ConduitT , mapC )
import Database.Persist.Sql ( Entity, entityVal )
import SqlExtras ( SqlActionT )


----- Conduit + Sql(ite) -----

recordPeeler :: (MonadIO m) => ConduitT (Entity b) b m ()
recordPeeler = mapC entityVal


----- SqlExtras + RIO -----

type SqlRIO env = SqlActionT (RIO env)
