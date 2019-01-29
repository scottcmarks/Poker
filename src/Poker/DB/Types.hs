{-|
Module      : Poker.DB.Types
Description : @Poker.db@ schema record types, field lenses, field types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Record Field datatypes for @Poker.db@.
Record constructors, field lenses,  and some field type definitions.
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -dth-dec-file           #-}


module Poker.DB.Types
  ( -- * Field type defs
    ExplanatoryNote  (..) , noteStr
  , AccountingAction (..) , actionStr
  , Money            (..) , moneyInt
  , ProcessingRole   (..) , roleStr
  ) where

import           Control.Lens         (makeLenses)
import           Database.Persist.Sql (PersistField, PersistFieldSql)
import           RIO                  (Eq, Int, Num, Ord, Read, Show, String)

-- | Action fields
--

newtype AccountingAction = AccountingAction
  {
 -- | Convert AccountingAction to String
    _actionStr :: String
  } deriving (Eq, Ord, Read, Show, PersistField, PersistFieldSql)
makeLenses ''AccountingAction

-- | Amount fields
--
newtype Money = Money
  {
 -- | Convert Money to Int
    _moneyInt :: Int
  } deriving (Eq, Num, Ord, Read, Show, PersistField, PersistFieldSql)
makeLenses ''Money

-- | ExplanatoryNote fields
--
newtype ExplanatoryNote = ExplanatoryNote
  {
 -- | Convert ExplanatoryNote to String
    _noteStr :: String
  } deriving (Eq, Ord, Read, Show, PersistField, PersistFieldSql)
makeLenses ''ExplanatoryNote

-- | Role fields
--
newtype ProcessingRole = ProcessingRole
  {
 -- | Convert ProcessingRole to String
    _roleStr :: String
  } deriving (Eq, Ord, Read, Show, PersistField, PersistFieldSql)
makeLenses ''ProcessingRole
