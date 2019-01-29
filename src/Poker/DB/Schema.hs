{-|
Module      : Poker.DB.Schema
Description : @Poker.db@ schema in TH, using Haskell field types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SQL Schema for @Poker.db@.
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -dth-dec-file           #-}


module Poker.DB.Schema
  ( -- * Record constructors and lenses
    Record_Action(..)         , Record_ActionId
  , action
  , Record_Note(..)           , Record_NoteId
  , note, action_role_id
  , Record_Role(..)           , Record_RoleId
  , role
  , Record_Action_Role(..)    , Record_Action_RoleId
  , action_id, role_id
  , Record_Poker(..)          , Record_PokerId
  , date, amount, note_id
    -- * Generated migration routine
  , migrateAll
  ) where

import           Data.Time.Calendar  (Day (..))
import           Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses,
                                      mpsPrefixFields, persistLowerCase, share,
                                      sqlSettings)
import           RIO                 (Bool (..), Show, Typeable)

import           Poker.DB.Types      (AccountingAction (..),
                                      ExplanatoryNote (..), Money (..),
                                      ProcessingRole (..))

-- | Poker database schema
--
share
  [ mkPersist sqlSettings{ mpsGenerateLenses = True
                         , mpsPrefixFields = False
                         }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|

-- | Action table record
Record_Action sql=actions
    action   AccountingAction  -- String
    deriving (Show)
    deriving (Typeable)

-- | Role table record
Record_Role sql=roles
    role    ProcessingRole  -- String
    deriving (Show)
    deriving (Typeable)

-- | Action_Role table record, action_id x role_id pairs
Record_Action_Role sql=action_role
    action_id   Record_ActionId
    role_id     Record_RoleId
    deriving   (Show)
    deriving   (Typeable)

-- | Note table record
Record_Note sql=notes
    note    ExplanatoryNote  -- String
    action_role_id Record_Action_RoleId
    deriving (Show)
    deriving (Typeable)


-- | Main data table record
Record_Poker sql=data
    date    Day            -- dateString YYYY-MM-DD
    amount  Money          --        Int e.g. -100
    note_id Record_NoteId  --        id of note (normalized db)
    deriving (Show)
    deriving (Typeable)

|]
