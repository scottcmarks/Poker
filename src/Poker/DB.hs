{-|
Module      : Poker.DB
Description : @Poker.db@ schema record types, field lenses, field types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SQL Schema for @Poker.db@.
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


module Poker.DB
  ( -- * Field type defs
    ExplanatoryNote  (..) , noteStr
  , AccountingAction (..) , actionStr
  , Money            (..) , moneyInt
  , ProcessingRole   (..) , roleStr

    -- * Record constructors and 'Database.Persist.Sql.Key' @record@ defs
    -- and lenses
  , Record_Action(..)       , Record_ActionId
  , action
  , Record_Note(..)         , Record_NoteId
  , note, action_role_id
  , Record_Role(..)         , Record_RoleId
  , role
  , Record_Action_Role(..)  , Record_Action_RoleId
  , action_id, role_id
  , Record_Poker(..)        , Record_PokerId
  , date, amount, note_id

    -- * Generated migration routine
  , migrateAll
  ) where

import           Poker.DB.Schema (Record_Action (..), Record_ActionId,
                                  Record_Action_Role (..), Record_Action_RoleId,
                                  Record_Note (..), Record_NoteId,
                                  Record_Poker (..), Record_PokerId,
                                  Record_Role (..), Record_RoleId, action,
                                  action_id, action_role_id, amount, date,
                                  migrateAll, note, note_id, role, role_id)
import           Poker.DB.Types  (AccountingAction (..), ExplanatoryNote (..),
                                  Money (..), ProcessingRole (..), actionStr,
                                  moneyInt, noteStr, roleStr)
