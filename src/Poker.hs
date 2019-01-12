{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -dth-dec-file           #-}
{-# LANGUAGE TypeFamilies               #-}

module Poker
where


import RIO(Typeable,Show,Int,String)
import Fmt( (|+), (+|) )


import Database.Persist.Sql (fromSqlKey)
import Database.Persist.TH ( share
                           , mkPersist
                           , sqlSettings
                           , mkMigrate
                           , persistLowerCase
                           )
import SqlExtras ( SqlPersistEntity )

-- | Poker database schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Record_Action sql=actions
    action   String
    deriving (Show)
    deriving (Typeable)


Record_Note sql=notes
    note    String
    deriving (Show)
    deriving (Typeable)


Record_Type sql=types
    type    String
    deriving (Show)
    deriving (Typeable)


Record_Note_to_Action sql=note_to_action
    note_id   Record_NoteId
    action_id Record_ActionId
    deriving (Show)
    deriving (Typeable)


Record_Note_to_Type sql=note_to_type
    note_id   Record_NoteId
    type_id   Record_TypeId
    deriving (Show)
    deriving (Typeable)


Record_Poker sql=data
    date    String         -- dateString YYYY-MM-DD
    amount  Int            --        Int e.g. -100
    note_id Record_NoteId  --        Int 1,2, or 3 (at the moment)
    deriving (Show)
    deriving (Typeable)

|]


csv :: Record_Poker -> String
csv (Record_Poker date amount note_id)= ""+|date|+","+|amount|+","+|fromSqlKey note_id|+""

instance SqlPersistEntity Record_Action
instance SqlPersistEntity Record_Note
instance SqlPersistEntity Record_Type
instance SqlPersistEntity Record_Note_to_Action
instance SqlPersistEntity Record_Note_to_Type
instance SqlPersistEntity Record_Poker

