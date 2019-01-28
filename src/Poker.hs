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


import RIO(
    Typeable
  , Show
  , Int
  , String
  , Text
  , IO
  , map
  , ($)
  , unlines
  )

import Fmt(
    (|+)
  , (|++|)
  , (+|)
  , padLeftF
  )


import Database.Persist.Sql (
    fromSqlKey
  )

import Database.Persist.TH (
    share
  , mkPersist
  , sqlSettings
  , mkMigrate
  , persistLowerCase
  )

import SqlExtras (
    SqlPersistEntity
  )



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
    note_id Record_NoteId  --        id of note (normalized db)
    deriving (Show)
    deriving (Typeable)

|]


  -- Put a header for the listAllrecords cmd
listHeader :: Text -> Int -> IO ()
listHeader db n =
  let column_heads = "Date        Amount  Note#\n"
   in case n of
           0 ->  db|+" has no records!\n"
           1 ->  db|+" has one record:\n"     +|column_heads
           _ ->  db|+" has "+|n|+" records:\n"+|column_heads

-- Put a record for the listAllrecords cmd
showRecord :: Record_Poker -> String
showRecord (Record_Poker date amount note_id) =
  date|++|padLeftF 7 ' ' amount|++|padLeftF 6 ' ' (fromSqlKey note_id)

-- Put a record for the listAllrecords cmd
listRecord :: Record_Poker -> IO ()
listRecord r = ""+|showRecord r|+"\n"

listChunk :: [Record_Poker] -> IO ()
listChunk chunk = ""+|(unlines $ map showRecord chunk)|+"\n" 


-- Convert a record to csv format
csv :: Record_Poker -> String
csv (Record_Poker date amount note_id)= ""+|date|+","+|padLeftF 5 ' ' amount|+","+|padLeftF 3 ' ' (fromSqlKey note_id)|+""

instance SqlPersistEntity Record_Action
instance SqlPersistEntity Record_Note
instance SqlPersistEntity Record_Type
instance SqlPersistEntity Record_Note_to_Action
instance SqlPersistEntity Record_Note_to_Type
instance SqlPersistEntity Record_Poker
