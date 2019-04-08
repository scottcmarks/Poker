{-|
Module      : Poker.Driver.CommandLine
Description : Command line parsing to drive executable main for poker
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Parsing of command line global settings, command, and command flags and arguments.

Export enough to move toward batteries included.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs       #-}


module Poker.Driver.CommandLine
  ( getAppAndCommand
  ) where

import           Control.Arrow                   (left)
import           Control.Monad.Trans.Except      (ExceptT)
import           Control.Monad.Trans.Writer      (Writer)
import           Data.Dates                      (parseDay)
import           Data.Map                        ((!))
import           Data.Time.Calendar              (Day (..))
import           Data.Time.Clock                 (getCurrentTime, utctDay)
import           Options.Applicative.Common      (Parser)
import           Options.Applicative.Simple      (CommandFields, Mod,
                                                  addCommand, argument, auto,
                                                  eitherReader, flag', help,
                                                  hidden, internal, long,
                                                  metavar, short, showDefault,
                                                  simpleVersion, strArgument,
                                                  strOption, switch, value)
import           RIO                             (Bool (..), IO, Map, String,
                                                  Text, pure, show, ($), (++),
                                                  (.), (<$>), (<*>), (<>),
                                                  (<|>), (^.))

import           Options.Applicative.PokerExtras (mapArgument,
                                                  optionsAndCommand)
import qualified Paths_Poker                     (version)
import           Poker.App                       (AddOptions (..), App,
                                                  AppOptions (..),
                                                  CashOptions (..),
                                                  ExplanatoryNote (..),
                                                  Force (Confirm, DryRun, Forced),
                                                  ListOptions (..),
                                                  MergeOptions (..), Money (..),
                                                  NotesOptions (..), RA,
                                                  Record_ActionId,
                                                  Record_NoteId, Record_RoleId,
                                                  asaidL, nidnsL, nsnidL,
                                                  rsridL)
import           Poker.Driver.Run                (add, cash, chunks,
                                                  countRecords, exportAsCsv,
                                                  fromAppOptions,
                                                  getDefaultNoteIdFromDB, list,
                                                  notes)

-- | Main command line parser
--
getAppAndCommand :: IO (App, RA ())
getAppAndCommand = do
    (opts, cmd) <-
        optionsAndCommand pokerVersion pokerHeader pokerDesc appOptions cmds
    app <- fromAppOptions opts
    pure (app,cmd)
  where
    cmds opts = do
        defaultNoteId <- getDefaultNoteIdFromDB opts
        today <- getToday
        app' <- fromAppOptions opts
        let
            asaid = app' ^. asaidL
            rsrid = app' ^. rsridL
            nsnid = app' ^. nsnidL
            nidns = app' ^. nidnsL
        pure $ commands nsnid nidns defaultNoteId today asaid rsrid

-- | Version string
pokerVersion :: String
pokerVersion = "Poker " ++ $(simpleVersion Paths_Poker.version)

-- | Header for command line arguments
pokerHeader :: String
pokerHeader =
    pokerVersion <> " -- © 2019 Magnolia Heights R&D.  All rights reserved."

-- | Program description, also for command line arguments
pokerDesc :: String
pokerDesc =
    "Operate on poker database. \
    \For more information on <COMMAND>, try <COMMAND> --help"

-- | Global arguments, processed before the commands
appOptions :: Parser AppOptions
appOptions = AppOptions <$> verboseOption <*> connStrOption

-- | Simple utility to get today's date as Day
getToday :: IO Day
getToday = utctDay <$> getCurrentTime

-- | Monad transformer stack for commands in parser over @b@
--
type EWMC b = ExceptT b (Writer (Mod CommandFields b))

-- | Monad transformer stack for commands in 'RA' @a@
--
type ERA = EWMC (RA ())

-- | Generate a parser for commands, specialized with
--   values to be used as defaults for some commands
--
commands ::
    Map String Record_NoteId
 -- ^ map note string to note id
 -> Map Record_NoteId String
 -- ^ map note id to note string
 -> Record_NoteId
 -- ^ default Note id
 -> Day
 -- ^ default Date (today)
 -> Map String Record_ActionId
 -- ^ inv (actionsTable ^. action.actionStr)
 -> Map String Record_RoleId
 -- ^ inv (rolesTable ^. role.roleStr)
 -> ERA ()
commands nsnid nidns defaultNoteId defaultDate asaid rsrid = do
    listCommands
    processCommands
    updateCommands nsnid nidns defaultNoteId defaultDate
    notesCommand asaid rsrid

-- | Commands for listing the database contents
--
listCommands :: ERA ()
listCommands = do
    addCommand
        "list"
        "List all database records"
        list
        listOptions
    addCommand
        "export"
        "Export all records in .csv format"
        exportAsCsv
        listOptions

-- | Commands for other access to the database contents for debugging
--
processCommands :: ERA ()
processCommands = do
    addCommand
        "count" -- for debugging
        "Print the count of all records"
        countRecords
        mergeOptions
    addCommand
        "chunks" -- for debugging
        "Gather the records into chunks"
        chunks
        mergeOptions

-- | Commands that can modify the database contents
--
updateCommands ::
    Map String Record_NoteId
 -- ^ map note string to note id
 -> Map Record_NoteId String
 -- ^ map note id to note string
 -> Record_NoteId
 -- ^ Default note value
 -> Day
 -- ^ Default date value
 -> ERA ()
updateCommands nsnid nidns defaultNoteId defaultDate = do
    addCommand
        "add"
        "Add new record"
        add
        (addRecordArguments nsnid nidns defaultNoteId defaultDate)
    addCommand
        "cash"
        "Show current cash amount, or update to new amount"
        cash
        (cashOptions nsnid nidns defaultNoteId defaultDate)

-- | Commands for listing the database contents
--
notesCommand ::
    Map String Record_ActionId
 -- ^ Lookup @action@ 'String' to find 'Record_ActionId'
 -> Map String Record_RoleId
 -- ^ Lookup @role@ 'String' to find 'Record_RoleId'
 -> ERA ()
notesCommand asaid rsrid =
    addCommand
        "notes"
        "List the notes table records, or add a new note"
        notes
        (notesOptions asaid rsrid)

-- | simple Parsers used for options and commands
--
-- | global options
--
verboseOption :: Parser Bool
verboseOption =
    switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output?"
      )

connStrOption :: Parser Text
connStrOption =
    strOption
      ( long "connStr"
     <> short 'c'
     <> help "Database connection string? (might be just file path)"
     <> metavar "DB"
     <> value "Poker.db"
     <> showDefault
      )

-- | listing options
--
noHeaderOption :: Parser Bool
noHeaderOption =
    switch
      ( long "no-header"
     <> short 'H'
     <> help "no header line in output?"
      )

rawOption :: Parser Bool
rawOption =
    switch
      ( long "raw"
     <> short 'r'
     <> help "raw (unmerged) output?"
     <> hidden
     <> internal
      )

showActionRoleOption :: Parser Bool
showActionRoleOption =
    switch
      ( long "actions-and-roles"
     <> short 'a'
     <> help "show action and role"
      )

forceOption :: Parser Force
forceOption =
    flag' DryRun  (long "dry-run" <> short 'd' <>
                   help "do not update DB"               ) <|>
    flag' Confirm (long "confirm" <> short 'c' <>
                   help "confirm DB update"              ) <|>
    flag' Forced  (long "force"   <> short 'f' <>
                   help "update DB without confirmation" ) <|>
    pure Confirm -- the default value


negativeArgumentNote :: String
negativeArgumentNote =
    "  negative in (), e.g. (-12) \
    \  -- may need to be quoted, e.g. for bash as '(-12)'"

help' :: String -> Mod f a
help' = help . (++ negativeArgumentNote)

newAmountArgument :: Parser Money
newAmountArgument =
    Money <$> argument auto
      ( help' "amount in new record?"
     <> metavar "AMOUNT"
      )

cashAmountArgument :: Parser Money
cashAmountArgument =
    Money <$> argument auto
      ( help' "amount in cash?"
     <> metavar "AMOUNT"
      )

optionalNoteIdArgument ::
    Map String Record_NoteId
 -- ^ map from note string to note id
 -> Map Record_NoteId String
 -- ^ map from note id to note string
 -> Record_NoteId
 -- ^ Default note Id
 -> Parser Record_NoteId
optionalNoteIdArgument nsnid nidns defaultNoteId =
    mapArgument nsnid
      ( help "note in new record?"
     <> metavar "NOTE"
     <> value (nidns ! defaultNoteId)
     <> showDefault
      )

optionalDateArgument ::
    Day
 -- ^ Default date value
 -> Parser Day
optionalDateArgument defaultDate =
    argument (auto <|> relativeDayReader)
      ( help "date in new record?"
     <> metavar "YYYY-MM-DD"
     <> value defaultDate
     <> showDefault
      )
  where
    relativeDayReader = eitherReader $ left show . parseDay defaultDate

requiredNewNoteArgument :: Parser ExplanatoryNote
requiredNewNoteArgument =
    ExplanatoryNote <$> strArgument
      ( help "new note?"
     <> metavar "NOTE"
      )

optionalActionArgument ::
    Map String Record_ActionId
 -- ^ inv ActionsTable ^. actionStr
 -> Parser Record_ActionId
optionalActionArgument asaid =
    mapArgument asaid
      ( help "new action?"
     <> metavar "ACTION"
     <> value "Profit"
     <> showDefault
      )

optionalRoleArgument ::
    Map String Record_RoleId
 -- ^ inv RolesTable ^. roleStr
 -> Parser Record_RoleId
optionalRoleArgument rsrid =
    mapArgument rsrid
      ( help "new role?"
     <> metavar "ROLE"
     <> value "regular"
     <> showDefault
      )

mergeOptions :: Parser MergeOptions
mergeOptions = MergeOptions <$> rawOption

showCashOptions :: Parser CashOptions
showCashOptions = ShowCash <$> rawOption

updateCashOptions ::
    Map String Record_NoteId
 -- ^ map note string to note id
 -> Map Record_NoteId String
 -- ^ map note id to note string
 -> Record_NoteId
 -- ^ Default note id
 -> Day
 -- ^ Default date value
 -> Parser CashOptions
updateCashOptions nsnid nidns defaultNoteId defaultDate =
    UpdateCash
    <$> rawOption
    <*> forceOption
    <*> cashAmountArgument
    <*> optionalNoteIdArgument nsnid nidns defaultNoteId
    <*> optionalDateArgument defaultDate

cashOptions ::
    Map String Record_NoteId
 -- ^ map note string to note id
 -> Map Record_NoteId String
 -- ^ map note id to note string
 -> Record_NoteId
 -- ^ Default note id
 -> Day
 -- ^ Default date value
 -> Parser CashOptions
cashOptions nsnid nidns defaultNoteId defaultDate =
    showCashOptions <|> updateCashOptions nsnid nidns defaultNoteId defaultDate

addRecordArguments ::
    Map String Record_NoteId
 -- ^ map note string to note id
 -> Map Record_NoteId String
 -- ^ map note id to note string
 -> Record_NoteId
 -- ^ Default note id
 -> Day
 -- ^ Default date value
 -> Parser AddOptions
addRecordArguments nsnid nidns defaultNoteId defaultDate =
    AddOptions
    <$> forceOption
    <*> newAmountArgument
    <*> optionalNoteIdArgument nsnid nidns defaultNoteId
    <*> optionalDateArgument defaultDate

listOptions :: Parser ListOptions
listOptions =
    ListOptions
    <$> noHeaderOption
    <*> rawOption

showNotesOption :: Parser NotesOptions
showNotesOption =
    ShowNotes
    <$> showActionRoleOption

newNoteOption ::
    Map String Record_ActionId
 -- ^ inv (actionsTable^.action.actionStr)
 -> Map String Record_RoleId
 -- ^ inv (rolesTable^.role.roleStr)
 -> Parser NotesOptions
newNoteOption asaid rsrid =
    NewNote
    <$> forceOption
    <*> requiredNewNoteArgument
    <*> optionalActionArgument asaid
    <*> optionalRoleArgument rsrid

notesOptions ::
    Map String Record_ActionId
 -- ^ inv (actionsTable ^. action.actionStr)
 -> Map String Record_RoleId
 -- ^ inv (rolesTable ^. role.roleStr)
 ->  Parser NotesOptions
notesOptions asaid rsrid =
    showNotesOption <|> newNoteOption asaid rsrid
