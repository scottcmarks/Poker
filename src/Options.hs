{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Options
where

import           App
import           RIO
import           Options.Applicative.Simple (
    CommandFields
  , Mod
  , Parser
  , simpleOptions
  , addCommand
  , help
  , short
  , long
  , switch
  , metavar
  , value
  , showDefault
  , strOption
  , simpleVersion
  , internal
  , hidden
  )
import           Control.Monad.Trans.Except(ExceptT)
import           Control.Monad.Trans.Writer(Writer)
import qualified Paths_Poker(version)
import           Run(
    chunks
  , countRecords
  , exportAsCsv
  , list
  )

-- common options that can appear first, before commands
appOptions :: Parser AppOptions
appOptions = ( AppOptions <$>

    switch
    ( long "verbose"
   <> short 'v'
   <> help "Verbose output?"
    )

  <*>

    strOption
    ( long "dbFilePath"
   <> short 'd'
   <> help "Database file path?"
   <> showDefault
   <> value "Poker.db"
   <> metavar "DB"
    )

  )

mergeOptions :: Parser MergeOptions
mergeOptions = ( MergeOptions <$>

     switch
     ( long "raw"
    <> short 'r'
    <> help "raw (unmerged) output?"
    <> hidden <> internal
     )

  )


-- Commands
commands :: ExceptT (RIO App ()) (Writer (Mod CommandFields (RIO App ()))) ()
commands = do addCommand "list"
                         "List all database records"
                         list
                         mergeOptions
              addCommand "export"
                         "Export all records in .csv format"
                         exportAsCsv
                         mergeOptions
              addCommand "count"
                         "Print the count of all records"
                         countRecords
                         mergeOptions
              addCommand "chunks"
                         "Gather the records into chunks"
                         chunks
                         mergeOptions

-- Version string
ver ∷ String
ver = "Poker " ++ $(simpleVersion Paths_Poker.version)

-- Header for command line arguments
head ∷ String
head = ver ++ " -- © 2019 Magnolia Heights R&D.  All rights reserved."

-- Program description, also for command line arguments
desc ∷ String
desc = "Operate on poker database.  For more information on <COMMAND>, try <COMMAND> --help"

--
getOptions :: IO (AppOptions, RIO App ())
getOptions = simpleOptions ver head desc appOptions commands

