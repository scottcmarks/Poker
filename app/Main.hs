{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_Poker

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_Poker.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch
           ( long "verbose"
          <> short 'v'
          <> help "Verbose output?"
           )
       <*> option auto
           ( long "wallet"
          <> short 'w'
          <> help "Wallet amount?"
          <> showDefault
          <> value 1
          <> metavar "AMT"
           )
       <*> strOption
           ( long "tag"
          <> short 't'
          <> help "Transaction tag?"
          <> showDefault
          <> value "Five Oaks"
          <> metavar "TAG"
           )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
