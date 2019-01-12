{-# LANGUAGE NoImplicitPrelude #-}

module Main
where

import RIO
import App
import Options(getOptions)
import RIO.Process(mkDefaultProcessContext)
import Run(run)

main :: IO ()
main = do
  ( options @ AppOptions{ appOptionsVerbose    = verbose
                        , appOptionsDbFilePath = dbFilePath
                        }
   ,
    runCmd
   ) <- getOptions
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc        = lf
                  , appProcessContext = pc
                  , appAppOptions     = options
                  , appVerbose        = verbose
                  , appDbFilePath     = fromString dbFilePath
                  }
     in runRIO app $ run runCmd
