{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

run :: RIO App ()
run = do
  options <- asks appOptions
  when (optionsVerbose options) $ logInfo "verbose"
  logInfo $ fromString ( "wallet=" ++ show ( optionsWallet options ) )
  logInfo $ fromString ( "tag=" ++ ( optionsTag options ) )
