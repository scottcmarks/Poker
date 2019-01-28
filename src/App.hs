{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App
where


import RIO
import RIO.Process
import RIO.Text (Text, pack, unpack)



-- | Command line arguments
data AppOptions = AppOptions
  { appOptionsVerbose    :: !Bool
  , appOptionsDbFilePath :: !FilePath
  }

class HasAppOptions env where
  optionsL :: Lens' env AppOptions
instance HasAppOptions AppOptions where
  optionsL = id

class HasVerbose env where
  verboseL :: Lens' env Bool
instance HasVerbose Bool  where
  verboseL = id

class HasDbFilePath env where
  dbFilePathL :: Lens' env Text
instance HasDbFilePath Text  where
  dbFilePathL = id
instance HasDbFilePath String  where
  dbFilePathL = lens pack (const unpack)


-- | processing subommand (list, chunks) line arguments
data MergeOptions = MergeOptions
  { processOptionsRaw    :: !Bool
  }

class HasProcessOptions env where
  processOptionsL :: Lens' env MergeOptions
instance HasProcessOptions MergeOptions where
  processOptionsL = id

class HasProcessRaw env where
  rawL :: Lens' env Bool
instance HasProcessRaw Bool  where
  rawL = id

instance HasProcessRaw MergeOptions where
   rawL = lens processOptionsRaw (\x y -> x { processOptionsRaw = y })




-- | application execution environment
data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appAppOptions     :: !AppOptions
  , appVerbose        :: !Bool
  , appDbFilePath     :: !Text
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL        = lens appLogFunc        (\x y -> x { appLogFunc        = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasAppOptions App where
  optionsL        = lens appAppOptions     (\x y -> x { appAppOptions     = y })
instance HasVerbose App  where
  verboseL        = lens appVerbose        (\x y -> x { appVerbose        = y })
instance HasDbFilePath App  where
  dbFilePathL     = lens appDbFilePath     (\x y -> x { appDbFilePath     = y })
