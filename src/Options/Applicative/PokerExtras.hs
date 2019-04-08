{-|
Module      : Options.Applicative.PokerExtras
Description : Parse command line with commands depending on the global settings
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Parsing of command line global settings, command, and command flags and arguments, with the command parser a function of the global settings.
-}

module Options.Applicative.PokerExtras
  (
    optionsAndCommand
  , mapStr
  , mapArgument
  ) where

import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.Trans.Writer           (Writer)
import           Data.List                            (intercalate)
import           Data.Map                             (keys, (!), (!?))
import           Options.Applicative.BashCompletion   (bashCompletionParser)
import           Options.Applicative.Builder          (readerError)
import           Options.Applicative.Builder.Internal (ArgumentFields (..),
                                                       DefaultProp (..),
                                                       Mod (..))
import           Options.Applicative.Common           (Parser, ParserInfo (..),
                                                       ParserPrefs, runParser)
import           Options.Applicative.Internal         (MonadP, runP)
import           Options.Applicative.Simple           (CommandFields,
                                                       ParserResult (..),
                                                       argument, defaultPrefs,
                                                       execParser,
                                                       execParserPure, fullDesc,
                                                       handleParseResult,
                                                       header, help, info,
                                                       infoOption, long,
                                                       parserFailure, progDesc,
                                                       simpleParser)
import           Options.Applicative.Types            (ArgPolicy, Args,
                                                       IsCmdStart (..),
                                                       ReadM (..), readerAsk)
import           RIO                                  (Either (..), IO, Map,
                                                       String, null, pure, snd,
                                                       undefined, ($), (<$>),
                                                       (<&>), (<*>), (<>),
                                                       (<|>))
import           System.Environment                   (getArgs, withArgs)

import           Data.Map.PokerExtras                 (inv)

--------------------------------------------------------------------------------
-- * Drill down into 'Options.Applicative.Simple.simpleOptions',
--   extending its functionality
--   so that the commands to be processed can depend on the global settings
--   before the commands.
--------------------------------------------------------------------------------

-- | Generate and execute a simple options parser.
--   The commands may depend on the global settings.
--   Modified from 'Options.Applicative.Simple.simpleOptions'
--   to parse the global settings,
--   compute the commands based on those values
--   and then parse those commands.
--   Used in our case to supply default values for command options
--   that depend on the global setting values
optionsAndCommand ::
     String
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser a
  -- ^ global setting
  -> (a -> IO (ExceptT b (Writer (Mod CommandFields b)) ()))
  -- ^ commands (use 'Options.Applicative.addCommand',
  --   but such that they depend on the parsed global settings)
  -> IO (a, b)
optionsAndCommand versionString h pd globalParser commandParser = do
    args <- getArgs
    case optsParse args of
      Failure _ | null args -> do
          opts <- showHelp optsParser
          pure (opts, undefined)
      Success (opts, args') -> do
          p <- commandParser opts
          cmd <-
              case cmdParse p args' of
                Failure _ | null args' -> showHelp $ cmdParser p
                Success c              -> pure c
                parseResult            -> handleParseResult parseResult
          pure (opts, cmd)
      parseResult -> do
          (opts, _) <- handleParseResult parseResult
          pure (opts, undefined)
  where
      optsParser = info (versionOption <*> globalParser) desc
      optsParse = execParserPurePartially defaultPrefs optsParser
      cmdParser p = snd <$> info (simpleParser (pure ()) p) desc
      cmdParse p = execParserPure defaultPrefs (cmdParser p)
      desc = fullDesc <> header h <> progDesc pd
      versionOption =
          infoOption versionString (long "version" <> help "Show version")
      showHelp p = withArgs ["--help"] (execParser p)

-- | Parse some args against a command line parser
--   returning a parser result and unused args.
--   Use to parse the global arguments before moving on to the commands
runParserPartially :: MonadP m =>
    ArgPolicy
 -- ^ option and argument intermixing rules
 -> Parser a
 -- ^ Program argument parser
 -> Args
 -- ^ Program arguments
 -> m (a, Args)
runParserPartially policy = runParser policy CmdStart

-- | Parse some args against a program description
--   returning a parser result and unused args.
--   Use to parse the global arguments before moving on to the commands
runParserInfoPartially :: MonadP m =>
    ParserInfo a
 -- ^ Description of the program to run
 -> Args
 -- ^ Program arguments
 -> m (a, Args)
runParserInfoPartially i = runParserPartially (infoPolicy i) (infoParser i)

-- | Parse some args returning a parser result and unused args.
--   Use to parse the global arguments before moving on to the commands
execParserPurePartially ::
     ParserPrefs
 -- ^ Global preferences for this parser
 -> ParserInfo a
 -- ^ Description of the program to run
 -> Args
 -- ^ Program arguments
 -> ParserResult (a, Args)
execParserPurePartially pprefs pinfo args =
  case runP p pprefs of
    (Right (Right r, args'), _) -> Success (r, args')
    (Right (Left c , _    ), _) -> CompletionInvoked c
    (Left err, ctx) -> Failure $ parserFailure pprefs pinfo err ctx
  where
    pinfo' =
      pinfo
        { infoParser =
            (Left  <$> bashCompletionParser pinfo pprefs)
           <|>
            (Right <$> infoParser pinfo)
        }
    p = runParserInfoPartially pinfo' args


-- * -- A couple of builders

-- | Make a 'ReadM' reader from a 'Map' 'String' @a@
mapStr ::
    Map String a
 -> ReadM a
mapStr m = do
    arg <- readerAsk
    maybe (readerError $ errMsg arg) pure $ m !? arg
  where
    errMsg arg =
        mconcat [ "Did not recognize `"
                , arg
                , "'\n"
                , "Legal values are: `"
                , (intercalate "', `" (keys m))
                , "'"
                ]

-- | Build an argument 'Parser' @a@ from a 'Map' 'String' -> @a@
--   This allows specifying the default 'Options.Applicative.Builder.value' as
--   a 'String' and simply specifying 'Options.Applicative.Builder.showDefault',
--   since the 'Mod' 'ArgumentFields' are over 'String'
--   (as with 'Options.Applicative.strArgument'),
--   with this code using the map as necessary as an isomorphism  between
--   'String' and @a@.
mapArgument :: Ord a =>
    Map String a
 -> Mod ArgumentFields String
 -> Parser a
mapArgument m (Mod f d g) = argument mapReader (Mod f' d' g)
  where
    mapReader = mapStr m
    f' (ArgumentFields cp) = (ArgumentFields cp')
      where (ArgumentFields cp') = f (ArgumentFields cp)
    d' = (DefaultProp ma mfa)
      where
        (DefaultProp ms mfs) = d
        ma  = maybe Nothing (m !?)                    ms
        mfa = maybe Nothing (Just . ((inv m !) <&>) ) mfs
