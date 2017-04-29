module Main where

import Options.Applicative
import Data.Monoid ((<>))
import Lib

data SibiUtilsOpts = SibiUtilsOpts
  { optCommand :: Command
  } deriving (Show, Eq, Ord)

data Command
  = Uncompress String
  | Compress String
  | UpDir Int
  | Deploy Config
  | Sync Config
  | Init
  deriving (Show, Eq, Ord)

data Config
  = Fish
  | Emacs
  | XMonad
  | Bash
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    Uncompress fname -> uncompress_ fname
    Compress fname -> compress fname
    UpDir level -> navigateParent level
    Deploy Fish -> deployFish
    Deploy Emacs -> deployEmacs
    Deploy XMonad -> deployXMonad
    Deploy Bash -> deployBash
    Sync Fish -> syncFish
    Sync Emacs -> syncEmacs
    Sync XMonad -> syncXMonad
    Init -> initializeSystem

optsParser :: ParserInfo SibiUtilsOpts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "Utility program for Sibi" <>
     header "sibi-utils - Utility kitchen sink tool")
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.1" (long "version" <> help "Show version")
    programOptions :: Parser SibiUtilsOpts
    programOptions =
      SibiUtilsOpts <$>
      hsubparser
        (uncompressCommand <> compressCommand <> upDirCommand <> deployCommand <>
         syncCommand <>
         initCommand)
    initCommand :: Mod CommandFields Command
    initCommand =
      command "init" (info (pure Init) (progDesc "Initialize system"))
    uncompressCommand :: Mod CommandFields Command
    uncompressCommand =
      command "uncompress" (info uncompressOptions (progDesc "Uncompress file"))
    uncompressOptions =
      Uncompress <$>
      strArgument (metavar "FILENAME" <> help "File to uncompress")
    compressCommand :: Mod CommandFields Command
    compressCommand =
      command "compress" (info compressOptions (progDesc "Compress file"))
    compressOptions =
      Compress <$> strArgument (metavar "FILENAME" <> help "File to compress")
    upDirCommand :: Mod CommandFields Command
    upDirCommand =
      command "updir" (info updirOptions (progDesc "Go to parsent dir"))
    updirOptions =
      UpDir <$>
      argument
        auto
        (metavar "LEVEL" <> help "Integer representing parent level")
    deployCommand :: Mod CommandFields Command
    deployCommand =
      command "deploy" (info deployOptions (progDesc "Deploy dotfiles"))
    deployOptions :: Parser Command
    deployOptions =
      Deploy <$>
      hsubparser (fishCommand <> emacsCommand <> xmonadCommand <> bashCommand)
    fishCommand :: Mod CommandFields Config
    fishCommand =
      command "fish" (info fishOptions (progDesc "Load fresh fish config"))
    fishOptions :: Parser Config
    fishOptions = pure Fish
    emacsCommand :: Mod CommandFields Config
    emacsCommand =
      command "emacs" (info emacsOptions (progDesc "Load fresh bash config"))
    emacsOptions :: Parser Config
    emacsOptions = pure Emacs
    xmonadCommand =
      command
        "xmonad"
        (info (pure XMonad) (progDesc "Load fresh xmonad config"))
    bashCommand =
      command "bash" (info (pure Bash) (progDesc "Load fresh bash config"))
    syncCommand :: Mod CommandFields Command
    syncCommand =
      command
        "sync"
        (info syncOptions (progDesc "Sync machine config to dotfiles"))
    syncOptions :: Parser Command
    syncOptions =
      Sync <$>
      hsubparser (fishSyncCommand <> emacsSyncCommand <> xmonadSyncCommand)
    fishSyncCommand :: Mod CommandFields Config
    fishSyncCommand =
      command "fish" (info (pure Fish) (progDesc "Sync fish configuration"))
    emacsSyncCommand :: Mod CommandFields Config
    emacsSyncCommand =
      command "emacs" (info (pure Emacs) (progDesc "Sync Emacs configuration"))
    xmonadSyncCommand :: Mod CommandFields Config
    xmonadSyncCommand =
      command
        "xmonad"
        (info (pure XMonad) (progDesc "Synx Xmonad configuration"))
