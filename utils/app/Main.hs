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
  deriving (Show, Eq, Ord)

data Config = Fish | Bash deriving (Show, Eq, Ord)

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    Uncompress fname -> uncompress_ fname
    Compress fname -> compress fname
    UpDir level -> navigateParent level
    Deploy Fish -> deployFish
    Deploy Bash -> print "bash"

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
      SibiUtilsOpts <$> hsubparser (uncompressCommand <> compressCommand <> upDirCommand <> deployCommand)
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
    upDirCommand = command "updir" (info updirOptions (progDesc "Go to parsent dir"))
    updirOptions = UpDir <$> argument auto (metavar "LEVEL" <> help "Integer representing parent level")
    deployCommand :: Mod CommandFields Command
    deployCommand = command "deploy" (info deployOptions (progDesc "Deploy dotfiles"))
    deployOptions :: Parser Command
    deployOptions = Deploy <$> hsubparser (fishCommand <> bashCommand)
    fishCommand :: Mod CommandFields Config
    fishCommand = command "fish" (info fishOptions (progDesc "Load fresh fish config"))
    fishOptions :: Parser Config
    fishOptions = pure Fish
    bashCommand :: Mod CommandFields Config
    bashCommand = command "bash" (info bashOptions (progDesc "Load fresh bash config"))
    bashOptions :: Parser Config
    bashOptions = pure Bash

