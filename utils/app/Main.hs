module Main where

import Options.Applicative
import Data.Monoid ((<>))
import Lib (uncompress_)

data SibiUtilsOpts = SibiUtilsOpts
  { optCommand :: Command
  } deriving (Show, Eq, Ord)

data Command =
  Uncompress String
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    Uncompress fname -> uncompress_ fname

optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "Utility program for Sibi" <>
     header "sibi-utils - Utility kitchen sink tool")
  where
    versionOption = infoOption "0.1" (long "version" <> help "Show version")
    programOptions = SibiUtilsOpts <$> hsubparser (uncompressCommand)
    uncompressCommand =
      command "uncompress" (info uncompressOptions (progDesc "Uncompress file"))
    uncompressOptions =
      Uncompress <$>
      strArgument (metavar "FILENAME" <> help "Name of the file to uncompress")
