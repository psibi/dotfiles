module Main where

import Options.Applicative
import Data.Monoid ((<>))
import Lib (uncompress_, compress)

data SibiUtilsOpts = SibiUtilsOpts
  { optCommand :: Command
  } deriving (Show, Eq, Ord)

data Command
  = Uncompress String
  | Compress String
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    Uncompress fname -> uncompress_ fname
    Compress fname -> compress fname

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
      SibiUtilsOpts <$> hsubparser (uncompressCommand <> compressCommand)
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
