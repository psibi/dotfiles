{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( releaseInfo
  , Uptime(..)
  , parseUptime
  , uptimeInfo
  , DiskInfo
  , spaceInfo
  , uncompress_
  ) where

import Data.Conduit.Shell hiding (strip)
import qualified Data.Conduit.Shell as S
import Data.Conduit.Shell.Segments (strings, texts)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Bits.Utils (c2w8)
import Data.Monoid ((<>))
import Data.Char (isAlpha, isSpace, isPunctuation, digitToInt)
import qualified Data.Text.IO as TIO
import Data.Text (Text, pack)
import Data.Attoparsec.Text
import Data.Word
import Data.List (isSuffixOf)

releaseInfo :: IO ()
releaseInfo = do
  (val1 :: [String]) <- run $ strings $ lsbRelease ["-i"]
  let distro = getData val1
  (val2 :: [String]) <- run $ strings $ lsbRelease "-r"
  let version = getData val2
  putStrLn (distro <> " " <> version)
  where
    getData val =
      let xs = splitOn ":" (head val)
      in strip $ last xs

data Uptime = Uptime
  { hour :: Word8
  , mins :: Word8
  } deriving (Show, Eq)

parseUptime :: Parser Uptime
parseUptime = do
  skipWhile (\x -> isAlpha x || isSpace x)
  hour <- decimal
  skipWhile (\x -> isAlpha x || isSpace x || isPunctuation x)
  min <- decimal
  return $ Uptime hour min

uptimeInfo :: IO ()
uptimeInfo = do
  (val :: [Text]) <- run $ texts $ uptime "-p"
  case maybeResult (parse parseUptime (head val)) of
    Nothing -> putStrLn ""
    Just inf -> putStrLn (show (hour inf) <> "h" <> show (mins inf) <> "m")

data DiskInfo = DiskInfo
  { fsystem :: Text
  , diSize :: Text
  , diUsed :: Text
  , diAvail :: Text
  , diUse :: Text
  , diMountPoint :: Text
  } deriving (Show, Eq)

parseDiskInfo :: Parser DiskInfo
parseDiskInfo = do
  fsystem <- takeTill isSpace
  skipSpace
  diSize <- takeTill isSpace
  skipSpace
  diUsed <- takeTill isSpace
  skipSpace
  diAvail <- takeTill isSpace
  skipSpace
  diUse <- takeTill isSpace
  skipSpace
  diMountPoint <- takeText
  return
    DiskInfo
    { ..
    }

spaceInfo :: IO ()
spaceInfo = do
  (val :: [Text]) <- run $ texts $ df "-h" ["/home"]
  case parseOnly parseDiskInfo (last val) of
    Left _ -> putStrLn ""
    Right inf -> TIO.putStrLn $ (diAvail inf) <> (pack " ") <> (diUse inf)

getExtension :: FilePath -> Maybe String
getExtension fname =
  case (filter (\x -> isSuffixOf x fname) (doubleExts <> singleExts)) of
    [] -> Nothing
    (x:_) -> Just x
  where
    doubleExts = ["tar.bz2", "tar.gz"]
    singleExts = ["rar", "gz", "tar", "tbz2", "bz2", "tgz", "zip"]

uncompress_ :: FilePath -> IO ()
uncompress_ fname =
  case (getExtension fname) of
    Nothing -> putStrLn "file format not supported"
    Just xs ->
      run $
      case xs of
        "tar.bz2" -> tar "xvjf" fname
        "tar.gz" -> tar "xvzf" fname
        "rar" -> unrar "x" fname
        "gz" -> gunzip fname
        "tar" -> tar "xvf" fname
        "tbz2" -> tar "xvjf" fname
        "tgz" -> tar "xvzf" fname
        "zip" -> S.unzip' fname
