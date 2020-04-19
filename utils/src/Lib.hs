{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( releaseInfo
  , Uptime(..)
  , parseUptime
  , uptimeInfo
  , DiskInfo
  , spaceInfo
  ) where

import Data.Conduit.Shell hiding (strip)
import qualified Data.Conduit.Shell as S
import Data.Conduit.Shell.Segments (strings, texts)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Bits.Utils (c2w8)
import Data.Monoid ((<>))
import Data.Char (isAlpha, isPunctuation, digitToInt)
import qualified Data.Text.IO as TIO
import Data.Text (Text, pack)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Word
import Data.List (isSuffixOf, intersperse)
import System.Directory
       (doesDirectoryExist, getHomeDirectory, doesFileExist,
        createDirectoryIfMissing)
import System.FilePath
import Data.Conduit (await)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.Binary as C
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Control.Monad.IO.Class (liftIO)
import RIO.Process
import RIO.Text (decodeUtf8With, lenientDecode)
import qualified RIO.Process as P
import qualified Data.ByteString.Lazy as BL
import RIO (runSimpleApp, void)

parseRelease :: Parser B8.ByteString
parseRelease = do
  void $ takeTill (\x -> x == ':')
  void $ A8.take 1
  skipSpace                     
  takeTill isSpace

releaseInfo :: IO ()
releaseInfo = do
  (_, val1) <- runSimpleApp $ P.proc "lsb_release" ["-i"] readProcessStdout
  (_, val2) <- runSimpleApp $ P.proc "lsb_release" ["-r"] readProcessStdout
  case parseOnly parseRelease (BL.toStrict val1) of
    Left _ -> putStrLn ""            
    Right distro -> case parseOnly parseRelease (BL.toStrict val2) of
      Left _ -> putStrLn ""                                 
      Right version -> B8.putStrLn (distro <> " " <> version)

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
  (_, val) <- runSimpleApp $ P.proc "uptime" ["-p"] readProcessStdout
  case maybeResult (parse parseUptime $ BL.toStrict val) of
    Nothing -> putStrLn ""
    Just inf -> putStrLn (show (hour inf) <> "h" <> show (mins inf) <> "m")

data DiskInfo = DiskInfo
  { fsystem :: B8.ByteString
  , diSize :: B8.ByteString
  , diUsed :: B8.ByteString
  , diAvail :: B8.ByteString
  , diUse :: B8.ByteString
  , diMountPoint :: B8.ByteString
  } deriving (Show, Eq)

parseDiskInfo :: Parser DiskInfo
parseDiskInfo = do
  void $ takeTill isNewLine
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
  diMountPoint <- takeByteString
  return
    DiskInfo
    { ..
    }

isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine _ = False

partialDecode :: BL.ByteString -> Text              
partialDecode bs = decodeUtf8With lenientDecode $ BL.toStrict bs

spaceInfo :: IO ()
spaceInfo = do
  (_, val) <- runSimpleApp $ P.proc "df" ["-h", "/home"] readProcessStdout
  case parseOnly parseDiskInfo (BL.toStrict val) of
    Left _ -> putStrLn ""
    Right inf -> BS.putStrLn $ (diAvail inf) <> " " <> (diUse inf)
