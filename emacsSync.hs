#!/usr/bin/env stack
{- stack
     --resolver lts-8.2
     --install-ghc
     runghc
     --package split
 -}

import System.Directory
import Data.List.Split (splitOn)

emacsDirectoryDotFiles = fmap (++ "/.emacs.d") getCurrentDirectory :: IO FilePath
emacsSystemDirectory = fmap (++ "/.emacs.d") getHomeDirectory  :: IO FilePath

removeIfExist :: FilePath -> IO ()
removeIfExist file = do
  exist <- doesFileExist file
  if exist
  then removeFile file
  else return ()
                
copyFileToDirectory :: FilePath -> FilePath -> IO ()
copyFileToDirectory file dir = copyFile file (dir ++ "/" ++ destinationFile)
    where destinationFile = last $ splitOn "/" file
  
main = do
  file' <- emacsDirectoryDotFiles
  file'' <- emacsSystemDirectory
  files <- getDirectoryContents file' :: IO [FilePath]
  let file1 = filter (\x -> x /= "." && x /= "..") files
      file2 = fmap (\x -> file'' ++ "/" ++ x) file1
      file3 = fmap (\x -> file' ++ "/" ++ x) file1
      file4 = zip file2 file3
  mapM_ removeIfExist file2
  mapM_ (\(x,y) -> copyFileToDirectory y file'') file4
