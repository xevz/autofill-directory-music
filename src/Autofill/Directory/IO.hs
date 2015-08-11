module Autofill.Directory.IO where

import Control.Monad
import Data.Foldable (for_)
import System.Directory (getDirectoryContents, doesDirectoryExist, copyFile)
import System.FilePath
import System.Posix

getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return $ fileSize stat


getDirectoryRecursive :: FilePath -> IO [FilePath]
getDirectoryRecursive topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isPathDirectory <- doesDirectoryExist path
        if isPathDirectory
            then getDirectoryRecursive path
            else return [path]
    return (concat paths)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles dest paths =
    for_ paths $ \path -> do
        let name = takeFileName path
        copyFile path $ dest </> name

