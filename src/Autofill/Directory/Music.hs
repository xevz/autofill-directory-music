module Autofill.Directory.Music where

import Autofill.Directory.IO

import Control.Monad
import Data.List
--import System.Directory
import System.Posix
import Text.Printf

data MusicFileType = Mp3 deriving (Eq, Show)
data MusicFileInfo = MusicFileInfo {
                     musicFilePath :: String
                   , musicFileSize :: FileOffset
                   , musicFileType :: MusicFileType
                   } deriving (Show)

data MusicFileTree = MusicFileTree [MusicFileInfo] deriving (Show)

pathToInfo :: FilePath -> IO MusicFileInfo
pathToInfo path = do
    fsize <- getFileSize path

    return MusicFileInfo {
                       musicFilePath = path
                     , musicFileSize = fsize
                     , musicFileType = Mp3
                     }

getMusicTree :: [FilePath] -> IO [MusicFileInfo]
getMusicTree paths =
    mapM pathToInfo $
      filter (isSuffixOf "mp3") paths

getAutofillMusic :: FileOffset -> [MusicFileInfo] -> IO [MusicFileInfo]
getAutofillMusic _ [] = return []
getAutofillMusic 0 _  = return []
getAutofillMusic size (x:xs) = do
    if newsize < 0
      then getAutofillMusic size xs
      else do
        music <- getAutofillMusic newsize xs
        return $ x : music

    where
      csize = musicFileSize x
      newsize = size - csize

printMusicTree :: [MusicFileInfo] -> IO ()
printMusicTree tree =
    forM_ tree $ \x ->
      printf "%s (%d)\n"
        (musicFilePath x)
        (toInteger $ musicFileSize x)

