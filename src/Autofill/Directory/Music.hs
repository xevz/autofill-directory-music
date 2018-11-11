module Autofill.Directory.Music where

import           Autofill.Directory.IO

import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified ID3.Simple as ID3
import           System.Posix
import           Text.Printf

data MusicFileType = Mp3 deriving (Eq, Show)
data MusicFileInfo = MusicFileInfo {
                     musicFilePath :: String
                   , musicFileSize :: FileOffset
                   , musicFileType :: MusicFileType
                   , musicFileTag  :: Maybe ID3.Tag
                   } deriving (Show)

data MusicFileTree = MusicFileTree [MusicFileInfo] deriving (Show)

pathToInfo :: FilePath -> IO MusicFileInfo
pathToInfo path = do
    fsize <- getFileSize path
    tag <- ID3.readTag path

    return MusicFileInfo {
                       musicFilePath = path
                     , musicFileSize = fsize
                     , musicFileType = Mp3
                     , musicFileTag  = tag
                     }

getMusicTree :: [FilePath] -> IO [MusicFileInfo]
getMusicTree paths =
    mapM pathToInfo $
      filter (isSuffixOf "mp3") paths

getAutofillMusic :: FileOffset -> [MusicFileInfo] -> IO [MusicFileInfo]
getAutofillMusic _ [] = return []
getAutofillMusic 0 _  = return []
getAutofillMusic size (x:xs) =
    if newsize < 0
      then getAutofillMusic size xs
      else do
        music <- getAutofillMusic newsize xs
        return $ x : music

    where
      csize = musicFileSize x
      newsize = size - csize

getArtistTitleFromTag :: MusicFileInfo -> Maybe String
getArtistTitleFromTag music = do
    let tag = musicFileTag music
    case tag of
      Just t ->
        case (ID3.getArtist t, ID3.getTitle t) of
          (Just a, Just ti) -> Just $ a ++ " - " ++ ti
          _                 -> Nothing
      _      -> Nothing

printMusicTree :: [MusicFileInfo] -> IO ()
printMusicTree tree =
    forM_ tree $ \x ->
      let name = fromMaybe (musicFilePath x) (getArtistTitleFromTag x) in
      printf "%s (%d)\n"
        name
        (toInteger $ musicFileSize x)

