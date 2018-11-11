module Autofill.Directory.Format where

import Data.Maybe
import ID3.Simple

type ID3Tag = Tag

data FormatString = Plain Char | Format Char deriving (Show, Read)

parseFormatString :: String -> [FormatString]
parseFormatString [] = []
parseFormatString [x] = [Plain x]
parseFormatString (x:xs)
    | x == '%' = 
        let (y:xy) = xs in
          if not $ null xy
            then Format y : parseFormatString xy
            else [Format y]
    | otherwise =
        Plain x : parseFormatString xs

musicInfoFormat :: Char -> ID3Tag -> String
musicInfoFormat 'a' tag = fromMaybe "Unknown artist" $ getArtist tag
musicInfoFormat 't' tag = fromMaybe "Unknown title"  $ getTitle  tag
musicInfoFormat 'b' tag = fromMaybe "Unknown album"  $ getAlbum  tag
musicInfoFormat 'y' tag = fromMaybe "Unknown year"   $ getYear   tag
musicInfoFormat 'n' tag = fromMaybe "Unknown track"  $ getTrack  tag
musicInfoFormat _   _   = error "musicInfoFormat incorrect format"

musicInfoString :: String -> ID3Tag -> String
musicInfoString str music =
    concatFor parsed $ \x ->
      case x of
        Plain  c -> [c]
        Format c -> musicInfoFormat c music

    where
      concatFor :: [a] -> (a -> [b]) -> [b]
      concatFor x y = concatMap y x

      parsed = parseFormatString str
