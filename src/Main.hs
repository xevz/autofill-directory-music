import Autofill.Directory.IO
import Autofill.Directory.Music
import Autofill.Directory.Shuffle

import Data.List
import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Posix

data Options = Options {
               optVerbose    :: Bool
             , optDryRun     :: Bool
             , optFileFormat :: String
             } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options {
                           optVerbose    = False
                         , optDryRun     = False
                         , optFileFormat = ""
                         }

options :: [OptDescr (Options -> Options)]
options = [
            Option "v" ["verbose"]
              (NoArg (\opts -> opts { optVerbose = True }))
              "verbose output"

          , Option "d" ["dry-run"]
              (NoArg (\opts -> opts { optDryRun = True }))
              "do not actually do anyting"

          , Option "f" ["format"]
              (ReqArg (\format opts -> opts { optFileFormat = format }) "FORMAT")
              "specify file naming format"
          ]

getUsage :: IO String
getUsage = do
    progname <- getProgName

    let header = "Usage: " ++ progname ++ " [OPTIONS] size source destination" 

    return $ usageInfo header options

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv = do
    usage <- getUsage

    case getOpt Permute options argv of
      (o, n, [])    -> return (foldl (flip id) defaultOptions o, n)
      (_ , _, errs) -> ioError (userError (concat errs ++ usage))

formatSize :: String -> FileOffset
formatSize ssize =
    case unit of
      "K" -> size * 1024
      "M" -> size * 1024 * 1024
      "G" -> size * 1024 * 1024 * 1024
      _   -> size
    where 
      (num, unit) = partition isDigit ssize
      size        = read num :: FileOffset

doAutofill :: Options -> [String] -> IO ()
doAutofill opts args = do
    files <- getDirectoryRecursive path
    music <- getMusicTree files
    shuffled <- shuffle music
    autofill <- getAutofillMusic (formatSize size) shuffled

    printMusicTree autofill

    if optDryRun opts
      then putStrLn "*** Dry run, did nothing."
      else copyFiles dest $ map musicFilePath autofill

    where
      (size:path:dest:_) = args

main :: IO ()
main = do
    argv <- getArgs
    (opts, args) <- parseOptions argv
    usage <- getUsage

    if length args < 3
      then putStrLn usage
      else doAutofill opts args

