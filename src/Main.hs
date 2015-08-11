import Autofill.Directory.IO
import Autofill.Directory.Music
import Autofill.Directory.Shuffle

import Data.List
import Data.Char
-- import Data.Maybe (fromMaybe)
import System.Console.GetOpt
-- import System.Directory
import System.Environment
import System.Posix

data Flag = Verbose | DryRun deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
            Option "v" ["verbose"] (NoArg Verbose) "verbose output"
          , Option "d" ["dry-run"] (NoArg DryRun)  "do not actually do anyting"
          ]

getUsage :: IO String
getUsage = do
    progname <- getProgName

    let header = "Usage: " ++ progname ++ " [OPTIONS] size source destination" 

    return $ usageInfo header options

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = do
    usage <- getUsage

    case getOpt Permute options argv of
      (o, n, [])    -> return (o, n)
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

doAutofill :: [Flag] -> [String] -> IO ()
doAutofill opts args = do
    let (size:path:dest:_) = args

    files <- getDirectoryRecursive path
    --files <- getDirectoryContents path
    music <- getMusicTree files
    shuffled <- shuffle music
    autofill <- getAutofillMusic (formatSize size) shuffled

    printMusicTree autofill

    if DryRun `elem` opts
      then putStrLn "*** Dry run, did nothing."
      else copyFiles dest $ map musicFilePath autofill

main :: IO ()
main = do
    argv <- getArgs
    (opts, args) <- parseOptions argv
    usage <- getUsage

    if length args < 3
      then putStrLn usage
      else doAutofill opts args

