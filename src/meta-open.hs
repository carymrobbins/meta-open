import Control.Applicative
import Control.Monad
import Data.Conf
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.Process

type GrepKey = String
type Command = String
type GrepMap = [(GrepKey, Command)]
type FileExt = String
type Program = String
type ProgramMap = [(Program, GrepMap)]
type FileTypeAssociations = [(FileExt, Program)]

defaultCommand :: Command
defaultCommand = "open"

getPathToConf :: IO FilePath
getPathToConf = flip fmap getHomeDirectory (</> ".meta-open.hs")

getProgramMap :: Conf -> ProgramMap
getProgramMap = fromMaybe [] . getConf "programMap"

getFileTypeAssociations :: Conf -> FileTypeAssociations
getFileTypeAssociations = fromMaybe [] . getConf "fileTypeAssociations"

main :: IO ()
main = getArgs >>= handleFile

handleFile :: [String] -> IO ()
handleFile [] = putStrLn "Usage: meta-open [filename, [--debug]]"
handleFile [ filename ] = handleFile [ filename, "" ]
handleFile (filename : debugFlag : _) = do
    let debug = debugFlag == "--debug"
    let printIfDebug xs = if debug then putStrLn . concat $ xs else pure ()
    conf <- readConf =<< getPathToConf
    let programMap = getProgramMap conf
    printIfDebug [ "programMap: ", show programMap ]
    let fileTypeAssociations = getFileTypeAssociations conf
    printIfDebug [ "fileTypeAssociations: ", show fileTypeAssociations ]
    let getGrep = getGrepMapForFile programMap fileTypeAssociations
    running <- findRunningFromGrepMap . getGrep $ filename
    printIfDebug [ "running: ", show running ]
    let command = chooseCommand . findFirstCommand $ running
    printIfDebug [ "command: ", show command ]
    runBash . unwords $ [ command, filename ]

getGrepMapForFile
    :: ProgramMap -> FileTypeAssociations -> FilePath -> Maybe GrepMap
getGrepMapForFile programMap fileTypeAssociations filename = do
    let ext = takeExtension filename
    program <- lookup ext fileTypeAssociations
    lookup program programMap

findRunningFromGrepMap :: Maybe GrepMap -> IO [((GrepKey, Command), Bool)]
findRunningFromGrepMap Nothing = return []
findRunningFromGrepMap (Just grepMap) = mapM (pairM (isRunning . fst)) grepMap

findFirstCommand :: [((GrepKey, Command), Bool)] -> Maybe Command
findFirstCommand = liftM (snd . fst) . listToMaybe . filter (id . snd)

chooseCommand :: Maybe Command -> Command
chooseCommand = fromMaybe defaultCommand

pairM :: Monad m => (a -> m b) -> a -> m (a, b)
pairM f x = do
    result <- f x
    return (x, result)

isRunning :: GrepKey -> IO Bool
isRunning grepKey = do
    output <- getBash . concat $
        [ "ps aux | grep ", show grepKey, " | grep -v grep | cat" ]
    return . not . null $ output

getBash :: String -> IO String
getBash cmd = readProcess "bash" ["-c", cmd] ""

runBash :: String -> IO ()
runBash = void . getBash

