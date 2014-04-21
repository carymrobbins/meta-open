import Control.Monad
import Data.Conf
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.Process

getPathToConf = do
    home <- getHomeDirectory
    return $ home ++ "/.meta-open.hs"

defaultCommand = "open"

type GrepKey = String
type Command = String
type GrepMap = [(GrepKey, Command)]
type Program = String

getProgramMap :: Conf -> [(Program, GrepMap)]
getProgramMap = fromMaybe [] . getConf "programMap"

getFileTypeAssociations :: Conf -> [(String, Program)]
getFileTypeAssociations = fromMaybe [] . getConf "fileTypeAssociations"

getGrepMapForFile
    :: [(Program, GrepMap)] -> [(String, Program)] -> FilePath -> Maybe GrepMap
getGrepMapForFile programMap fileTypeAssociations filename = do
    program <- lookup ext fileTypeAssociations
    lookup program programMap
  where
    ext = takeExtension filename

findRunningFromGrepMap :: Maybe GrepMap -> IO [((GrepKey, Command), Bool)]
findRunningFromGrepMap Nothing = return []
findRunningFromGrepMap (Just grepMap) = mapM (pairM (isRunning . fst)) grepMap

findFirstCommand :: [((GrepKey, Command), Bool)] -> Maybe Command
findFirstCommand = liftM (snd . fst) . listToMaybe . filter (id . snd)

chooseCommand :: Maybe Command -> Command
chooseCommand = fromMaybe defaultCommand

handleFile :: Maybe FilePath -> IO ()
handleFile Nothing = putStrLn "Usage: meta-open [filename]"
handleFile (Just filename) = do
    conf <- readConf =<< getPathToConf
    let programMap = getProgramMap conf
    let fileTypeAssociations = getFileTypeAssociations conf
    let getGrep = getGrepMapForFile programMap fileTypeAssociations
    running <- findRunningFromGrepMap . getGrep $ filename
    let command = chooseCommand . findFirstCommand $ running
    runBash . unwords $ [ command, filename ]

main :: IO ()
main = liftM listToMaybe getArgs >>= handleFile

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

