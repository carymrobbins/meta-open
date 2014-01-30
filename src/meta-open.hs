import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath
import System.Process

type GrepKey = String
type Command = String
type GrepMap = [(GrepKey, Command)]

defaultCommand :: Command
defaultCommand = "open"

data Program = IntelliJ

programMap :: [(Program, GrepMap)]
programMap = undefined

fileTypeAssociations :: [(String, Program)]
fileTypeAssociations = undefined

getGrepMapForFile :: FilePath -> GrepMap
getGrepMapForFile = undefined

findRunningFromGrepMap :: GrepMap -> IO [((GrepKey, Command), Bool)]
findRunningFromGrepMap = undefined

findFirstCommand :: [((GrepKey, Command), Bool)] -> Maybe Command
findFirstCommand = undefined

chooseCommand :: Maybe Command -> Command
chooseCommand = undefined

handleFile :: Maybe FilePath -> IO ()
handleFile Nothing = putStrLn "Usage: meta-open [filename]"
handleFile (Just filename) = do
    let grepMap = getGrepMapForFile filename
    running <- findRunningFromGrepMap grepMap
    let command = chooseCommand . findFirstCommand $ running
    runBash . unwords $ [command, filename]

main :: IO ()
main = liftM listToMaybe getArgs >>= handleFile

pairM :: Monad m => (a -> m b) -> a -> m (a, b)
pairM = undefined

isRunning :: GrepKey -> IO Bool
isRunning grepKey = undefined

getBash :: String -> IO String
getBash cmd = readProcess "bash" ["-c", cmd] ""

runBash :: String -> IO ()
runBash cmd = getBash cmd >> return ()

