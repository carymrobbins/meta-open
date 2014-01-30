import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath
import System.Process

type GrepKey = String
type Command = String
type GrepMap = [(GrepKey, Command)]

data Program = IntelliJ

programMap :: [(Program, GrepMap)]
programMap = undefined

fileTypeAssociations :: [(String, Program)]
fileTypeAssociations = undefined

getGrepMapForFile :: FilePath -> GrepMap
getGrepMapForFile = undefined

handleFile :: Maybe FilePath -> IO ()
handleFile = undefined

main :: IO ()
main = liftM listToMaybe getArgs >>= handleFile

getBash :: String -> IO String
getBash cmd = readProcess "bash" ["-c", cmd] ""

