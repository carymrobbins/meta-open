import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath
import System.Process

data Program = IntelliJ

programMap :: [(Program, [(GrepKey, Command)])]
programMap = undefined

fileTypeAssociations :: [(String, Program)]
fileTypeAssociations = undefined

type GrepKey = String
type Command = String

main :: IO ()
main = undefined

getBash :: String -> IO String
getBash cmd = readProcess "bash" ["-c", cmd] ""

