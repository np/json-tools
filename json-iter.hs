import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Aeson
import Utils (parseJSONFiles, systemWithStdin)
import qualified Data.Vector as V

unSequence :: Value -> [Value]
unSequence (Array a) = V.toList a
unSequence _         = error "unSequence: a JSON sequence was expected"

iterJSON :: String -> [String] -> IO ()
iterJSON cmd jsonfiles =
  mapM_ (mapM_ (systemWithStdin cmd . encode) . unSequence)
    =<< parseJSONFiles jsonfiles

usage :: IO a
usage = mapM_ (hPutStrLn stderr)
  ["Usage: json-iter [--help] <cmd> <json-file>*"
  ,""
  ,"Reads the given JSON files which have to be JSON arrays, and run the given"
  ,"command for each element. The command receive the JSON document as"
  ,"standard input."]
  >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  when ("--help"`elem`args) usage
  case args of
    cmd : files -> iterJSON cmd files
    [] -> usage
