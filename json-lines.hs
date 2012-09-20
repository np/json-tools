import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,stderr)
import Utils (readFilesL)
import qualified Data.Vector as V

jsonUnlines :: Value -> [Value]
jsonUnlines (Array xs) = V.toList xs
jsonUnlines _          = error "Unexpected JSON value, only an array was expected"

usage :: IO a
usage = mapM_ (hPutStrLn stderr)
  ["Usage: json-line [--help] <file>*"
  ,""
  ,"Reads the given files as JSON arrays (or standard input, if '-' or"
  ,"no file is given). Writes on standard output the elements of these"
  ,"arrays as one JSON document per line."]
  >> exitFailure

main :: IO ()
main =
 do args <- getArgs
    when ("--help"`elem`args) usage
    L.putStr                  -- printing the output
     . L8.unlines             -- joining lines
     . map encode             -- showing each item
     . concat                 -- merge lines from all files
     . map (                  -- for each file
         jsonUnlines          -- main processing
         . maybe err id       -- error handling
         . decode'            -- parsing each file as JSON
       ) =<< readFilesL args  -- reading the input
  where err = error "JSON decoding error"
