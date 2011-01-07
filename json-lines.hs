import Text.JSON.AttoJSON
import qualified Data.ByteString.Char8 as S
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,stderr)
import Utils (readFilesS)

jsonUnlines :: JSValue -> [JSValue]
jsonUnlines (JSArray xs) = xs
jsonUnlines _            = error "Unexpected JSON value, only an array was expected"

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
    S.putStr                  -- printing the output
     . S.unlines              -- joining lines
     . map showJSON           -- showing each item
     . concat                 -- merge lines from all files
     . map (                  -- for each file
         jsonUnlines          -- main processing
         . either err id      -- error handling
         . parseJSON          -- parsing each file as JSON
       ) =<< readFilesS args  -- reading the input
  where err msg = error $ "JSON parsing: "++msg
