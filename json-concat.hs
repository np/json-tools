import Text.JSON.AttoJSON
import qualified Data.ByteString as S
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs)

usage :: IO a
usage = mapM_ (hPutStrLn stderr)
  ["Usage: json-concat [--help]"
  ,""
  ,"Reads the standard input as a JSON array of arrays, and writes"
  ,"on standard output the concatenations of these arrays as a JSON array."]
  >> exitFailure

main :: IO ()
main = do args <- getArgs
          when (not . null $ args) usage
          S.putStrLn . showJSON . jsonConcat . either err id . parseJSON
            =<< S.getContents
  where jsonConcat (JSArray a)  = JSArray (concatMap unJSArray a)
        jsonConcat (JSObject _) = error "unexpected non-JSON array"
        jsonConcat _            = error "not a JSON document (neither array, nor object)"
        unJSArray (JSArray a)   = a
        unJSArray _             = error "unexpected non-JSON array"
        err s                   = error $ "JSON decoding: " ++ s
