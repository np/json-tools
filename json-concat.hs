import Data.Aeson
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
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
          L8.putStrLn . encode . jsonConcat . maybe err id . decode'
            =<< L.getContents
  where jsonConcat (Array a)  = Array (V.concatMap unArray a)
        jsonConcat (Object _) = error "unexpected non-JSON array"
        jsonConcat _          = error "not a JSON document (neither array, nor object)"
        unArray (Array a)     = a
        unArray _             = error "unexpected non-JSON array"
        err                   = error "JSON decoding error"
