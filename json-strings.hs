import Text.JSON.AttoJSON
import qualified Data.ByteString as S
import Data.Monoid
import Data.Foldable hiding (mapM_)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- NOTE that the map keys are not included

jsonStrings :: JSValue -> [S.ByteString]
jsonStrings x0 = appEndo (go x0) []
  where go (JSString s) = Endo (s:)
        go (JSNumber _) = mempty
        go (JSBool   _) = mempty
        go JSNull       = mempty
        go (JSObject m) = foldMap go m
        go (JSArray xs) = foldMap go xs

usage :: IO a
usage = mapM_ (hPutStrLn stderr)
  ["Usage: json-strings [--help]"
  ,""
  ,"Reads a JSON document as standard input and writes a JSON array"
  ,"of all the strings found in the documents (expect object/mapping keys)."]
  >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  when (not . null $ args) usage
  S.putStrLn . showJSON             -- printing the output
     . JSArray . map JSString       -- building the result
     . jsonStrings                  -- main processing
     . either err id                -- error handling
     . parseJSON =<< S.getContents  -- reading the input
  where err = error . ("JSON parsing: "++)
