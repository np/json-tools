import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid
import Data.Maybe
import Data.Foldable hiding (mapM_)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Vector as V

-- NOTE that the map keys are not included

jsonStrings :: Value -> [T.Text]
jsonStrings x0 = appEndo (go x0) []
  where go (String s) = Endo (s:)
        go (Number _) = mempty
        go (Bool   _) = mempty
        go Null       = mempty
        go (Object m) = foldMap go m
        go (Array xs) = foldMap go xs

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
  L8.putStrLn . encode              -- printing the output
     . Array . V.fromList           -- building the result
     . map String                   -- building the elements
     . jsonStrings                  -- main processing
     . fromMaybe err                -- error handling
     . decode' =<< L.getContents    -- reading the input
  where err = error "JSON parsing"
