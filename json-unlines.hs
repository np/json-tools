{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,stderr)
import Utils (readFilesL)

usage :: IO a
usage = mapM_ (hPutStrLn stderr)
  ["Usage: json-unline [--help] <file>*"
  ,""
  ,"Reads the given files line by line (or standard input, if '-' or"
  ,"no file is given). Writes on standard output the a JSON array"
  ,"made of the JSON document found on each line."]
  >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  when ("--help"`elem`args) usage
  L.putStrLn . L.cons '[' . L.intercalate "\n," . L.lines . L.concat
    =<< readFilesL args
  L.putStrLn "]"
