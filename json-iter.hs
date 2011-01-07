{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Exception
import System.Environment (getArgs)
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Process as P
import Text.JSON.AttoJSON
import qualified Data.ByteString as S
import Utils (parseJSONFiles, systemWithStdin)

unSequence :: JSValue -> [JSValue]
unSequence (JSArray a) = a
unSequence _           = error "unSequence: a JSON sequence was expected"

iterJSON :: String -> [String] -> IO ()
iterJSON cmd jsonfiles =
  mapM_ (mapM_ (systemWithStdin cmd . showJSON) . unSequence)
    =<< parseJSONFiles jsonfiles

main :: IO ()
main = do
  cmd:args <- getArgs
  iterJSON cmd args
