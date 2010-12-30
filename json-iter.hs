{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Exception
import System.Environment (getArgs)
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Process as P
import Text.JSON hiding (decode)
import Text.JSON.Parsec (p_value)
import Text.ParserCombinators.Parsec (parse)
import qualified System.IO.UTF8 as UTF8

unRes :: Result b -> b
unRes (Ok r)    = r
unRes (Error s) = error $ "JSON decoding: " ++ s

decode :: String -> Result JSValue
decode = either (Error . show) Ok . parse p_value "<input>"


decodeFile :: FilePath -> IO (Result JSValue)
decodeFile fp = decode <$> UTF8.readFile fp

parseJSONFiles :: [FilePath] -> IO [JSValue]
parseJSONFiles [] = (:[]) . unRes . decodeStrict <$> getContents
parseJSONFiles xs = mapM (unsafeInterleaveIO . fmap unRes . decodeFile) xs

systemWithStdin :: String -> String -> IO ExitCode
systemWithStdin shellCmd input = do
  (Just stdinHdl, _, _, pHdl) <-
     P.createProcess (P.shell shellCmd){ P.std_in = P.CreatePipe }
  handle (\(_ :: IOException) -> return ()) $ do
    UTF8.hPutStr stdinHdl input
    hClose stdinHdl
  P.waitForProcess pHdl

unSequence :: JSValue -> [JSValue]
unSequence (JSArray a) = a
unSequence _           = error "unSequence: a JSON sequence was expected"

iterJSON :: String -> [String] -> IO ()
iterJSON cmd jsonfiles =
  mapM_ (mapM_ (systemWithStdin cmd . encode) . unSequence)
    =<< parseJSONFiles jsonfiles

main :: IO ()
main = do
  cmd:args <- getArgs
  iterJSON cmd args
