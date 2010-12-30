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

err :: String -> a
err s = error $ "JSON decoding: " ++ s

decodeFile :: FilePath -> IO (Either String JSValue)
decodeFile fp = parseJSON <$> S.readFile fp

parseJSONFiles :: [FilePath] -> IO [JSValue]
parseJSONFiles [] = (:[]) . either err id . parseJSON <$> S.getContents
parseJSONFiles xs = mapM (unsafeInterleaveIO . fmap (either err id) . decodeFile) xs

systemWithStdin :: String -> S.ByteString -> IO ExitCode
systemWithStdin shellCmd input = do
  (Just stdinHdl, _, _, pHdl) <-
     P.createProcess (P.shell shellCmd){ P.std_in = P.CreatePipe }
  handle (\(_ :: IOException) -> return ()) $ do
    S.hPutStr stdinHdl input
    hClose stdinHdl
  P.waitForProcess pHdl

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
