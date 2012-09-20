{-# LANGUAGE ScopedTypeVariables #-}
module Utils
  ( readFiles, decodeFile, parseJSONFiles
  , systemWithStdin)
where

import Data.Aeson
import Data.Maybe
import Data.Functor
import Control.Exception
import qualified Data.ByteString.Lazy as L
import System.IO (hClose)
import System.Exit (ExitCode)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Process as P

readFiles :: [String] -> IO [L.ByteString]
readFiles args
  | null args = fmap (:[]) L.getContents
  | otherwise = mapM file args
  where file "-" = L.getContents
        file xs  = L.readFile xs

decodeFile :: FilePath -> IO (Maybe Value)
decodeFile fp = decode' <$> L.readFile fp

decErr :: a
decErr = error "JSON decoding"

parseJSONFiles :: [FilePath] -> IO [Value]
parseJSONFiles [] = (:[]) . fromMaybe decErr . decode' <$> L.getContents
parseJSONFiles xs = mapM (unsafeInterleaveIO . fmap (fromMaybe decErr) . decodeFile) xs

systemWithStdin :: String -> L.ByteString -> IO ExitCode
systemWithStdin shellCmd input = do
  (Just stdinHdl, _, _, pHdl) <-
     P.createProcess (P.shell shellCmd){ P.std_in = P.CreatePipe }
  handle (\(_ :: IOException) -> return ()) $ do
    L.hPutStr stdinHdl input
    hClose stdinHdl
  P.waitForProcess pHdl
