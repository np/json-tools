{-# LANGUAGE ScopedTypeVariables #-}
module Utils
  ( readFilesL, readFilesS, decodeFile, parseJSONFiles
  , systemWithStdin)
where

import Text.JSON.AttoJSON
import Data.Functor
import Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.IO (hClose)
import System.Exit (ExitCode)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Process as P

readFilesL :: [String] -> IO [L.ByteString]
readFilesL args
  | null args = fmap (:[]) L.getContents
  | otherwise = mapM file args
  where file "-" = L.getContents
        file xs  = L.readFile xs

readFilesS :: [String] -> IO [S.ByteString]
readFilesS args
  | null args = fmap (:[]) S.getContents
  | otherwise = mapM file args
  where file "-" = S.getContents
        file xs  = S.readFile xs

decodeFile :: FilePath -> IO (Either String JSValue)
decodeFile fp = parseJSON <$> S.readFile fp

decErr :: String -> a
decErr s = error $ "JSON decoding: " ++ s

parseJSONFiles :: [FilePath] -> IO [JSValue]
parseJSONFiles [] = (:[]) . either decErr id . parseJSON <$> S.getContents
parseJSONFiles xs = mapM (unsafeInterleaveIO . fmap (either decErr id) . decodeFile) xs

systemWithStdin :: String -> S.ByteString -> IO ExitCode
systemWithStdin shellCmd input = do
  (Just stdinHdl, _, _, pHdl) <-
     P.createProcess (P.shell shellCmd){ P.std_in = P.CreatePipe }
  handle (\(_ :: IOException) -> return ()) $ do
    S.hPutStr stdinHdl input
    hClose stdinHdl
  P.waitForProcess pHdl
