module Utils where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

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

parseJSONFiles :: [FilePath] -> IO [JSValue]
parseJSONFiles [] = (:[]) . either err id . parseJSON <$> S.getContents
parseJSONFiles xs = mapM (unsafeInterleaveIO . fmap (either err id) . decodeFile) xs
  where err :: String -> a
        err s = error $ "JSON decoding: " ++ s

systemWithStdin :: String -> S.ByteString -> IO ExitCode
systemWithStdin shellCmd input = do
  (Just stdinHdl, _, _, pHdl) <-
     P.createProcess (P.shell shellCmd){ P.std_in = P.CreatePipe }
  handle (\(_ :: IOException) -> return ()) $ do
    S.hPutStr stdinHdl input
    hClose stdinHdl
  P.waitForProcess pHdl
