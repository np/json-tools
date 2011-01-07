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
