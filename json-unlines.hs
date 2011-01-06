{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as L
import System.Environment

-- utility function
getAllContents :: IO L.ByteString
getAllContents =
  do args <- getArgs
     if null args
      then L.getContents
      else fmap L.concat . mapM file $ args
  where file "-" = L.getContents
        file xs  = L.readFile xs

main :: IO ()
main = do
  L.putStrLn . L.cons '[' . L.intercalate "\n," . L.lines =<< getAllContents
  L.putStrLn "]"
