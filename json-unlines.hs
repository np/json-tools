import Text.JSON.AttoJSON
import qualified Data.ByteString.Char8 as S
import Control.Arrow (second)

main :: IO ()
main = S.putStr . showJSON          -- printing the output
     . JSArray                      -- building the result
     . map err                      -- error handling
     . map (second parseJSON)       -- parsing a line
     . zip [1::Integer ..]          -- number lines
     . S.lines                      -- cut into a list of lines
     =<< S.getContents              -- reading the input
  where
  err (ln, Left msg) = error $ "JSON parsing: (line "++show ln++"): "++msg
  err (_,  Right x)  = x
