import Text.JSON.AttoJSON
import qualified Data.ByteString.Char8 as S

jsonUnlines :: JSValue -> [JSValue]
jsonUnlines (JSArray xs) = xs
jsonUnlines _            = error "Unexpected JSON value, only an array was expected"

main :: IO ()
main = S.putStr                     -- printing the output
     . S.unlines                    -- joining lines
     . map showJSON                 -- showing each item
     . jsonUnlines                  -- main processing
     . either err id                -- error handling
     . parseJSON =<< S.getContents  -- reading the input
  where err msg = error $ "JSON parsing: "++msg
