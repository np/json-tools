import Text.JSON.AttoJSON
import qualified Data.ByteString as S
import Data.Monoid
import Data.Foldable

-- NOTE that the map keys are not included

jsonStrings :: JSValue -> [S.ByteString]
jsonStrings x0 = appEndo (go x0) []
  where go (JSString s) = Endo (s:)
        go (JSNumber _) = mempty
        go (JSBool   _) = mempty
        go JSNull       = mempty
        go (JSObject m) = foldMap go m
        go (JSArray xs) = foldMap go xs

main :: IO ()
main = S.putStr . showJSON          -- printing the output
     . JSArray . map JSString       -- building the result
     . jsonStrings                  -- main processing
     . either err id                -- error handling
     . parseJSON =<< S.getContents  -- reading the input
  where err = error . ("JSON parsing: "++)
