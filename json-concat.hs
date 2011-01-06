import Text.JSON.AttoJSON
import qualified Data.ByteString as S

main :: IO ()
main = S.putStr . showJSON . jsonConcat . either err id . parseJSON =<< S.getContents
  where jsonConcat (JSArray a)  = JSArray (concatMap unJSArray a)
        jsonConcat (JSObject _) = error "unexpected non-JSON array"
        jsonConcat _            = error "impossible by decodeStrict post-condition"
        unJSArray (JSArray a)   = a
        unJSArray _             = error "unexpected non-JSON array"
        err s                   = error $ "JSON decoding: " ++ s
