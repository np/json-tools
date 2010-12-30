import Text.JSON hiding (decode)
import Text.JSON.Parsec (p_value)
import Text.ParserCombinators.Parsec (parse)
import qualified System.IO.UTF8 as UTF8

decode :: String -> Result JSValue
decode = either (Error . show) Ok . parse p_value "<input>"

main :: IO ()
main = UTF8.putStr . encode . jsonConcat . unRes . decodeStrict =<< UTF8.getContents
  where jsonConcat (JSArray a)  = JSArray (concatMap unJSArray a)
        jsonConcat (JSObject o) = error "unexpected non-JSON array"
        jsonConcat _            = error "impossible by decodeStrict post-condition"
        unJSArray (JSArray a)   = a
        unJSArray _             = error "unexpected non-JSON array"
        unRes (Ok r)            = r
        unRes (Error s)         = error $ "JSON decoding: " ++ s
