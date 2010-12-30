import Data.List (intercalate)
import Text.JSON hiding (decode)
import Text.JSON.Parsec (p_value)
import Text.ParserCombinators.Parsec (parse)
import qualified System.IO.UTF8 as UTF8

decode :: String -> Result JSValue
decode = either (Error . show) Ok . parse p_value "<input>"

main :: IO ()
main = UTF8.putStr . f . unRes . decodeStrict =<< UTF8.getContents
  where f (JSArray a)   = '[' : intercalate "\n," (map encode a) ++ "]"
        f (JSObject o)  = '{' : intercalate "\n," (map g $ fromJSObject o) ++ "}"
        f _             = error "impossible by decodeStrict post-condition"
        g (key, val)    = encode key ++ ':' : encode val
        unRes (Ok r)    = r
        unRes (Error s) = error $ "JSON decoding: " ++ s
