{-# LANGUAGE OverloadedStrings #-}
import Data.List (intersperse)
import Data.Map (toList)
import Data.Monoid
import Text.JSON.AttoJSON
import qualified Data.ByteString as S

-- wrap, as in line-wrapping

main :: IO ()
main = mapM_ S.putStrLn . ($[]) . f . either err id . parseJSON =<< S.getContents
  where f (JSArray a)   = t"[" . cat (intersperse (t"\n,") (map (t . showJSON) a)) . t"]"
        f (JSObject o)  = t"{" . cat (intersperse (t"\n,") (map g . toList $ o)) . t"}"
        f _             = error "impossible by decodeStrict post-condition"
        g (key, val)    = t key . t ":" . (t . showJSON) val
        err s           = error $ "JSON decoding: " ++ s
        t x             = (x:)
        cat             = appEndo . mconcat . map Endo
