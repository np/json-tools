{-# LANGUAGE OverloadedStrings #-}
import Data.List (intersperse)
import Data.Monoid
import Data.Aeson
import Data.Maybe
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

-- wrap, as in line-wrapping

main :: IO ()
main = mapM_ L.putStr . ($["\n"]) . f . fromMaybe err . decode' =<< L.getContents
  where f (Array a)   = t"[" . cat (intersperse (t"\n,") (map j . V.toList $ a)) . t"]"
        f (Object o)  = t"{" . cat (intersperse (t"\n,") (map g . KM.toList $ o)) . t"}"
        f _           = error "impossible by decodeStrict post-condition"
        g (key, val)  = t (encode key) . t(L8.pack ":") . j val
        j             = t . encode
        err           = error "JSON decoding"
        t x           = (x:)
        cat           = appEndo . mconcat . map Endo
