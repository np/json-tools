import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Aeson
import System.Environment (getArgs)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

deepSelectKey :: Key -> Value -> V.Vector Value
deepSelectKey key (Array vs)
  = V.concatMap (deepSelectKey key) vs
deepSelectKey key (Object o)
  = maybe (V.concatMap (deepSelectKey key) (V.fromList $ KM.elems o)) return
  . KM.lookup key $ o
deepSelectKey _ _ = V.empty

main :: IO ()
main = do
  args <- getArgs
  obj <- maybe (fail "JSON decoding") return . decode' =<< L.getContents
  case args of
    [key] ->
      L8.putStrLn . encode . Array $ deepSelectKey (K.fromString key) obj
    _ -> error "Usage: json-deep-select-key <key>"

