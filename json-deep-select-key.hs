import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Data.Text as T
import System.Environment (getArgs)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

deepSelectKey :: T.Text -> Value -> [Value]
deepSelectKey key (Array vs)
  = concatMap (deepSelectKey key) (V.toList vs)
deepSelectKey key (Object o)
  = maybe (concatMap (deepSelectKey key) (map snd (HM.toList o))) return
  . HM.lookup key $ o
deepSelectKey _ _ = []

main :: IO ()
main = do
  args <- getArgs
  obj <- maybe (fail "JSON decoding") return . decode' =<< L.getContents
  case args of
    [key] ->
      L8.putStrLn . encode . Array . V.fromList $ deepSelectKey (T.pack key) obj
    _ -> error "Usage: json-deep-select-key <key>"

