import qualified Data.Map as M
import Text.JSON.AttoJSON
import System.Environment (getArgs)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

deepSelectKey :: S.ByteString -> JSValue -> [JSValue]
deepSelectKey key (JSArray vs)
  = concatMap (deepSelectKey key) vs
deepSelectKey key (JSObject o)
  = maybe (concatMap (deepSelectKey key) (map snd (M.toList o))) return . M.lookup key $ o
deepSelectKey _ _ = []

main :: IO ()
main = do
  args <- getArgs
  obj <- either fail return . parseJSON =<< S.getContents
  case args of
    [key] ->
      S.putStrLn . showJSON . JSArray $ deepSelectKey (S8.pack key) obj
    _ -> error "Usage: json-deep-select-key <key>"

