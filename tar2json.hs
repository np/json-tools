import Codec.Archive.Tar as Tar
import Data.Convertible.Base (convertSuccess)
import Data.Object
import Data.Object.Json as Json
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import System.IO

tar2json :: Tar.Entries -> JsonObject
tar2json es0 = Mapping [(k,v) | (k,Just v) <- entries es0] where

  entries :: Tar.Entries -> [(S.ByteString, Maybe JsonObject)]
  entries Done         = []
  entries (Fail s)     = error s
  entries (Next e es)  = (path e, contents e) : entries es

  path :: Tar.Entry -> S.ByteString
  path = S8.pack {- Is this an encoding mistake? -} . entryPath

  contents :: Tar.Entry -> Maybe JsonObject
  contents = fmap (Scalar . JsonString) . f . entryContent where
    f (NormalFile s _) = Just (convertSuccess s)
    f _                = Nothing

main :: IO ()
main
 = do hPutStrLn stderr "Reading standard input..."
      hSetEncoding stdin latin1
      hSetEncoding stdout latin1
      S.putStr            -- printing output
       . Json.encode      -- producing JSON
       . tar2json         -- main translation
       . Tar.read         -- reading the archive
       =<< L.getContents  -- reading the input
