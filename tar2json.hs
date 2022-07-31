import Codec.Archive.Tar as Tar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.String.Conversions (cs)
import qualified Data.ByteString.Lazy as L
import System.IO

{-TODO currently does not work for binary files-}

tar2json :: Tar.Entries FormatError -> Aeson.Value
tar2json es0 = Aeson.Object $ KM.fromList [(k,v) | (k,Just v) <- entries es0] where

  entries :: Tar.Entries FormatError -> [(Aeson.Key, Maybe Aeson.Value)]
  entries Done         = []
  entries (Fail s)     = error . show $ s
  entries (Next e es)  = (path e, contents e) : entries es

  path :: Tar.Entry -> Aeson.Key
  path = K.fromString. entryPath

  contents :: Tar.Entry -> Maybe Aeson.Value
  contents = fmap (Aeson.String . cs) . f . entryContent where
    f (NormalFile s _) = Just s
    f _                = Nothing

main :: IO ()
main
 = do hPutStrLn stderr "Reading standard input..."
      hSetEncoding stdin latin1
      hSetEncoding stdout latin1
      L.putStr            -- printing output
       . Aeson.encode     -- producing JSON
       . tar2json         -- main translation
       . Tar.read         -- reading the archive
       =<< L.getContents  -- reading the input
