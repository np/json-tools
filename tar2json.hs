import Codec.Archive.Tar as Tar
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import System.IO

tar2json :: Tar.Entries FormatError -> Aeson.Value
tar2json es0 = Aeson.Object $ HM.fromList [(k,v) | (k,Just v) <- entries es0] where

  entries :: Tar.Entries FormatError -> [(T.Text, Maybe Aeson.Value)]
  entries Done         = []
  entries (Fail s)     = error . show $ s
  entries (Next e es)  = (path e, contents e) : entries es

  path :: Tar.Entry -> T.Text
  path = T.pack . entryPath

  contents :: Tar.Entry -> Maybe Aeson.Value
  contents = fmap Aeson.toJSON . f . entryContent where
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
