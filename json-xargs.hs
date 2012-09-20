import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Foldable
import Data.Functor
import System.Environment
import System.Exit
import System.Cmd (rawSystem)

-- NOTE that the map keys are not included

jsonArgs :: Value -> [String]
jsonArgs x0 = appEndo (go x0) []
  where go (String s) = f (T.unpack s)
        go (Number n) = f (show n)
        go (Bool   b) = f (if b then "true" else "false")
        go Null       = f "null"
        go (Object m) = foldMap go m
        go (Array xs) = foldMap go xs
        f x = Endo (x:)

main :: IO ()
main = do cmd:argv <- getArgs
          args <- jsonArgs                   -- main processing
                . fromMaybe err              -- error handling
                . decode' <$> L.getContents  -- reading the input
          exitWith =<< rawSystem cmd (argv ++ args)
  where err = error "Invalid JSON input"
