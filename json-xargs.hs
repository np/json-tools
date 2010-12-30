import Text.JSON.AttoJSON
import qualified Data.ByteString.Char8 as S
import Data.Monoid
import Data.Foldable
import Data.Functor
import System.Environment
import System.Exit
import System.Cmd (rawSystem)

-- NOTE that the map keys are not included

jsonArgs :: JSValue -> [String]
jsonArgs x0 = appEndo (go x0) []
  where go (JSString s) = f (S.unpack s)
        go (JSNumber n) = f (show n)
        go (JSBool   b) = f (if b then "true" else "false")
        go JSNull       = f "null"
        go (JSObject m) = foldMap go m
        go (JSArray xs) = foldMap go xs
        f x = Endo (x:)

main :: IO ()
main = do cmd:argv <- getArgs
          args <- jsonArgs                     -- main processing
                . either err id                -- error handling
                . parseJSON <$> S.getContents  -- reading the input
          exitWith =<< rawSystem cmd (argv ++ args)
  where err = error . ("JSON parsing: "++)
