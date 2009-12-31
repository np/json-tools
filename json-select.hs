import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad
import Control.Arrow
import Text.JSON
import System.Environment (getArgs)
import System.IO (stderr)
import qualified System.IO.UTF8 as UTF8

data JSContext = JSTip
               | JSArrayCxt [JSValue] JSContext [JSValue]
               | JSObjectCxt String JSContext (JSObject JSValue)
  deriving (Show)

data JSSegment = JSArrayIndex Int
               | JSObjectKey  String
type JSPath = [JSSegment]

pathFromContext :: JSContext -> JSPath
pathFromContext = go []
  where
    go :: JSPath -> JSContext -> JSPath
    go acc JSTip                     = acc
    go acc (JSArrayCxt prefix cxt _) = (go $! JSArrayIndex (length prefix) : acc) cxt
    go acc (JSObjectCxt f cxt _)     = (go $! JSObjectKey f : acc) cxt
 
selectJSONSegment :: JSSegment -> (JSContext, JSValue) -> (JSContext, Result JSValue)
selectJSONSegment (JSArrayIndex ix) (cxt, JSArray vs)
  = case splitAt ix vs of
      (prefix, v : suffix) -> (JSArrayCxt prefix cxt suffix, Ok v)
      (prefix, [])         -> (JSArrayCxt prefix cxt [],     Error msg)
  where msg = "An array with an index "++show ix++" was expected"
selectJSONSegment (JSObjectKey   f) (cxt, JSObject o)
  = maybe (cxt, Error msg) ok . lookup f $ assoc
  where msg   = "An object with a field "++show f++" was expected"
        ok x  = (JSObjectCxt f cxt (toJSObject (delete (f, x) assoc)), Ok x)
        assoc = fromJSObject o
selectJSONSegment (JSArrayIndex  _) (cxt, _)
  = (cxt, Error "An array was expected")
selectJSONSegment (JSObjectKey   _) (cxt, _)
  = (cxt, Error "An object was expected")

  {-
selectJSONSegment :: JSValue -> JSSegment -> Result JSValue
selectJSONSegment (JSArray vs) (JSArrayIndex ix)
  = maybe (Error msg) Ok . listToMaybe $ drop ix vs
  where msg = "An array with an index "++show ix++" was expected"
selectJSONSegment (JSObject o) (JSObjectKey   f)
  = maybe (Error msg) Ok . lookup f . fromJSObject $ o
  where msg = "An object with a field "++show f++" was expected"
selectJSONSegment _            (JSArrayIndex  _)
  = Error "An array was expected"
selectJSONSegment _            (JSObjectKey   _)
  = Error "An object was expected"
  -}

class MonadSnd m where
  bindSnd :: (a, m b) -> ((a, b) -> (a, m c)) -> (a, m c)

{-
  not used

returnSnd :: Monad m => (a, b) -> (a, m b)
returnSnd = second return

joinSnd :: Monad m => (a, m (m b)) -> (a, m b)
joinSnd = second join
-}

instance MonadSnd Result where
  (a, Ok v)      `bindSnd` f = f (a, v)
  (a, Error msg) `bindSnd` _ = (a, Error msg)

selectJSON :: (JSContext, JSValue) -> JSPath -> (JSContext, Result JSValue)
selectJSON = foldl (flip (flip bindSnd . selectJSONSegment)) . second return

readJSSegment :: String -> (JSSegment, String)
readJSSegment xs@('"':_) = case reads xs of
                             [r] -> first JSObjectKey r
                             _   -> error "Parse error: malformed string (when reading path segment)"
readJSSegment xs         = case reads xs of
                             [r] -> first JSArrayIndex r
                             _   -> error "Parse error: malformed integer (when reading path segment)"

readJSPath :: String -> JSPath
readJSPath []       = []
readJSPath ['/']    = []
readJSPath ('/':xs) = let (seg, ys) = readJSSegment xs in seg : readJSPath ys
readJSPath (x:_)    = error $ "Parse error: unexpected char "++show x++", '/' was expected"

showJSPath :: JSPath -> ShowS
showJSPath = (('/':) .) . mconcat . intersperse ('/':) . map showJSSegment

showJSSegment :: JSSegment -> ShowS
showJSSegment (JSObjectKey f)   = shows f
showJSSegment (JSArrayIndex ix) = shows ix

toMonad :: Monad m => Result t -> m t
toMonad (Ok x) = return x
toMonad (Error e) = fail e

-- should already exists
distrEitherPair :: (a, Either b c) -> Either (a, b) (a, c)
distrEitherPair (a, Left  b) = Left  (a, b)
distrEitherPair (a, Right c) = Right (a, c)

main :: IO ()
main = do
  [path] <- getArgs
  obj <- toMonad . decode =<< getContents
  either err (UTF8.putStrLn . encode . snd) . distrEitherPair . second resultToEither $ selectJSON (JSTip, obj) (readJSPath path)
  where err (cxt, msg) = UTF8.hPutStrLn stderr $ (showString msg . showString "\nLocation in the structure: " . showJSPath (pathFromContext cxt)) ""

