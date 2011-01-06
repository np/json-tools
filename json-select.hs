import Data.List
import Data.Monoid
import qualified Data.Map as M
import Control.Arrow
import Text.JSON.AttoJSON
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit
import qualified Data.ByteString as S

type JSObject a = M.Map S.ByteString a

data JSFrame = JSArrayFrm [JSValue] [JSValue]
             | JSObjectFrm S.ByteString (JSObject JSValue)
  deriving (Show)

type JSContext = [JSFrame]

data JSSegment = JSArrayIndex Int
               | JSObjectKey  S.ByteString
type JSPath = [JSSegment]

type JSMultiPath = [Maybe JSSegment] -- Nothing means '*'

type JSCxtValue = (JSContext, JSValue)

segmentFromFrame :: JSFrame -> JSSegment
segmentFromFrame (JSArrayFrm prefix _) = JSArrayIndex (length prefix)
segmentFromFrame (JSObjectFrm f _)     = JSObjectKey f

pathFromContext :: JSContext -> JSPath
pathFromContext = map segmentFromFrame

selectJSONSegment :: JSSegment -> JSValue -> Either String (JSFrame, JSValue)
selectJSONSegment (JSArrayIndex ix) (JSArray vs)
  = case splitAt ix vs of
      (prefix, v : suffix) -> Right (JSArrayFrm prefix suffix, v)
      (_, [])              -> Left msg
  where msg = "An array with an index "++show ix++" was expected"
selectJSONSegment (JSObjectKey   f) (JSObject o)
  = maybe (Left msg) ok . M.lookup f $ o
  where msg   = "An object with a field "++show f++" was expected"
        ok x  = Right (JSObjectFrm f (M.delete f o), x)
selectJSONSegment (JSArrayIndex  _) _
  = Left "An array was expected"
selectJSONSegment (JSObjectKey   _) _
  = Left "An object was expected"

selectJSON :: JSCxtValue -> JSPath -> (JSContext, Either String JSValue)
selectJSON (cxt,val) [] = (cxt, Right val)
selectJSON (cxt,val) (seg:path) =
  either ((,)cxt . Left) (flip selectJSON path . first (:cxt)) (selectJSONSegment seg val)

zips :: ([a] -> a -> [a] -> b) -> [a] -> [b]
zips _ [] = error "zips: empty list"
zips f (y : ys) = go [] y ys
  where go sx x []                = [f (reverse sx) x []]
        go sx x xs@(xs'x : xs'xs) = f (reverse sx) x xs : go (x : sx) xs'x xs'xs

allSubValues :: JSCxtValue -> [JSCxtValue]
allSubValues (_,   JSArray []) = []
allSubValues (cxt, JSArray vs) = zips f vs
  where f prefix value suffix = (JSArrayFrm prefix suffix : cxt, value)
allSubValues (cxt, JSObject o) = map (first f) . M.toList $ o
  where f key = JSObjectFrm key (M.delete key o) : cxt
allSubValues _ = []

selectJSONMaybeSegment :: Maybe JSSegment -> JSCxtValue -> [JSCxtValue]
selectJSONMaybeSegment (Just seg) (cxt,val) = either (const []) (return . first (:cxt)) $ selectJSONSegment seg val
selectJSONMaybeSegment Nothing    cxtval = allSubValues cxtval

selectMultiJSON :: JSCxtValue -> JSMultiPath -> [JSCxtValue]
selectMultiJSON cxtval []        = return cxtval
selectMultiJSON cxtval (mp : ps) = selectJSONMaybeSegment mp cxtval >>= flip selectMultiJSON ps

type Reader a = String -> (a, String)

readJSSegment :: Reader JSSegment
readJSSegment xs@('"':_) = case reads xs of -- TODO maybe we should use the JSON syntax for literal strings
                             [r] -> first JSObjectKey r
                             _   -> error "Parse error: malformed string (when reading path segment)"
readJSSegment xs         = case reads xs of
                             [r] -> first JSArrayIndex r
                             _   -> error "Parse error: malformed integer (when reading path segment)"

readJSMultiSegment :: Reader (Maybe JSSegment)
readJSMultiSegment ('*':xs) = (Nothing, xs)
readJSMultiSegment xs       = first Just . readJSSegment $ xs

readJSPathGen :: Reader a -> String -> [a]
readJSPathGen _ []       = []
readJSPathGen _ ['/']    = []
readJSPathGen r ('/':xs) = let (seg, ys) = r xs in seg : readJSPathGen r ys
readJSPathGen _ (x:_)    = error $ "Parse error: unexpected char "++show x++", '/' was expected"

readJSPath :: String -> JSPath
readJSPath = readJSPathGen readJSSegment

readJSMultiPath :: String -> JSMultiPath
readJSMultiPath = readJSPathGen readJSMultiSegment

showJSPath :: JSPath -> ShowS
showJSPath = (('/':) .) . mconcat . intersperse ('/':) . map showJSSegment

showJSSegment :: JSSegment -> ShowS
showJSSegment (JSObjectKey f)   = shows f
showJSSegment (JSArrayIndex ix) = shows ix

-- should already exists
distrEitherPair :: (a, Either b c) -> Either (a, b) (a, c)
distrEitherPair (a, Left  b) = Left  (a, b)
distrEitherPair (a, Right c) = Right (a, c)

usage :: String -> IO a
usage msg = mapM_ (hPutStrLn stderr)
  ["Usage: json-select [-m] <path>"
  ,""
  ,"path ::=                        # A path can be empty"
  ,"       | '/' <segment> <path>   # Chain a path segment and a path"
  ,""
  ,"segement ::= '\"' <char>* '\"'    # Access the object/mapping at the given key"
  ,"           | [ '0' - '9' ]*     # Access the array/sequence at the given index"
  ,"           | '*'                # Keep all children of the given node (requires -m)"
  ,""
  ,msg] >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  let pobj = either fail return . parseJSON =<< S.getContents
  case args of
    [] -> usage "Too few arguments"
    ["-m", path] ->
      do obj <- pobj
         S.putStrLn . showJSON . JSArray . map snd . selectMultiJSON ([], obj) . readJSMultiPath $ path
    [path] ->
      do obj <- pobj
         either err (S.putStrLn . showJSON . snd) . distrEitherPair $ selectJSON ([], obj) (readJSPath path)
      where err (cxt, msg) = hPutStrLn stderr $ (showString msg . showString "\nLocation in the structure: " . showJSPath (pathFromContext cxt)) ""
    _ -> usage "Too many arguments"

