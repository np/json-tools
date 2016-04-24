import Data.List
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import Control.Arrow
import Data.Aeson
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Vector as V

data Frame = ArrayFrm Array Array
           | ObjectFrm Text Object
  deriving (Show)

type Context = [Frame]

data Segment = ArrayIndex Int
             | ObjectKey  Text
type Path = [Segment]

type MultiPath = [Maybe Segment] -- Nothing means '*'

type CxtValue = (Context, Value)

segmentFromFrame :: Frame -> Segment
segmentFromFrame (ArrayFrm prefix _) = ArrayIndex (V.length prefix)
segmentFromFrame (ObjectFrm f _)     = ObjectKey f

pathFromContext :: Context -> Path
pathFromContext = map segmentFromFrame

selectJSONSegment :: Segment -> Value -> Either String (Frame, Value)
selectJSONSegment (ArrayIndex ix) (Array vs)
  = case V.splitAt ix vs of
      (prefix, suffix)
        | V.null suffix -> Left msg
        | otherwise     -> Right (ArrayFrm prefix (V.tail suffix), V.head suffix)
  where msg = "An array with an index "++show ix++" was expected"
selectJSONSegment (ObjectKey f) (Object o)
  = maybe (Left msg) ok . HM.lookup f $ o
  where msg   = "An object with a field "++show f++" was expected"
        ok x  = Right (ObjectFrm f (HM.delete f o), x)
selectJSONSegment (ArrayIndex  _) _
  = Left "An array was expected"
selectJSONSegment (ObjectKey   _) _
  = Left "An object was expected"

selectJSON :: CxtValue -> Path -> (Context, Either String Value)
selectJSON (cxt,val) [] = (cxt, Right val)
selectJSON (cxt,val) (seg:path) =
  either ((,)cxt . Left) (flip selectJSON path . first (:cxt)) (selectJSONSegment seg val)

zips :: ([a] -> a -> [a] -> b) -> [a] -> [b]
zips _ [] = error "zips: empty list"
zips f (y : ys) = go [] y ys
  where go sx x []                = [f (reverse sx) x []]
        go sx x xs@(xs'x : xs'xs) = f (reverse sx) x xs : go (x : sx) xs'x xs'xs

allSubValues :: CxtValue -> [CxtValue]
allSubValues (cxt, Array vs)
    | V.null vs = []
    | otherwise = zips f (V.toList vs)
  where f prefix value suffix = (ArrayFrm (V.fromList prefix) (V.fromList suffix) : cxt, value)
allSubValues (cxt, Object o) = map (first f) . HM.toList $ o
  where f key = ObjectFrm key (HM.delete key o) : cxt
allSubValues _ = []

selectJSONMaybeSegment :: Maybe Segment -> CxtValue -> [CxtValue]
selectJSONMaybeSegment (Just seg) (cxt,val) = either (const []) (return . first (:cxt)) $ selectJSONSegment seg val
selectJSONMaybeSegment Nothing    cxtval = allSubValues cxtval

selectMultiJSON :: CxtValue -> MultiPath -> [CxtValue]
selectMultiJSON cxtval []        = return cxtval
selectMultiJSON cxtval (mp : ps) = selectJSONMaybeSegment mp cxtval >>= flip selectMultiJSON ps

type Reader a = String -> (a, String)

readJSSegment :: Reader Segment
readJSSegment xs@('"':_) = case reads xs of -- TODO maybe we should use the JSON syntax for literal strings
                             [r] -> first ObjectKey r
                             _   -> error "Parse error: malformed string (when reading path segment)"
readJSSegment xs         = case reads xs of
                             [r] -> first ArrayIndex r
                             _   -> error "Parse error: malformed integer (when reading path segment)"

readJSMultiSegment :: Reader (Maybe Segment)
readJSMultiSegment ('*':xs) = (Nothing, xs)
readJSMultiSegment xs       = first Just . readJSSegment $ xs

readJSPathGen :: Reader a -> String -> [a]
readJSPathGen _ []       = []
readJSPathGen _ ['/']    = []
readJSPathGen r ('/':xs) = let (seg, ys) = r xs in seg : readJSPathGen r ys
readJSPathGen _ (x:_)    = error $ "Parse error: unexpected char "++show x++", '/' was expected"

readJSPath :: String -> Path
readJSPath = readJSPathGen readJSSegment

readJSMultiPath :: String -> MultiPath
readJSMultiPath = readJSPathGen readJSMultiSegment

showJSPath :: Path -> ShowS
showJSPath = (('/':) .) . mconcat . intersperse ('/':) . map showJSSegment

showJSSegment :: Segment -> ShowS
showJSSegment (ObjectKey f)   = shows f
showJSSegment (ArrayIndex ix) = shows ix

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
  ,"The same behavior can be obtained with jq (or hjq) following this translation:"
  ,""
  ,"path() = ."
  ,"path(/<segment><path>) = .[segment(<segment>)]"
  ,""
  ,"segment(\"<char>*\" as s) = s"
  ,"segment(['0'-'9']*  as n) = n"
  ,"segment('*')              = "
  ,""
  ,"Finally calling ./jq with following filter: [path(p)]"
  ,""
  ,"For instance the path /\"foo\"/\"$bar\"/*/42 becomes:"
  ,"  jq '[.foo[\"$bar\"][][42]]'"
  ,msg] >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  let pobj = maybe (fail "JSON decoding") return . decode' =<< L.getContents
  case args of
    [] -> usage "Too few arguments"
    ["-m", path] ->
      do obj <- pobj
         L8.putStrLn . encode . Array . V.fromList . map snd . selectMultiJSON ([], obj) . readJSMultiPath $ path
    [path] ->
      do obj <- pobj
         either err (L8.putStrLn . encode . snd) . distrEitherPair $ selectJSON ([], obj) (readJSPath path)
      where err (cxt, msg) = hPutStrLn stderr $ (showString msg . showString "\nLocation in the structure: " . showJSPath (pathFromContext cxt)) ""
    _ -> usage "Too many arguments"

