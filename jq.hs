{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS -fno-warn-orphans #-}
import Control.Applicative as A
import Control.Arrow (first,second,(***))
import Control.Monad ((>=>), guard, when, join)
import Control.Monad.Reader
import Control.Exception (evaluate, try, SomeException)
import Data.Ord (comparing)
import Prelude hiding (filter,sequence,Ordering(..))
import Data.Maybe
import Data.Char
import Data.Functor
import Data.Foldable as Foldable
import Data.List ((\\),sort,sortBy,intersperse,nub)
import qualified Data.List as List
import Data.Monoid
import Data.Aeson hiding ((<?>))
import Data.Aeson.Parser (jstring, value)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Scientific hiding (scientific, toBoundedInteger)
import qualified Data.Scientific as Scientific
import Data.String (IsString)
import Data.String.Conversions (ConvertibleStrings, cs)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashSet as S
import Data.Attoparsec.ByteString.Char8 hiding (Result, parse, try, match)
import qualified Data.Attoparsec.Lazy as L
import Data.Attoparsec.Expr
import System.Environment
import Data.Traversable (Traversable(..),foldMapDefault)
import System.IO (stderr)
import System.IO.Unsafe
import System.Process (readProcess)
--import Debug.Trace (trace)

type Op1 v = v -> v
type Op2 v = v -> v -> v
type Op3 v = v -> v -> v -> v
type Op4 v = v -> v -> v -> v -> v

type ValueOp1 = Value -> Value
type ValueOp2 = Value -> Value -> Value
type ValueOp3 = Value -> Value -> Value -> Value
type BoolOp2 = Value -> Value -> Bool
type Filter = Value -> [Value]

newtype Obj a = Obj { unObj :: [(a,a)] }
  deriving (Eq, Show)

instance Functor Obj where
  fmap f = Obj . fmap (f *** f) . unObj

instance Traversable Obj where
  traverse f (Obj o) = Obj <$> traverse trPair o
    where trPair (x,y) = (,) <$> f x <*> f y

instance Foldable Obj where
  foldMap = foldMapDefault

data Kind = KNull | KNumber | KString | KBool | KArray | KObject
  deriving (Eq)

kinds :: [Kind]
kinds = [KNull, KNumber, KString, KBool, KArray, KObject]

instance Show Kind where
  show KNull = "null"
  show KNumber = "number"
  show KString = "string"
  show KBool = "boolean"
  show KArray = "array"
  show KObject = "object"

kindOf :: Value -> Kind
kindOf Null     = KNull
kindOf Number{} = KNumber
kindOf String{} = KString
kindOf Bool{}   = KBool
kindOf Array{}  = KArray
kindOf Object{} = KObject

err :: [String] -> a
err = error . unwords

err2 :: Value -> Value -> (String -> String -> [String]) -> a
err2 x y msg = err (msg (show (kindOf x)) (show (kindOf y)))

err3 :: Value -> Value -> Value -> (String -> String -> String -> [String]) -> a
err3 x y z msg = err (msg (show (kindOf x)) (show (kindOf y)) (show (kindOf z)))

err1 :: Value -> (String -> [String]) -> a
err1 x msg = err (msg (show (kindOf x)))

errK :: String -> [(Value,[Kind])] -> a
errK nm vks =
  err . head $
        [ "cannot call" : nm : "since argument" :
          show i : "is a" : show kv :
          "and not a" : intersperse "or a" (map show ks)
        | (i,(v,ks)) <- zip [1::Int ..] vks
        , let kv = kindOf v
        , kv `notElem` ks
        ]

vecDiff :: Vector Value -> Vector Value -> Vector Value
x `vecDiff` y = V.filter p x
  where p = not . (`S.member`s)
        s = S.fromList (V.toList y)

(+|), (-|), (/|), ( *| ), (%|) :: ValueOp2

Null     +| x        = x
x        +| Null     = x
Number x +| Number y = Number (x + y)
String x +| String y = String (x <> y)
Array  x +| Array  y = Array  (x <> y)
Object x +| Object y = Object (y <> x) -- Right biased
x        +| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be added"]

Number x -| Number y = Number (x - y)
Array  x -| Array  y = Array (x `vecDiff` y)
x        -| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be subtracted"]

Number x *| Number y = Number (x * y)
String x *| Number y = String (T.replicate (floor y) x)
                       -- This use of floor is consistent with jq
x        *| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be multiplied"]

Number x /| Number y = toJSON (Scientific.toRealFloat x /
                               Scientific.toRealFloat y :: Double)
                       -- Scientific.(/) has limitations hence the use of Double here.
String x /| String y = toJSON (T.splitOn y x)
x        /| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be divided"]

Number x %| Number y = Number (fromInteger $ floor x `rem` floor y)
x %| y = err2 x y $ \x' y' -> [x', "and", y', "cannot be 'mod'ed"]

newtype NValue = NValue Value
  deriving (Eq)

instance Ord NValue where
  NValue vx <= NValue vy =
    case (vx, vy) of
      (Null    , _       ) -> True
      (_       , Null    ) -> False
      (Bool   x, Bool y  ) -> x <= y
      (Bool   _, _       ) -> True
      (_       , Bool _  ) -> False
      (Number x, Number y) -> x <= y
      (Number _, _       ) -> True
      (_       , Number _) -> False
      (String x, String y) -> x <= y
      (String _, _       ) -> True
      (_       , String _) -> False
      (Array  x, Array  y) -> (NValue <$> x) <= (NValue <$> y)
      (Array  _, _       ) -> True
      (_       , Array _ ) -> False
      (Object x, Object y) -> x == y || f x <= f y
      where f = map (second NValue) . sortBy (comparing fst) . KM.toList

nvalueOp2 :: (NValue -> NValue -> a) -> Value -> Value -> a
nvalueOp2 f x y = f (NValue x) (NValue y)

boolOp2 :: BoolOp2 -> ValueOp2
boolOp2 f x y = Bool (f x y)

boolOp2' :: (Bool -> Bool -> Bool) -> ValueOp2
boolOp2' f x y = Bool (f (trueValue x) (trueValue y))

boolOp2Ord :: (NValue -> NValue -> Bool) -> ValueOp2
boolOp2Ord = boolOp2 . nvalueOp2

-- NOTE: As in jq length is the identity on numbers.
lengthFi :: Value -> Scientific
lengthFi Null       = 0
lengthFi (Array v)  = fromIntegral $ V.length v
lengthFi (Object o) = fromIntegral $ length . KM.toList $ o
lengthFi (String s) = fromIntegral $ T.length s
lengthFi (Number n) = n
lengthFi (Bool b)   = err1 (Bool b) $ \x' -> [x', "has no length"]

toString :: Value -> Text
toString (String s) = s
toString x = cs . encode $ x

toStringNoNull :: Value -> Text
toStringNoNull Null = ""
toStringNoNull x = toString x

iterKeysF :: Filter
iterKeysF (Array v)  = toJSON <$> [0.. V.length v - 1]
iterKeysF (Object o) = toJSON <$> sort (H.keys o)
iterKeysF x          = err1 x $ \x' -> [x', "has no keys"]

lengthOp, keysOp, addOp, negateOp, sqrtOp, floorOp, sortOp,
  uniqueOp, toNumberOp, toStringOp, fromjsonOp, linesOp, unlinesOp,
  wordsOp, unwordsOp, tailOp, initOp, reverseOp :: ValueOp1

lengthOp = Number . lengthFi

keysOp = toJSON . iterKeysF
{-
keysOp (Array v)  = toJSON [0.. V.length v - 1]
keysOp (Object o) = toJSON . sort . KM.keys $ o
keysOp x          = err1 x $ \x' -> [x', "has no keys"]
-}

addOp = foldr (+|) Null . toListF

negateOp (Number n) = Number (negate n)
negateOp x          = err1 x $ \x' -> [x', "cannot be negated"]

sqrtOp (Number n) = Number (fromFloatDigits (sqrt (toRealFloat n :: Double)))
sqrtOp x          = err1 x $ \x' -> [x', "has no square root"]

floorOp (Number n) = Number (fromInteger $ floor n)
floorOp x          = err1 x $ \x' -> [x', "cannot be floored"]

sortOp (Array v) = Array (V.fromList . sort . V.toList $ v)
sortOp x         = err1 x $ \x' -> [x', "cannot be sorted, as it is not an array"]

uniqueOp (Array v) = Array (V.fromList . nub . sort . V.toList $ v)
uniqueOp x         = err1 x $ \x' -> [x', "cannot be grouped, as it is not an array"]

toNumberOp n@Number{} = n
toNumberOp (String s) = either (const e) Number $ parseM "number" (top scientific) (cs s)
  where e = error $ "Invalid numeric literal (while parsing '" <> T.unpack s <> "'"
toNumberOp x     = err1 x $ \x' -> [x', "cannot be parsed as a number"]

toStringOp = String . toString

fromjsonOp (String s) = either error id . parseM "JSON value" (top value) . cs $ s
fromjsonOp x = errK "fromjson" [(x,[KString])]

linesOp (String s) = Array (V.fromList . map String . T.lines $ s)
linesOp x = errK "lines" [(x,[KString])]

unlinesOp (Array v) = String (T.unlines . map unString . V.toList $ v)
unlinesOp x = err1 x $ \x' -> ["cannot take unlines of", x', "(not an array of string)"]

wordsOp (String s) = Array (V.fromList . map String . T.words $ s)
wordsOp x = errK "words" [(x,[KString])]

unwordsOp (Array v) = String (T.unwords . map unString . V.toList $ v)
unwordsOp x = err1 x $ \x' -> ["cannot take unwords of", x', "(not an array of string)"]

tailOp (String s) = String (T.tail s)
tailOp (Array  v) = Array  (V.tail v)
tailOp x = errK "tail" [(x,[KString,KArray])]

initOp (String s) = String (T.init s)
initOp (Array  v) = Array  (V.init v)
initOp x = errK "init" [(x,[KString,KArray])]

reverseOp (String s) = String (T.reverse s)
reverseOp (Array  v) = Array  (V.reverse v)
reverseOp x = errK "reverse" [(x,[KString,KArray])]

unString :: Value -> Text
unString (String s) = s
unString x          = err1 x $ \x' -> [x', "is not a string"]

endoTextOp :: String -> (T.Text -> T.Text) -> ValueOp1
endoTextOp _    textOp (String s) = String (textOp s)
endoTextOp name _      x          = errK name [(x,[KString])]

cannotIndex :: Value -> Value -> a
cannotIndex x y = err2 x y $ \x' y' -> ["Cannot index", x', "with", y']

at, joinOp, intersperseOp, splitOp, chunksOp, takeOp, dropOp,
  indexOp, rindexOp, indicesOp, errorOp :: ValueOp2

joinOp (String s) (Array v) = String . T.intercalate s . map toStringNoNull . V.toList $ v
joinOp x y = errK "join" [(x,[KString]),(y,[KArray])]

intersperseOp x (Array v) = Array . V.fromList . intersperse x . V.toList $ v
intersperseOp x y = errK "intersperse" [(x,kinds),(y,[KArray])]

splitOp (String x) (String y) = toJSON $ T.splitOn x y
splitOp x y = errK "split" [(x,[KString]),(y,[KString])]

indicesOp (String "") (String _) = Null
indicesOp (String x) (String y)
  = toJSON $ T.length . fst <$> T.breakOnAll x y
indicesOp x          (Array xs) = toJSON $ V.elemIndices x xs
-- TODO Array . Array
indicesOp x y = cannotIndex x y

indexOp (String "") (String _) = Null
indexOp (String x)  (String y) = toJSON . T.length . fst $ T.breakOn x y
indexOp x           (Array xs) = toJSON $ V.elemIndex x xs
-- TODO Array . Array
indexOp x y = cannotIndex x y

rindexOp (String "") (String _) = Null
rindexOp (String x)  (String y) = toJSON . (\z -> z - (T.length x))
                                . T.length . fst $ T.breakOnEnd x y
rindexOp x           (Array xs) = toJSON . V.elemIndex x $ V.reverse xs
-- TODO Array . Array
rindexOp x y = cannotIndex x y

errorOp (String msg) _ = error $ cs msg
errorOp x _ = errK "error" [(x, [KString])]

-- UNUSED
-- TODO make ViewPattern
-- TODO replace use of floor
{-
toBoundedInteger :: (Integral i, Bounded i) => Value -> Maybe i
toBoundedInteger (Number n) = Scientific.toBoundedInteger n
toBoundedInteger _          = Nothing
-}

chunksOp (Number n) (String s) = toJSON $ T.chunksOf (floor n) s
chunksOp x y = errK "chunks" [(x,[KNumber]),(y,[KString])]

takeOp (Number n) (String s) = String (T.take (floor n) s)
takeOp (Number n) (Array  v) = Array  (V.take (floor n) v)
takeOp x y = errK "take" [(x,[KNumber]),(y,[KString,KArray])]

dropOp (Number n) (String s) = String (T.drop (floor n) s)
dropOp (Number n) (Array  v) = Array  (V.drop (floor n) v)
dropOp x y = errK "drop" [(x,[KNumber]),(y,[KString,KArray])]

Object o `at` String s     = fromMaybe Null $ KM.lookup (K.fromText s) o
Array  a `at` Number n     = fromMaybe Null $ do
                              case Scientific.toBoundedInteger n of
                                Just i
                                  | i < 0     -> a V.!? (V.length a + i)
                                  | otherwise -> a V.!? i
                                Nothing -> Nothing
Null     `at` String{}     = Null
Null     `at` Number{}     = Null
x        `at` y            = cannotIndex x y

has :: BoolOp2
Object o `has` String s     = KM.member (K.fromText s) o
Array  a `has` Number s     = fromInteger (floor s) < V.length a
Null     `has` String{}     = False
Null     `has` Number{}     = False
x        `has` y = err2 x y $ \x' y' -> ["Cannot check whether", x', "has a", y', "key"]

contains :: BoolOp2
x `contains` y
  | kindOf x /= kindOf y = err2 x y $ \x' y' -> [x', "and", y', "cannot have their containment checked"]
  | x == y = True
String x `contains` String y = x `T.isInfixOf` y
-- TODO: subarray, ...
x `contains` _ = err1 x $ \x' -> ["Not yet implemented: containement on", x']

toListF :: Filter
toListF (Array v)  = V.toList v
toListF (Object o) = KM.elems o
toListF x          = err1 x $ \x' -> ["Cannot iterate over", x']

toG :: (Applicative m, Monoid (m e), PureValue e, HasValue e) => Filter -> GFilter m e
toG f = foldMap (pure . pureValue) . f . valueOf

toListG :: (Applicative m, Monoid (m e), PureValue e, HasValue e) => GFilter m e
toListG = toG toListF

arrayF :: Filter -> Filter
arrayF f x = [Array (V.fromList $ f x)]

dist :: Obj [Value] -> [Obj Value]
dist = sequence

asObjectKey :: Value -> Key
asObjectKey (String x) = K.fromText x
asObjectKey x          = err1 x $ \x' -> ["Cannot use", x', "as object key"]

objectF :: Obj Filter -> Filter
objectF o x = fmap (Object . KM.fromList . fmap (first asObjectKey) . unObj) . dist . fmap ($x) $ o

op2VF :: Monad m => Op2 e -> Op1 (GFilter m e)
op2VF op f inp = do
  x <- f inp
  pure $ op x inp

-- This is actually in IO!
systemOp :: ValueOp2
systemOp cmdargs (String inp) =
  case fromJSON cmdargs of
    Success (cmd:args) ->
      -- Yes I am ashamed!
      unsafePerformIO . fmap (String . T.pack) . readProcess cmd args . T.unpack $ inp
    _ ->
      err1 cmdargs $ \cmdargs' -> ["system()'s second argument must be an array of strings and not a", cmdargs']
systemOp _cmdargs inp =
  err1 inp $ \inp' -> ["system()'s input must be a string and not a", inp']
  -- errK "system" [(inp,[KString])]

op1F :: a -> F a
op1F op = OpF op []

--UNUSED
--op2F :: a -> F a -> F a
--op2F op f = OpF op [f]

op3F :: a -> F a -> F a -> F a
op3F op f g = OpF op [f,g]

op1to2 :: Op2 v -> Op3 v
op1to2 = const

emptyF :: Alternative m => GFilter m e
emptyF _ = empty

type Name = String

-- Filter
data F a
  = IdF                 -- .
  | CompF (F a) (F a)   -- f | g
  | BothF (F a) (F a)   -- f, g
  | ArrayF (F a)        -- [f]
  | ObjectF (Obj (F a)) -- {a: f, b: g}
  | OpF a [F a]         -- F, F(f₀;...;fn)
  | ConstF Value        -- 1, "foo", null
  | ReduceF (F a) a (F a) (F a) -- reduce f as $n (g; h)
  | IfF (F a) (F a) (F a) -- if f then g else h end
  | AsF (F a) a (F a)   -- f as $n | g
  | VarF a              -- $n
  deriving (Show)

type F' = F Name

-- f[g]
atF :: F' -> F' -> F'
atF = op3F "_at"

-- f[g]
-- f[]
atFm :: F' -> Maybe F' -> F'
atFm f (Just g) = atF f g
atFm f Nothing  = f `CompF` OpF "[]" []

-- .[key]
atKeyF :: F' -> F'
atKeyF = atF IdF

trueValue :: Value -> Bool
trueValue (Bool b) = b
trueValue Null     = False
trueValue _        = True

selectF :: (IsTrue e, Foldable m, Alternative m) => Op1 (GFilter m e)
selectF f x = guard (any isTrue (f x)) $> x

whenF :: (IsTrue e, Foldable m, Alternative m) => Op1 (GFilter m e)
whenF f x = guard (all isTrue (f x)) $> x

debugOp :: ValueOp1
debugOp x = unsafePerformIO $ do
  L8.hPutStrLn stderr $ encode $ Array $ V.fromList [String "DEBUG:", x]
  pure x

{- UNUSED
concatF :: [F a] -> F a
concatF [] = IdF
concatF xs = foldr1 BothF xs
-}

instance Semigroup (F a) where
  (<>) = CompF

instance Monoid (F a) where
  mempty = IdF

composeF :: Foldable t => t (F a) -> F a
composeF = fold

toEntry :: (Value,Value) -> Value
toEntry (k,v) = Object $ KM.fromList [("key",k),("value",v)]

toEntries :: [(Value,Value)] -> Value
toEntries = Array . V.fromList . fmap toEntry

-- In ./jq to_entries is defined as:
-- def to_entries: [keys[] as $k | {key: $k, value: .[$k]}];
-- However I have no plan to implement variables yet
toEntriesOp :: ValueOp1
toEntriesOp (Object o) = toEntries . fmap (first (String . K.toText)) . KM.toList $ o
toEntriesOp (Array  v) = toEntries . zip ((Number . fromInteger) <$> [0..]) . V.toList $ v
toEntriesOp x = err1 x $ \x' -> [x', "has no keys"]

fromEntriesF :: F'
fromEntriesF = parseF "map({(.key): .value}) | add"
-- TOOD .key // .Key // .name // .Name
-- TOOD .value // .Value

{-
subst :: (a -> [F b] -> F b) -> F a -> F b
subst _   IdF           = IdF
subst env (CompF f g)   = CompF (subst env f) (subst env g)
subst env (BothF f g)   = BothF (subst env f) (subst env g)
subst env (ArrayF f)    = ArrayF (subst env f)
subst env (ObjectF o)   = ObjectF (fmap (subst env) o)
subst env (OpF op fs)   = env op (fmap (subst env) fs)
subst _   (ConstF v)    = ConstF v
-}

filterF2 :: String -> String -> Op1 Filter
filterF2 nmf sf f = filter env (parseF sf)
  where env nm [] | nm == nmf = f
        env nm fs             = filterOp nm fs

{-
data Def = Def { name :: Text, params :: [Text], body :: F }

paramsP :: Parser [Text]
paramsP =  tok '(' *> (bareWord `sepBy1` tok ';') <* tok ')'
       <|> pure []
       <?> "parameters"

definitionP :: Parser Def
definitionP = Def <$ string "def" <*> bareWord <*> paramsP <* tok ':' <*> parseFilter <* tok ';'
           <?> "definition"
-}

filterOp0 :: (Alternative m, Monoid (m e), PureValue e, HasValue e) => Name -> GFilter m e
filterOp0 = lookupOp tbl 0 where
  tbl = H.fromList $
          [("empty"         , emptyF)
          ,("error"         , emptyF) -- JQ-QUIRK
          ,("[]"            , toListG)
          ,("from_entries"  , toG $ filter filterOp fromEntriesF)
          ]
          ++ map (second (toG . (pure .)))
          [("keys"          , keysOp)
          ,("length"        , lengthOp)
          ,("add"           , addOp)
          ,("min"           , minimum . toListF)
          ,("max"           , minimum . toListF)
          ,("type"          , String . T.pack . show . kindOf)
          ,("to_entries"    , toEntriesOp)
          ,("_negate"       , negateOp)
          ,("sqrt"          , sqrtOp)
          ,("_sqrt"         , sqrtOp)
          ,("floor"         , floorOp)
          ,("_floor"        , floorOp)
          ,("sort"          , sortOp)
          ,("tonumber"      , toNumberOp)
          ,("tostring"      , toStringOp)
          ,("not"           , Bool . not . trueValue)
          ,("unique"        , uniqueOp)
          ,("tojson"        , String . cs . encode)
          ,("fromjson"      , fromjsonOp)
          ,("debug"         , debugOp)
          -- NP extensions
          ,("negate"        , negateOp)
          ,("lines"         , linesOp)
          ,("unlines"       , unlinesOp)
          ,("words"         , wordsOp)
          ,("unwords"       , unwordsOp)
          ,("init"          , initOp)
          ,("tail"          , tailOp)
          ,("reverse"       , reverseOp)
          ,("casefold"      , endoTextOp "casefold"  T.toCaseFold)
          ,("lowercase"     , endoTextOp "lowercase" T.toLower)
          ,("uppercase"     , endoTextOp "uppercase" T.toUpper)
          ,("strip"         , endoTextOp "strip"     T.strip)
          ,("rstrip"        , endoTextOp "rstrip"    T.stripEnd)
          ,("lstrip"        , endoTextOp "lstrip"    T.stripStart)
  -- Arrays and String:
  --   null
  -- Text:
  --   isPrefixOf :: Text -> Text -> Bool
  --   isSuffixOf :: Text -> Text -> Bool
  --   isInfixOf :: Text -> Text -> Bool
          ]

-- filterOp1 :: (IsTrue e, Foldable m, Alternative m) => Name -> Op1 (GFilter m e)
filterOp1 :: Name -> Op1 Filter
filterOp1 = lookupOp tbl 1
  where tbl = H.fromList
          [("select"      , selectF)
          ,("has"         , op2VF (boolOp2 (flip has)))
          ,("contains"    , op2VF (boolOp2 contains))
          ,("map"         , filterF2 "f" "[.[] | f]") -- def map(f): [.[] | f];
-         ,("with_entries", filterF2 "f" "to_entries | map(f) | from_entries") -- "def with_entries(f): to_entries | map(f) | from_entries;"
          ,("range"       , rangeF1)
          ,("isempty"     , isemptyF)
          ,("IN"          , _inF pure)
          ,("error"       , op2VF errorOp)
          ,("join"        , op2VF joinOp)
          ,("split"       , op2VF splitOp)
          ,("index"       , op2VF indexOp)
          ,("rindex"      , op2VF rindexOp)
          ,("indices"     , op2VF indicesOp)
          -- NP extensions
          ,("when"        , whenF)
          ,("system"      , op2VF systemOp)
          ,("intersperse" , op2VF intersperseOp)
          ,("chunks"      , op2VF chunksOp)
          ,("take"        , op2VF takeOp)
          ,("drop"        , op2VF dropOp)
          -- NP definitions
--          ,("jsystem"     , filterF2 "f" "tojson | system(f) | fromjson")
          ]

unknown :: Int -> Name -> a
unknown a nm = error $ nm ++ " is not defined (arity " ++ show a ++ ")"

lookupOp :: HashMap Name a -> Int -> Name -> a
lookupOp tbl arity nm = fromMaybe (unknown arity nm) (H.lookup nm tbl)

replaceOp :: ValueOp3
replaceOp (String x) (String y) (String z) = String (T.replace y z x)
replaceOp x y z = err3 x y z $ \x' y' z' -> ["replace expects 3 string arguments not", x', y', z']

rangeOp :: Value -> Value -> Value -> [Value]
rangeOp (Number x) (Number y) (Number z)
    | z < 0     = down x
    | otherwise = up x
  where
    down a | a > y     = Number a : down (a + z)
           | otherwise = []
    up   a | a < y     = Number a : up (a + z)
           | otherwise = []
rangeOp _ _ _ = err ["Range bounds must be numeric"]

rangeF3 :: (PureValue e, HasValue e, Monoid (m e), Monad m) =>
 -- GFilter m e -> GFilter m e -> GFilter m e -> ()
  Op3 (GFilter m e)
rangeF3 f g h x =
  join
  ((\fx gx hx -> foldMap (pure . pureValue) (rangeOp (valueOf fx) (valueOf gx) (valueOf hx)))
  <$> f x <*> g x <*> h x
  )

rangeF2 :: (PureValue e, HasValue e, Monoid (m e), Monad m) => Op2 (GFilter m e)
rangeF2 f g = rangeF3 f g (constG $ Number 1)

rangeF1 :: (PureValue e, HasValue e, Monoid (m e), Monad m) => Op1 (GFilter m e)
rangeF1 = rangeF2 (constG $ Number 0)

--modifyF :: Filter -> Filter -> Filter
--modifyF source update v =
--TODO
--jq relies on: reduce, path/1, setpath/2, getpath/1, delpaths/1, label, break
--def _modify(paths; update): reduce path(paths) as $p (.; label $out | (setpath($p; getpath($p) | update) | ., break $out), delpaths([$p]));

type GFilter m e = e -> m e

idG :: Applicative m => GFilter m e
idG = pure

constG :: (Applicative m, PureValue e) => Value -> GFilter m e
constG = const . pure . pureValue

bothG :: PGFilter a e -> PGFilter a e -> PGFilter a e
bothG f g x = f x <> g x

bothG' :: Monoid (m e) => GFilter m e -> GFilter m e -> GFilter m e
bothG' f g x = f x <> g x

{-
class ListValues a where
  listValues :: a -> [Value]

arrayG :: ListValues (m e) => GFilter m e -> GFilter m e
arrayG f x = pure $ Array (V.fromList . listValues $ f x)
-}

--pgToFilter

{-
arrayG :: PGFilter a Value -> PGFilter a Value
arrayG f x = do
  env <- ask
  pure $ Array (V.fromList $ runReaderT (f x) env)
-}

class IsTrue a where
  isTrue :: a -> Bool

instance IsTrue Value where
  isTrue = trueValue

ifG :: (Monad m, IsTrue e) => GFilter m e -> GFilter m e -> GFilter m e -> GFilter m e
ifG c t e x = do
  b <- c x
  if isTrue b then t x else e x

type GEnv a m v = a -> [GFilter m v] -> GFilter m v

extend :: (Applicative m, Eq a) => a -> v -> GEnv a m v -> GEnv a m v
extend x z env y | x == y    = \_ _ -> pure z
                 | otherwise = env y

-- E = Writer [Value] Value
data E = E { epath :: [Value], evalue :: Value }

instance IsTrue E where
  isTrue = isTrue . evalue

class HasValue a where
  valueOf :: a -> Value

instance HasValue Value where
  valueOf = id

instance HasValue E where
  valueOf = evalue

type EFilter = GFilter [] E

class PureValue a where
  pureValue :: Value -> a

instance PureValue Value where
  pureValue = id

instance PureValue E where
  pureValue = E []

toE :: Filter -> EFilter
toE f (E _ x) = pureValue <$> f x

fromE :: EFilter -> Filter
fromE f x = evalue <$> f (pureValue x)

constE :: Value -> EFilter
constE = constG

arrayE :: EFilter -> EFilter
arrayE f x = pure . pureValue . Array . V.fromList $ evalue <$> f x
  -- pureValue == E [], but should it be E (epath x) ???

objectE :: Obj EFilter -> EFilter
objectE o x = fmap (pureValue . Object . H.fromList . fmap (first asObjectKey) . unObj)
            . sequence . fmap (fmap evalue . ($x)) $ o

asG' :: (Monad m, Eq a) => GEnv a m v -> GFilter m v -> a -> (GEnv a m v -> GFilter m v) -> GFilter m v
asG' env f n g x = do
  y <- f x
  g (extend n y env) x

asG :: (MonadReader (GEnv a m v) m, Eq a) => GFilter m v -> a -> GFilter m v -> GFilter m v
asG f n g x = do
  y <- f x
  local (extend n y) (g x)

class HasNull a where
  nullValue :: a

instance HasNull Value where
  nullValue = Null

instance HasNull E where
  nullValue = E [] Null

lastValue :: (Foldable f, HasNull v) => f v -> v
lastValue = fromMaybe nullValue . getLast . foldMap pure

reduceG' :: (Foldable m, Monad m, Eq a, HasNull v) => GEnv a m v -> GFilter m v -> a -> GFilter m v -> (GEnv a m v -> GFilter m v) -> GFilter m v
reduceG' env input n nil cons x = do
  z <- nil x
  pure . foldl' f z $ input x
  where
    f acc y = lastValue $ cons (extend n y env) acc

reduceG :: (Foldable m, MonadReader (GEnv a m v) m, Eq a, HasNull v)
        => GFilter m v -> a -> GFilter m v -> GFilter m v -> GFilter m v
reduceG input n nil cons x = do
  z <- nil x
  pure . foldl' f z $ input x
  where
    f acc y = lastValue . local (extend n y) $ cons acc

type EEnv a = a -> [EFilter] -> EFilter

{-
type M e m = (MonadWriter [e] m, MonadReader e m)

idG :: M e m => m ()
idG = ask >>= tell . pure

bothG :: Applicative m => m () -> m a -> m a
bothG = (*>)

arrayG :: m () ->
arrayG f = do
  x <- ask
  tell [Array (V.fromList $ f x)]

ifG :: (Monad m, a ~ Value) => m a -> m a -> m a -> m a
ifG c t e = do
  b <- c
  if trueValue b then t else e

type GFilter m = m Value
type GEnv a m = a -> [m Value] -> m Value
-}

pathE :: EFilter -> EFilter
pathE f (E _ x) = [ E ps (toJSON ps) | E ps _ <- f (E [] x) ]

filterE :: (IsString a, Eq a) => EEnv a -> F a -> EFilter
filterE _   IdF               = idG
filterE _   (ConstF v)        = constE v
filterE env (CompF f g)       = filterE env f >=> filterE env g
filterE env (BothF f g)       = bothG' (filterE env f) (filterE env g)
filterE env (ArrayF f)        = arrayE (filterE env f)
filterE env (ObjectF o)       = objectE (filterE env <$> o)
filterE env (OpF "path" [f])  = pathE (filterE env f)
filterE env (OpF op fs)       = env op (filterE env <$> fs)
filterE env (IfF c t e)       = ifG (filterE env c) (filterE env t) (filterE env e)
filterE env (VarF n)          = env n []
filterE env (AsF f n g)       = asG' env (filterE env f) n (`filterE` g)
filterE env (ReduceF f n g h) = reduceG' env (filterE env f) n (filterE env g) (`filterE` h)
{-
pathF :: (IsString a, Eq a, MonadReader Value m, MonadWriter [[Value]] m)
      => GEnv a -> F a -> m Value
pathF _   IdF               = tell [[]] >> ask
pathF env (CompF f g)       = pathF env f >=> pathF env g
pathF env (BothF f g)       = bothG (pathF env f) (pathF env g)
pathF _   (OpF "[]" [])     = do i <- ask
                                 mapM_ tell $ iterKeysF i
>=> (\v -> [toJSON [v]])
pathF env (OpF "_at" [f,g]) = \v -> liftA2 (\x y -> x +| Array (V.fromList [y])) (pathF env f v) (filter env g Null)
pathF env (OpF op fs)       = error "pathF" env op (filter env <$> fs)
pathF env (IfF c t e)       = ifG (pathF env c) (pathF env t) (pathF env e)
pathF env (VarF n)          = env n []
pathF env (AsF f n g)       = error "asF" env (filter env f) n (`pathF` g)
pathF env (ReduceF f n g h) = error "reduceF" env (filter env f) n (pathF env g) (`pathF` h)
pathF env f@ConstF{}        = filter env f >=> \v -> err ["Invalid path expression with result", cs $ encode v]
pathF env f@ArrayF{}        = filter env f >=> \v -> err ["Invalid path expression with result", cs $ encode v]
pathF env f@ObjectF{}       = filter env f >=> \v -> err ["Invalid path expression with result", cs $ encode v]

-- pathF :: Eq a => Env a -> F a -> Filter
pathF :: (IsString a, Eq a) => Env a -> F a -> m Value
pathF _   IdF               = pure [Array mempty]
pathF env (CompF f g)       = \v -> liftA2 (+|) (pathF env f v) ((filter env f >=> pathF env g) v)
pathF env (BothF f g)       = bothF (pathF env f) (pathF env g)
pathF _   (OpF "[]" [])     = iterKeysF >=> (\v -> [toJSON [v]])
pathF env (OpF "_at" [f,g]) = \v -> liftA2 (\x y -> x +| Array (V.fromList [y])) (pathF env f v) (filter env g Null)
pathF env (OpF op fs)       = error "pathF" env op (filter env <$> fs)
pathF env (IfF c t e)       = ifF (filter env c) (pathF env t) (pathF env e)
pathF env (VarF n)          = env n []
pathF env (AsF f n g)       = asF env (filter env f) n (`pathF` g)
pathF env (ReduceF f n g h) = reduceF env (filter env f) n (pathF env g) (`pathF` h)
pathF env f@ConstF{}        = filter env f >=> \v -> err ["Invalid path expression with result", cs $ encode v]
pathF env f@ArrayF{}        = filter env f >=> \v -> err ["Invalid path expression with result", cs $ encode v]
pathF env f@ObjectF{}       = filter env f >=> \v -> err ["Invalid path expression with result", cs $ encode v]
-}

isemptyF :: (PureValue e, Foldable m, Applicative m) => Op1 (GFilter m e)
isemptyF f v = pure . pureValue . Bool $ Foldable.null (f v)

-- streamOp2 :: ([Value] -> [Value] -> Value) -> Filter -> Filter -> Filter
-- streamOp2 op f g v =

-- NOTE `Eq e` when E ~ e this compare paths too!
_inF :: (Eq e, PureValue e, Foldable m, Applicative m) => Op2 (GFilter m e)
_inF source elems v = pure . pureValue . Bool $ any (`elem` elems v) (source v)

-- _indexF :: Filter -> Filter -> Filter
-- _indexF source ix_expr

-- The use of `ceiling` seems consistent with jq.
limitF :: HasValue e => Op2 (GFilter [] e)
limitF f g v = do
  Number l <- valueOf <$> f v
  List.take (ceiling l) (g v)

nthF :: (HasValue e, HasNull e) => Op2 (GFilter [] e)
nthF f g v = do
    Number l <- valueOf <$> f v
    pure $ nth (ceiling l :: Int) (g v)
  where
    nth n _ | n < 0 = err ["nth doesn't support negative indices"]
    nth _ []        = nullValue
    nth 0 (x:_)     = x
    nth n (x:xs)    = go (n - 1) x xs

    go _ x []     = x
    go 0 _ (y:_)  = y
    go n _ (y:ys) = go (n - 1) y ys

valueOp3 :: Applicative m => Op3 e -> Op2 (GFilter m e)
valueOp3 op f g x = op x <$> f x <*> g x
-- liftA2 (op x) (f x) (g x)

valueOp3' :: Applicative m => Op3 e -> Op2 (GFilter m e)
valueOp3' op f g x = flip (op x) <$> g x <*> f x

filterOp2 :: Name -> Op2 Filter
filterOp2 = lookupOp tbl 2 where
  tbl = H.fromList $
            [("limit", limitF)
            ,("nth",   nthF)
            ,("IN",    _inF)
--            ,("INDEX", _indexF)
-- TODO            ,("_modify", modifyF)
            ] ++
          -- NP definitions
            map (second valueOp3)
            [("replace"    , replaceOp)]
         ++ map (second (valueOp3' . op1to2))
            [("_plus"      , (+|))
            ,("_multiply"  , (*|))
            ,("_minus"     , (-|))
            ,("_divide"    , (/|))
            ,("_mod"       , (%|))
            ,("_less"      , boolOp2Ord (<))
            ,("_lesseq"    , boolOp2Ord (<=))
            ,("_greatereq" , boolOp2Ord (>=))
            ,("_greater"   , boolOp2Ord (>))
            ,("_equal"     , boolOp2 (==))
            ,("_notequal"  , boolOp2 (/=))
            ,("_at"        , at)
            ]
         ++ map (second (valueOp3 . op1to2))
            [("_and"       , boolOp2' (&&))
            ,("_or"        , boolOp2' (||))
            ]
         ++ [("range"      , rangeF2)
            ]

filterOp3 :: (PureValue e, HasValue e, Monoid (m e), Monad m) => Name -> Op3 (GFilter m e)
filterOp3 = lookupOp tbl 3 where
  tbl = H.fromList $
            [("range"      , rangeF3)
            ]

type Env a = a -> [Filter] -> Filter

filterOp :: Env Name
filterOp nm []      = filterOp0 nm
filterOp nm [f]     = filterOp1 nm f
filterOp nm [f,g]   = filterOp2 nm f g
filterOp nm [f,g,h] = filterOp3 nm f g h
filterOp nm fs      = unknown (length fs) nm

askEnv :: (Eq a, MonadReader (GEnv a m Value) m) => a -> [GFilter m Value] -> GFilter m Value
askEnv op fs x = do
  env <- ask
  env op fs x

type PGFilter a e = forall m. (Foldable m, Monoid (m e), MonadReader (GEnv a m e) m) => GFilter m e

-- filter :: Eq a => Env a -> F a -> Filter
-- filterG :: (IsString a, Eq a, MonadReader (Env a) m) => F a -> GFilter m Value
{-
filterG :: (IsString a, Eq a) => F a -> PGFilter a Value
filterG IdF               = pure
filterG (ConstF v)        = constG v
filterG (CompF f g)       = filterG f >=> filterG g
filterG (BothF f g)       = bothG (filterG f) (filterG g)
filterG (ArrayF f)        = arrayG (filterG f)
filterG (ObjectF o)       = _HobjectF (filterG <$> o)
-- filterG (OpF "path" [f])  = pathF f
filterG (OpF op fs)       = askEnv op (filterG <$> fs)
filterG (IfF c t e)       = ifG (filterG c) (filterG t) (filterG e)
filterG (VarF n)          = askEnv n []
filterG (AsF f n g)       = asG (filterG f) n (filterG g)
filterG (ReduceF f n g h) = reduceG (filterG f) n (filterG g) (filterG h)
-}
-- filter :: Eq a => Env a -> F a -> Filter
filter :: (IsString a, Eq a) => Env a -> F a -> Filter
{-
filter env f = fromE $ filterE env f
--filter env f = fromE $ filterE (\a fs -> toE (env a (fromE <$> fs))) f
-}
filter _   IdF               = pure
filter _   (ConstF v)        = constG v
filter env (CompF f g)       = filter env f >=> filter env g
filter env (BothF f g)       = bothG' (filter env f) (filter env g)
filter env (ArrayF f)        = arrayF (filter env f)
filter env (ObjectF o)       = objectF (filter env <$> o)
-- filter env (OpF "path" [f])  = pathF env f
filter env (OpF op fs)       = env op (filter env <$> fs)
filter env (IfF c t e)       = ifG (filter env c) (filter env t) (filter env e)
filter env (VarF n)          = env n []
filter env (AsF f n g)       = asG' env (filter env f) n (`filter` g)
filter env (ReduceF f n g h) = reduceG' env (filter env f) n (filter env g) (`filter` h)

parseSimpleFilter, parseNegateFilter, parseAsFilter,
  parseNoCommaFilter, parseFilter, parseDotFilter, parseConcFilter :: Parser F'

parseDotFilter = atKeyF <$> (char '.' *> skipSpace *> parseKey)
  <?> "dot filter"

parseAtFilters :: F' -> Parser F'
parseAtFilters f = do
  b <- tok '[' *> (atFm f <$> optional parseFilter) <* tok ']'
       <|> (f `CompF`) <$> parseDotFilter
       <|> pure IdF
  case b of
    IdF -> pure f
    _   -> parseAtFilters b

keywords :: [String]
keywords = ["if", "then", "else", "end", "as", "reduce"]

keyword :: B.ByteString -> Parser ()
keyword s = skipSpace <* string s

ident :: Parser String
ident = do x <- (:) <$> satisfy lic <*> many (satisfy ic)
           guard $ x `notElem` keywords
           pure x
  where lic c = c == '_' || isAscii c && isAlpha c
        ic  c = c == '_' || isAscii c && isAlphaNum c

bareWord :: Parser Text
bareWord = T.pack <$> ident
        <?> "bare word"

parseOp0 :: Parser Value
parseOp0
   =  Number        <$> scientific
  <|> Bool True     <$  string "true"
  <|> Bool False    <$  string "false"
  <|> Null          <$  string "null"
  <|> Array mempty  <$  string "[]"
  <|> Object mempty <$  string "{}"
  <?> "arity 0 operator (\"a\", 42, true, null, [], {}, ...)"

tok :: Char -> Parser Char
tok c = skipSpace *> char c

parseSimpleFilter
  =  skipSpace *>
  (  parseDotFilter
 <|> interpolatedString
 <|> IdF      <$  char '.'
 <|> VarF     <$> parseVar
 <|> ConstF   <$> parseOp0
 <|> ReduceF  <$  string  "reduce" <*> parseFilter
              <*  keyword "as"     <*> parseVar
              <*  tok     '('      <*> parseFilter
              <*  tok     ';'      <*> parseFilter
              <*  tok     ')'
 <|> OpF      <$> ident <*> (tok '(' *> parseFilter `sepBy1` tok ';' <* tok ')'
                              <|> pure [])
 <|> ArrayF   <$  char '[' <*> parseFilter <* tok ']'
 <|> ObjectF  <$> objectFilterP
 <|> IfF      <$  string  "if"   <*> parseFilter
              <*  keyword "then" <*> parseFilter
              <*  keyword "else" <*> parseFilter
              <*  keyword "end"
 <|> char '('  *> parseFilter <* tok ')'
 <?> "simple filter"
  )

parseConcFilter = (parseSimpleFilter >>= parseAtFilters)
               <?> "conc filter"

parseAsFilter = AsF <$> parseConcFilter <* keyword "as" <*> parseVar <* tok '|' <*> parseFilter
             <|> parseConcFilter
             <?> "as filter"

negateF' :: F' -> F'
negateF' = (`CompF` op1F "_negate")

parseNegateFilter = negateF' <$> (tok '-' *> parseAsFilter)
                 <|> parseConcFilter
                 <?> "negate filter"

modifyOp :: IsString a => a -> F a -> F a -> F a
modifyOp op f g = op3F "_modify" f (op3F op IdF g)

table :: [[Operator B.ByteString F']]
table   = [ [Prefix (negateF' <$ tok '-')]
          , [binary op AssocLeft | op <- [("*","_multiply"),("/","_divide"),("%","_mod")]]
          , [binary op AssocLeft | op <- [("+","_plus"),("-","_minus")]]
          , [binary op AssocNone | op <- [("<=","_lesseq"),("<","_less"),("==","_equal")
                                         ,("!=","_notequal"),(">=","_greatereq"),(">","_greater")
                                         ]]
          , [binary ("and","_and") AssocLeft]
          , [binary ("or","_or") AssocLeft]
          , [binaryF op AssocNone
            | op <- [("=",  op3F "_assign")
                    ,("|=", op3F "_modify")
                    ,("+=", modifyOp "_plus")
                    ,("-=", modifyOp "_minus")
                    ,("*=", modifyOp "_multiply")
                    ,("/=", modifyOp "_divide")
                    ,("%=", modifyOp "_mod")
               --   ,("//=", modifyOp "_definedor")
                    ]
            ]
          -- AssocRight //
          , [binaryF (",", BothF) AssocLeft]
          , [binaryF ("|", CompF) AssocRight]
          ]

binaryF :: (B.ByteString, F' -> F' -> F') -> Assoc -> Operator B.ByteString F'
binaryF (name, f) = Infix (f <$ skipSpace <* string name)

binary :: (B.ByteString, Name) -> Assoc -> Operator B.ByteString F'
binary (name, fun) = binaryF (name, op3F fun)

parseFilter = buildExpressionParser table parseAsFilter
           <?> "filter"

parseNoCommaFilter = composeF <$> parseNegateFilter `sepBy1` tok '|'
                  <?> "no comma filter"

parseVar :: Parser Name
parseVar = ('$':) <$> (tok '$' *> ident)

objectFilterP :: Parser (Obj F')
objectFilterP = Obj
             <$> (char '{' *> skipSpace *>
                 ((pair <* skipSpace) `sepBy` (char ',' *> skipSpace))
               <* char '}')
   where fill k = (k , atKeyF k)
         keyFilterP =  parseKey
                   <|> char '(' *> parseFilter <* tok ')'
                   <?> "key filter"
         pair =  (,)  <$> keyFilterP <*> (tok ':' *> skipSpace *> parseNoCommaFilter)
             <|> fill <$> parseKey

stringF :: ConvertibleStrings s Text => s -> F'
stringF = ConstF . String . cs

-- TODO add support for the combination of interpolation and escapes.
-- example: "\n\(1)"
interpolatedString :: Parser F'
interpolatedString = p -- <|> stringF <$> jstring
                        -- ^ should become useless once `p` handles escapes
  where
    p :: Parser F'
    p =  char '"'
      *> (foldr addString (ConstF $ String "") <$> manyTill q (char '"'))
      <?> "interpolated string"
    q :: Parser F'
    q  =  stringF <$> many1' (satisfy (`notElem` ['\\', '\"']))
      <|> char '\\' *> e
      <|> stringF . (:[]) <$> satisfy (/= '"')
      <?> "inside interpolated string"
    e :: Parser F'
    e  =  char '(' *> parseFilter <* tok ')'
      <|> char 'r' $> stringF ['\r']
      <|> char 'n' $> stringF ['\n']
      <|> char 't' $> stringF ['\t']
      <|> char 'b' $> stringF ['\b']
      <|> char 'f' $> stringF ['\f']
      <|> char 'u' *> u
    u' = (\a b c d -> ['"','\\','u',a,b,c,d,'"']) <$> h <*> h <*> h <*> h
    u = u' >>= (\s -> stringF <$> parseSub s jstring)
    h = satisfy isHexDigit
    addString f = op3F "_plus" (f `CompF` op1F "tostring")

parseKey :: Parser F'
parseKey =  stringF <$> bareWord
        <|> interpolatedString

type Msg = String

parseF :: String -> F'
parseF = either error id . parseM "filter" (top parseFilter) . L8.pack

parseSub :: ConvertibleStrings s L.ByteString => s -> Parser a -> Parser a
parseSub s p =
  case L.parse p (cs s) of
    L.Done _ r -> pure r
    L.Fail _ _ _ -> empty

parseM :: Msg -> Parser a -> L.ByteString -> Either String a
parseM msg p s =
  case L.parse p s of
    L.Done _ r -> Right r
    L.Fail _ ctx msg' -> Left (msg <> ": " <> msg' <> " context:" <> show ctx)

parseIO :: Msg -> Parser a -> L.ByteString -> IO a
parseIO msg p s = either fail return $ parseM msg p s

top :: Parser a -> Parser a
top p = p <* skipSpace <* endOfInput

stream :: Parser [Value]
stream = value `sepBy` skipSpace

readInput :: Bool -> IO [Value]
readInput True = return [Null]
readInput _    = parseIO "JSON decoding" (top stream) =<< L.getContents

mainFilter :: Bool -> Bool -> Bool -> String -> IO ()
mainFilter wrap_output noinput raw_output arg = do
  f <- parseIO "parsing filter" (top parseFilter) (L8.pack arg)
  -- print f
  input <- readInput noinput
  mapM_ (outputValue wrap_output raw_output) $ concatMap (filter filterOp f) input

type TestCase = (L.ByteString,L.ByteString,[L.ByteString])

parseTestCase :: TestCase -> Either String (F',Value,[Value])
parseTestCase (prg,inp,out) =
   (,,) <$> parseM "test program" (top parseFilter) prg
        <*> parseM "test input"   (top value)       inp
        <*> parseM "test output"  (top stream)      (L8.unwords out)

runTest :: (String -> Bool) -> Bool -> TestCase -> IO ()
runTest match code tc = do
  case parseTestCase tc of
    Left msg ->
          when (match "ERROR") $ do
            printTestCase tc
            putStrLn msg
            when code $ putStrLn (color 31 "ERROR")
            putStrLn ""
    Right (f, input, reference) -> do
      let output  = filter filterOp f input
          encoded = encode <$> output
      result <- try $ evaluate (sum (L8.length <$> encoded) `seq` ())
      case result of
        Right ()
          | output == reference ->
            when (match "PASS") $ do
              printTestCase tc
              when code $ putStrLn (color 32 "PASS")
              putStrLn ""
          | otherwise -> do
            when (match "FAIL") $ do
              printTestCase tc
              putStrLn "was expected, but instead this is the output"
              mapM_ L8.putStrLn encoded
              when code $ putStrLn (color 31 "FAIL")
              putStrLn ""
        Left exn -> do
            when (match "ERROR") $ do
              printTestCase tc
              putStrLn "execution failed with this error:"
              putStrLn (show (exn :: SomeException))
              when code $ putStrLn (color 31 "ERROR")
              putStrLn ""

color :: Int -> String -> String
color n = ("\^[["++) . shows n . ('m':) . (++ "\^[[m")

printTestCase :: TestCase -> IO ()
printTestCase (x,y,zs) = mapM_ L8.putStrLn (x:y:zs)

runTests :: (String -> Bool) -> Bool -> IO ()
runTests match code
  = mapM_ (runTest match code)
  . fmap splitTestCase
  . splitOnEmptyLines
  . fmap dropComment
  . L8.lines
  =<< L.getContents

mkPred :: Eq a => [a] -> a -> Bool
mkPred [] _ = True
mkPred xs x = x `elem` xs

splitTestCase :: [a] -> (a,a,[a])
splitTestCase (x:y:zs) = (x,y,zs)
splitTestCase _        = error "splitTestCase: too few lines for a test case"

splitOnEmptyLines :: [L.ByteString] -> [[L.ByteString]]
splitOnEmptyLines []  = []
splitOnEmptyLines xss =
  case break L.null (dropWhile L.null xss) of
    (yss,zss) -> (if List.null yss then id else (yss :))
                 (splitOnEmptyLines zss)

dropComment :: L.ByteString -> L.ByteString
dropComment s
  | "#" `L.isPrefixOf` s = L.empty
  | otherwise            = s

encodeValue :: Bool -> Value -> L.ByteString
encodeValue True (String s) = cs s
encodeValue _    v          = encode v

outputValue :: Bool -> Bool -> Value -> IO ()
outputValue False raw_output = L8.putStrLn . encodeValue raw_output
outputValue True  raw_output = mapM_ L.putStr . ($["\n"]) . f
  where f (Array a)   = t"[" . cat (intersperse (t"\n,") (map j . V.toList $ a)) . t"]"
        f (Object o)  = t"{" . cat (intersperse (t"\n,") (map g . KM.toList $ o)) . t"}"
        f v           = t $ encodeValue raw_output v
        g (key, val)  = t (encode key) . t(L8.pack ":") . j val
        j             = t . encode
        t x           = (x:)
        cat           = appEndo . mconcat . map Endo

main :: IO ()
main = do args <- getArgs
          if "--run-tests" `elem` args
            then
              let args' = args \\ ["--run-tests"] in
              runTests (mkPred args') (length args' /= 1)
            else do
              -- -c is ignored
              let [arg] = args \\ ["-n","-r","--raw-output","-c","-w"]
              mainFilter ("-w" `elem` args) ("-n" `elem` args)
                         ("-r" `elem` args || "--raw-output" `elem` args)
                         arg
