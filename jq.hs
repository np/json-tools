{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
import Control.Applicative as A
import Control.Arrow (first,second,(***))
import Control.Monad ((>=>), guard, when)
import Control.Exception (evaluate, try, SomeException)
import Data.Ord (comparing)
import Prelude hiding (filter,sequence,Ordering(..))
import Data.Maybe
import Data.Char
import Data.Functor
import Data.List ((\\),sort,sortBy,intersperse,nub)
import qualified Data.List as List
import Data.Monoid
import Data.Aeson hiding ((<?>))
import Data.Aeson.Parser (jstring, value)
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

Number x /| Number y = Number (x / y)
String x /| String y = toJSON (T.splitOn y x)
x        /| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be divided"]

Number x %| Number y = Number (fromInteger $ floor x `rem` floor y)
x %| y = err2 x y $ \x' y' -> [x', "and", y', "cannot be 'mod'ed"]

newtype NObj a = NObj (HashMap Text a)
  deriving (Eq)
instance Ord a => Ord (NObj a) where
  x <= y | x == y = True
  NObj x <= NObj y = f x <= f y where f = sortBy (comparing fst) . H.toList

instance Ord Value where
  Null     <= _        = True
  _        <= Null     = False
  Bool   x <= Bool y   = x <= y
  Bool   _ <= _        = True
  _        <= Bool _   = False
  Number x <= Number y = x <= y
  Number _ <= _        = True
  _        <= Number _ = False
  String x <= String y = x <= y
  String _ <= _        = True
  _        <= String _ = False
  Array  x <= Array  y = x <= y
  Array  _ <= _        = True
  _        <= Array _  = False
  Object x <= Object y = NObj x <= NObj y

boolOp2 :: BoolOp2 -> ValueOp2
boolOp2 f x y = Bool (f x y)

boolOp2' :: (Bool -> Bool -> Bool) -> ValueOp2
boolOp2' f x y = Bool (f (trueValue x) (trueValue y))

-- NOTE: As in jq length is the identity on numbers.
lengthFi :: Value -> Scientific
lengthFi Null       = 0
lengthFi (Array v)  = fromIntegral $ V.length v
lengthFi (Object o) = fromIntegral $ length . H.toList $ o
lengthFi (String s) = fromIntegral $ T.length s
lengthFi (Number n) = n
lengthFi (Bool b)   = err1 (Bool b) $ \x' -> [x', "has no length"]

toString :: Value -> Text
toString (String s) = s
toString x = cs . encode $ x

toStringNoNull :: Value -> Text
toStringNoNull Null = ""
toStringNoNull x = toString x

lengthOp, keysOp, addOp, negateOp, sqrtOp, floorOp, sortOp,
  uniqueOp, toNumberOp, toStringOp, fromjsonOp, linesOp, unlinesOp,
  wordsOp, unwordsOp, tailOp, initOp, reverseOp :: ValueOp1

lengthOp = Number . lengthFi

keysOp (Array v)  = toJSON [0.. V.length v - 1]
keysOp (Object o) = toJSON . sort . H.keys $ o
keysOp x          = err1 x $ \x' -> [x', "has no keys"]

addOp = foldr (+|) Null . toList

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

chunksOp (Number n) (String s) = toJSON $ T.chunksOf (floor n) s
chunksOp x y = errK "chunks" [(x,[KNumber]),(y,[KString])]

takeOp (Number n) (String s) = String (T.take (floor n) s)
takeOp (Number n) (Array  v) = Array  (V.take (floor n) v)
takeOp x y = errK "take" [(x,[KNumber]),(y,[KString,KArray])]

dropOp (Number n) (String s) = String (T.drop (floor n) s)
dropOp (Number n) (Array  v) = Array  (V.drop (floor n) v)
dropOp x y = errK "drop" [(x,[KNumber]),(y,[KString,KArray])]

Object o `at` String s     = fromMaybe Null $ H.lookup s o
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
Object o `has` String s     = H.member s o
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

toList :: Filter
toList (Array v)  = V.toList v
toList (Object o) = H.elems o
toList x          = err1 x $ \x' -> ["Cannot iterate over", x']

bothF :: Filter -> Filter -> Filter
bothF f g x = f x ++ g x

arrayF :: Filter -> Filter
arrayF f x = [Array (V.fromList $ f x)]

dist :: Obj [Value] -> [Obj Value]
dist = sequence

asObjectKey :: Value -> Text
asObjectKey (String x) = x
asObjectKey x          = err1 x $ \x' -> ["Cannot use", x', "as object key"]

objectF :: Obj Filter -> Filter
objectF o x = fmap (Object . H.fromList . fmap (first asObjectKey) . unObj) . dist . fmap ($x) $ o

op2VF :: ValueOp2 -> Filter -> Filter
op2VF op f inp = [ op x inp | x <- f inp ]

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

op2to3 :: ValueOp2 -> ValueOp3
op2to3 = const

emptyF :: Filter
emptyF _ = []

constF :: Value -> Filter
constF v _ = [v]

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

selectF :: Filter -> Filter
selectF f x = [x | any trueValue (f x)]

whenF :: Filter -> Filter
whenF f x = [x | all trueValue (f x)]

ifF :: Filter -> Filter -> Filter -> Filter
ifF c t e x = [ y
              | b <- c x
              , y <- if trueValue b then t x else e x
              ]

debugOp :: ValueOp1
debugOp x = unsafePerformIO $ do
  L8.hPutStrLn stderr $ encode $ Array $ V.fromList [String "DEBUG:", x]
  pure x

{- UNUSED
concatF :: [F a] -> F a
concatF [] = IdF
concatF xs = foldr1 BothF xs
-}

composeF :: [F a] -> F a
composeF [] = IdF
composeF xs = foldr1 CompF xs

toEntry :: (Value,Value) -> Value
toEntry (k,v) = Object $ H.fromList [("key",k),("value",v)]

toEntries :: [(Value,Value)] -> Value
toEntries = Array . V.fromList . fmap toEntry

-- In ./jq to_entries is defined as:
-- def to_entries: [keys[] as $k | {key: $k, value: .[$k]}];
-- However I have no plan to implement variables yet
toEntriesOp :: ValueOp1
toEntriesOp (Object o) = toEntries . fmap (first String) . H.toList $ o
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

filterF2 :: String -> String -> Filter -> Filter
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

filterOp1 :: Name -> Filter
filterOp1 = lookupOp tbl 1 where
  tbl = H.fromList . (tbl' ++) $
          [("empty"         , emptyF)
          ,("error"         , emptyF) -- JQ-QUIRK
          ,("[]"            , toList)
          ,("from_entries"  , filter filterOp fromEntriesF)]

  tbl' = map (second (pure .))
          [("keys"          , keysOp)
          ,("length"        , lengthOp)
          ,("add"           , addOp)
          ,("min"           , minimum . toList)
          ,("max"           , minimum . toList)
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

filterOp2 :: Name -> Filter -> Filter
filterOp2 = lookupOp tbl 2
  where tbl = H.fromList
          [("select"      , selectF)
          ,("has"         , op2VF (boolOp2 (flip has)))
          ,("contains"    , op2VF (boolOp2 contains))
          ,("map"         , filterF2 "f" "[.[] | f]") -- def map(f): [.[] | f];
          ,("with_entries", filterF2 "f" "to_entries | map(f) | from_entries") -- "def with_entries(f): to_entries | map(f) | from_entries;"
          ,("range"       , rangeF2)
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
          ,("jsystem"     , filterF2 "f" "tojson | system(f) | fromjson")
          ]

unknown :: Int -> Name -> a
unknown a nm = error $ nm ++ " is not defined (arity " ++ show a ++ ")"

lookupOp :: HashMap Name a -> Int -> Name -> a
lookupOp tbl a nm = fromMaybe (unknown a nm) (H.lookup nm tbl)

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

rangeF4 :: Filter -> Filter -> Filter -> Filter
rangeF4 f g h x =
  [ r | fx <- f x, gx <- g x, hx <- h x, r <- rangeOp fx gx hx ]

rangeF3 :: Filter -> Filter -> Filter
rangeF3 f g = rangeF4 f g (constF $ Number 1)

rangeF2 :: Filter -> Filter
rangeF2 = rangeF3 (constF $ Number 0)

isemptyF :: Filter -> Filter
isemptyF f v = [Bool $ List.null (f v)]

_inF :: Filter -> Filter -> Filter
_inF source elems v = [Bool $ any (`elem` elems v) (source v)]

-- The use of `ceiling` seems consistent with jq.
limitF :: Filter -> Filter -> Filter
limitF f g v =
  [ r | Number l <- f v, r <- List.take (ceiling l) (g v) ]

nthF :: Filter -> Filter -> Filter
nthF f g v = [ nth (ceiling l :: Int) (g v) | Number l <- f v ]
  where
    nth n _ | n < 0 = err ["nth doesn't support negative indices"]
    nth _ []        = Null
    nth 0 (x:_)     = x
    nth n (x:xs)    = go (n - 1) x xs

    go _ x []     = x
    go 0 _ (y:_)  = y
    go n _ (y:ys) = go (n - 1) y ys

valueOp3 :: ValueOp3 -> Filter -> Filter -> Filter
valueOp3 op f g x = [ op x y z | z <- g x, y <- f x ]

filterOp3 :: Name -> Filter -> Filter -> Filter
filterOp3 = lookupOp tbl 3 where
  tbl = H.fromList $
            [("limit", limitF)
            ,("nth",   nthF)
            ,("IN",    _inF)
            ] ++
          -- NP definitions
            map (second valueOp3)
            [("replace"    , replaceOp)]
         ++ map (second (valueOp3 . op2to3))
            [("_plus"      , (+|))
            ,("_multiply"  , (*|))
            ,("_minus"     , (-|))
            ,("_divide"    , (/|))
            ,("_mod"       , (%|))
            ,("_less"      , boolOp2 (<))
            ,("_lesseq"    , boolOp2 (<=))
            ,("_greatereq" , boolOp2 (>=))
            ,("_greater"   , boolOp2 (>))
            ,("_equal"     , boolOp2 (==))
            ,("_notequal"  , boolOp2 (/=))
            ,("_and"       , boolOp2' (&&))
            ,("_or"        , boolOp2' (||))
            ,("_at"        , at)
            ]
         ++ [("range"      , rangeF3)
            ]

filterOp4 :: Name -> Filter -> Filter -> Filter -> Filter
filterOp4 = lookupOp tbl 4 where
  tbl = H.fromList $
            [("range"      , rangeF4)
            ]

filterOp :: Name -> [Filter] -> Filter
filterOp nm []      = filterOp1 nm
filterOp nm [f]     = filterOp2 nm f
filterOp nm [f,g]   = filterOp3 nm f g
filterOp nm [f,g,h] = filterOp4 nm f g h
filterOp nm fs      = unknown (length fs) nm

type Env a = a -> [Filter] -> Filter

extend :: Eq a => a -> Value -> Env a -> Env a
extend x z env y | x == y    = \_ _ -> pure z
                 | otherwise = env y

asF :: Eq a => Env a -> Filter -> a -> (Env a -> Filter) -> Filter
asF env f n g x = do
  y <- f x
  g (extend n y env) x

lastValue :: [Value] -> Value
lastValue [] = Null
lastValue [x] = x
lastValue (_:xs) = lastValue xs

reduceF :: Eq a => Env a -> Filter -> a -> Filter -> (Env a -> Filter) -> Filter
reduceF env input n nil cons x = do
  z <- nil x
  [List.foldl' f z $ input x]
  where
    f acc y = lastValue $ cons (extend n y env) acc

filter :: Eq a => Env a -> F a -> Filter
filter _   IdF               = pure
filter _   (ConstF v)        = constF v
filter env (CompF f g)       = filter env f >=> filter env g
filter env (BothF f g)       = bothF (filter env f) (filter env g)
filter env (ArrayF f)        = arrayF (filter env f)
filter env (ObjectF o)       = objectF (filter env <$> o)
filter env (OpF op fs)       = env op (filter env <$> fs)
filter env (IfF c t e)       = ifF (filter env c) (filter env t) (filter env e)
filter env (VarF n)          = env n []
filter env (AsF f n g)       = asF env (filter env f) n (`filter` g)
filter env (ReduceF f n g h) = reduceF env (filter env f) n (filter env g) (`filter` h)

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
    (yss,zss) -> (if null yss then id else (yss :))
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
        f (Object o)  = t"{" . cat (intersperse (t"\n,") (map g . H.toList $ o)) . t"}"
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
