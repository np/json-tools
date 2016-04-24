{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
import Control.Applicative as A
import Control.Arrow (first,second,(***))
import Control.Monad ((<=<),(>=>))
import Data.Ord (comparing)
import Prelude hiding (filter,sequence,Ordering(..))
import Data.Maybe
import Data.Char
import Data.List ((\\),sort,sortBy,intersperse,nub)
import Data.Monoid
import Data.Aeson
import Data.Aeson.Parser (jstring, value)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Scientific hiding (scientific)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashSet as S
import Data.Attoparsec.ByteString.Char8 hiding (Result, parse)
import qualified Data.Attoparsec.Lazy as L
import Data.Attoparsec.Expr
import System.Environment
import Data.Traversable (Traversable(..),foldMapDefault)
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
x        *| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be multiplied"]

Number x /| Number y = Number (x / y)
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

lengthFi :: Value -> Int
lengthFi Null       = 0
lengthFi (Array v)  = V.length v
lengthFi (Object o) = length . H.toList $ o
lengthFi (String s) = T.length s
lengthFi x          = err1 x $ \x' -> [x', "has no length"]

lengthOp, keysOp, addOp, negateOp, sqrtOp, floorOp, sortOp,
  uniqueOp, toNumberOp, toStringOp, decodeOp, linesOp, unlinesOp,
  wordsOp, unwordsOp, tailOp, initOp, reverseOp :: ValueOp1

lengthOp = toJSON . lengthFi

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
toNumberOp (String s) = either (const e) Number $ parseM "number" scientific (cs s)
  where e = error $ "Invalid numeric literal (while parsing '" <> T.unpack s <> "'"
toNumberOp x     = err1 x $ \x' -> [x', "cannot be parsed as a number"]

toStringOp s@String{} = s
toStringOp x = String . cs . encode $ x

decodeOp (String s) = either error id . parseM "JSON value" value . cs $ s
decodeOp x = errK "decode" [(x,[KString])]

linesOp (String s) = Array (V.fromList . map String . T.lines $ s)
linesOp x = errK "lines" [(x,[KString])]

unlinesOp (Array v) = String (T.unlines . map fromString . V.toList $ v)
unlinesOp x = err1 x $ \x' -> ["cannot take unlines of", x', "(not an array of string)"]

wordsOp (String s) = Array (V.fromList . map String . T.words $ s)
wordsOp x = errK "words" [(x,[KString])]

unwordsOp (Array v) = String (T.unwords . map fromString . V.toList $ v)
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

fromString :: Value -> Text
fromString (String s) = s
fromString x          = err1 x $ \x' -> [x', "is not a string"]

endoTextOp :: String -> (T.Text -> T.Text) -> ValueOp1
endoTextOp _    textOp (String s) = String (textOp s)
endoTextOp name _      x          = errK name [(x,[KString])]

at, intercalateOp, intersperseOp, splitOp, chunksOp, takeOp, dropOp :: ValueOp2

intercalateOp (String s) (Array v) = String . T.intercalate s . map fromString . V.toList $ v
intercalateOp x y = errK "intercalate" [(x,[KString]),(y,[KArray])]

intersperseOp x (Array v) = Array . V.fromList . intersperse x . V.toList $ v
intersperseOp x y = errK "intersperse" [(x,kinds),(y,[KArray])]

splitOp (String x) (String y) = toJSON $ T.splitOn x y
splitOp x y = errK "split" [(x,[KString]),(y,[KString])]

chunksOp (Number n) (String s) = toJSON $ T.chunksOf (floor n) s
chunksOp x y = errK "chunks" [(x,[KNumber]),(y,[KString])]

takeOp (Number n) (String s) = String (T.take (floor n) s)
takeOp (Number n) (Array  v) = Array  (V.take (floor n) v)
takeOp x y = errK "take" [(x,[KNumber]),(y,[KString,KArray])]

dropOp (Number n) (String s) = String (T.drop (floor n) s)
dropOp (Number n) (Array  v) = Array  (V.drop (floor n) v)
dropOp x y = errK "drop" [(x,[KNumber]),(y,[KString,KArray])]

Object o `at` String s     = fromMaybe Null $ H.lookup s o
Array  a `at` Number n     = fromMaybe Null $ a V.!? floor n
Null     `at` String{}     = Null
Null     `at` Number{}     = Null
x        `at` y = err2 x y $ \x' y' -> ["Cannot index", x', "with", y']

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

toList :: Value -> [Value]
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

filterOp3 :: ValueOp3 -> Filter -> Filter -> Filter
filterOp3 op f g x = [ op x y z | z <- g x, y <- f x ]

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
  | AllF                -- .[]
  | BothF (F a) (F a)   -- f, g
  | ArrayF (F a)        -- [f]
  | ObjectF (Obj (F a)) -- {a: f, b: g}
  | OpF a [F a]         -- F, F(f₀;...;fn)
  | ConstF Value        -- 1, "foo", null
  | ErrorF String
  deriving (Show)

type F' = F Name

keyF :: Text -> F a
keyF = ConstF . String

-- f[g]
atF :: F' -> F' -> F'
atF = op3F "_at"

-- .key
atKeyF :: Text -> F'
atKeyF = atF IdF . keyF

trueValue :: Value -> Bool
trueValue (Bool b) = b
trueValue Null     = False
trueValue _        = True

selectF :: Filter -> Filter
selectF f x = [x | any trueValue (f x)]

concatF, composeF :: [F a] -> F a

concatF [] = IdF
concatF xs = foldr1 BothF xs

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

{-
subst :: (a -> [F b] -> F b) -> F a -> F b
subst _   IdF           = IdF
subst env (CompF f g)   = CompF (subst env f) (subst env g)
subst _   AllF          = AllF
subst env (BothF f g)   = BothF (subst env f) (subst env g)
subst env (ArrayF f)    = ArrayF (subst env f)
subst env (ObjectF o)   = ObjectF (fmap (subst env) o)
subst env (OpF op fs)   = env op (fmap (subst env) fs)
subst _   (ConstF v)    = ConstF v
subst _   (ErrorF msg)  = ErrorF msg
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
          ,("from_entries"  , filter filterOp fromEntriesF)]

  tbl' = map (second (pure .))
          [("keys"          , keysOp)
          ,("length"        , lengthOp)
          ,("add"           , addOp)
          ,("min"           , minimum . toList)
          ,("max"           , minimum . toList)
          ,("type"          , String . T.pack . show . kindOf)
          ,("to_entries"    , toEntriesOp)
          ,("negate"        , negateOp)
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
          -- NP extensions
          ,("encode"        , String . cs . encode)
          ,("decode"        , decodeOp)
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
          -- NP extensions
          ,("system"      , op2VF systemOp)
          ,("intercalate" , op2VF intercalateOp)
          ,("intersperse" , op2VF intersperseOp)
          ,("split"       , op2VF splitOp)
          ,("chunks"      , op2VF chunksOp)
          ,("take"        , op2VF takeOp)
          ,("drop"        , op2VF dropOp)
          -- NP definitions
          ,("jsystem"     , filterF2 "f" "encode | system(f) | decode")
          ]

unknown :: Int -> Name -> a
unknown a nm = error $ nm ++ " is not defined (arity " ++ show a ++ ")"

lookupOp :: HashMap Name a -> Int -> Name -> a
lookupOp tbl a nm = fromMaybe (unknown a nm) (H.lookup nm tbl)

replaceOp :: ValueOp3
replaceOp (String x) (String y) (String z) = String (T.replace y z x)
replaceOp x y z = err3 x y z $ \x' y' z' -> ["replace expects 3 string arguments not", x', y', z']

valueOp3 :: Name -> ValueOp3
valueOp3 = lookupOp tbl 3 where
  tbl = H.fromList $
            [("replace"    , replaceOp)]
         ++ map (second op2to3)
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

filterOp :: Name -> [Filter] -> Filter
filterOp nm []     = filterOp1 nm
filterOp nm [f]    = filterOp2 nm f
filterOp nm [f,g]  = filterOp3 (valueOp3 nm) f g
filterOp nm fs     = unknown (length fs) nm

filter :: (a -> [Filter] -> Filter) -> F a -> Filter
filter _   IdF           = pure
filter env (CompF f g)   = filter env f >=> filter env g
filter _   AllF          = toList
filter env (BothF f g)   = bothF (filter env f) (filter env g)
filter env (ArrayF f)    = arrayF (filter env f)
filter env (ObjectF o)   = objectF (fmap (filter env) o)
filter env (OpF op fs)   = env op (fmap (filter env) fs)
filter _   (ConstF v)    = constF v
filter _   (ErrorF msg)  = error msg

parseSimpleFilter, parseOpFilter, parseCommaFilter,
  parseNoCommaFilter, parseFilter, parseDotFilter, parseConcFilter :: Parser F'

parseDotFilter =
   parseAtFilters =<< (atKeyF <$> (skipSpace *> (bareWord <|> jstring)) <|> pure IdF)
  <?> "dot filter"

parseAtFilters :: F' -> Parser F'
parseAtFilters f =
  do g <-    f `CompF` AllF <$  (skipSpace *> string "[]")
         <|> atF f <$> (tok '[' *> parseFilter <* tok ']')
         <|> pure IdF
     case g of
       IdF -> return f
       _   -> parseAtFilters g

ident :: Parser String
ident = (:) <$> satisfy lic <*> many (satisfy ic)
  where lic c = c == '_' || isAscii c && isAlpha c
        ic  c = c == '_' || isAscii c && isAlphaNum c

bareWord :: Parser Text
bareWord = T.pack <$> ident
        <?> "bare word"

parseOp0 :: Parser Value
parseOp0
   =  String        <$> jstring
  <|> Number        <$> scientific
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
  (  composeF <$> many1 (char '.'  *> parseDotFilter)
 <|> ConstF   <$> parseOp0
 <|> OpF      <$> ident <*> (tok '(' *> parseFilter `sepBy` tok ';' <* tok ')' <|> pure [])
 <|> ArrayF   <$  char '[' <*> parseFilter <* tok ']'
 <|> ObjectF  <$> objectFilterP
 <|> char '('  *> parseFilter <* tok ')'
 <?> "simple filter"
  )

parseConcFilter = parseAtFilters =<< parseSimpleFilter
               <?> "conc filter"

table :: [[Operator B.ByteString F']]
table   = [ [binary op AssocLeft | op <- [("*","_multiply"),("/","_divide"),("%","_mod")]]
          , [binary op AssocLeft | op <- [("+","_plus"),("-","_minus")]]
          , [binary op AssocNone | op <- [("<=","_lesseq"),("<","_less"),("==","_equal")
                                         ,("!=","_notequal"),(">=","_greatereq"),(">","_greater")]]
          , [binary ("and","_and") AssocRight]
          , [binary ("or","_or") AssocRight]
          ]

binary :: (B.ByteString, Name) -> Assoc -> Operator B.ByteString F'
binary (name, fun) = Infix (op3F fun <$ skipSpace <* string name)

parseOpFilter = buildExpressionParser table parseConcFilter
             <?> "op filter"

parseCommaFilter = concatF <$> parseOpFilter `sepBy` tok ','
                <?> "comma filter"

parseNoCommaFilter = composeF <$> parseOpFilter `sepBy` tok '|'
                  <?> "no comma filter"

parseFilter = composeF <$> parseCommaFilter `sepBy1` tok '|'
           <?> "filter"

objectFilterP :: Parser (Obj F')
objectFilterP = Obj
             <$> (char '{' *> skipSpace *>
                 ((pair <* skipSpace) `sepBy` (char ',' *> skipSpace))
               <* char '}')
   where fill k = (keyF k , atKeyF k)
         keyFilterP =  ConstF . String <$> (bareWord <|> jstring)
                   <|> char '(' *> parseFilter <* tok ')'
                   <?> "key filter"
         pair =  (,)  <$> (keyFilterP <* skipSpace) <*> (char ':' *> skipSpace *> parseNoCommaFilter)
             <|> fill <$> bareWord

parseF :: String -> F'
parseF = either error id . parseM "filter" parseFilter . L8.pack

parseM :: String -> Parser a -> L.ByteString -> Either String a
parseM msg p s =
    case L.parse (top p) s of
      L.Done _ r -> Right r
      L.Fail _ ctx msg' -> Left (msg <> ": " <> msg' <> " context:" <> show ctx)

parseIO :: String -> Parser a -> L.ByteString -> IO a
parseIO msg p s = either fail return $ parseM msg p s

top :: Parser a -> Parser a
top p = p <* skipSpace <* endOfInput

stream :: Parser [Value]
stream = value `sepBy` skipSpace

readInput :: Bool -> IO [Value]
readInput True = return [Null]
readInput _    = parseIO "JSON decoding" stream =<< L.getContents

mainFilter :: Bool -> Bool -> String -> IO ()
mainFilter wrap_output noinput arg = do
  f <- parseIO "parsing filter" parseFilter (L8.pack arg)
  -- print f
  input <- readInput noinput
  mapM_ (outputValue wrap_output) $ concatMap (filter filterOp f) input

type TestCase = (L.ByteString,L.ByteString,[L.ByteString])

parseTestCase :: TestCase -> Either String (F',Value,[Value])
parseTestCase (prg,inp,out) =
   (,,) <$> parseM "test program" parseFilter prg
        <*> parseM "test input"   value       inp
        <*> parseM "test output"  stream      (L8.unwords out)

runTest :: Either String (F', Value, [Value]) -> IO ()
runTest (Left msg) = putStrLn msg >> putStrLn (color 31 "ERROR\n")
runTest (Right {-test@-}(f, input, reference)) =
  let output = filter filterOp f input in
  if output == reference then
    {-print test >>-} putStrLn (color 32 "PASS\n")
  else do
    putStrLn "was expected, but instead this is the output"
    mapM_ (outputValue False) output
    putStrLn (color 31 "FAIL\n")

color :: Int -> String -> String
color n = ("\^[["++) . shows n . ('m':) . (++ "\^[[m")

printTestCase :: TestCase -> IO TestCase
printTestCase t@(x,y,zs) = mapM_ L8.putStrLn (x:y:zs) >> return t

runTests :: IO ()
runTests = mapM_ (runTest . parseTestCase <=< printTestCase)
         . fmap splitTestCase
         . splitOnEmptyLines
         . fmap dropComment
         . L8.lines
       =<< L.getContents

splitTestCase :: [a] -> (a,a,[a])
splitTestCase (x:y:zs) = (x,y,zs)
splitTestCase _        = error "splitTestCase: too few lines for a test case"

splitOnEmptyLines :: [L.ByteString] -> [[L.ByteString]]
splitOnEmptyLines []  = []
splitOnEmptyLines xss =
  case break L.null (dropWhile L.null xss) of
    (yss,zss) -> yss : splitOnEmptyLines zss

dropComment :: L.ByteString -> L.ByteString
dropComment s
  | "#" `L.isPrefixOf` s = L.empty
  | otherwise            = s

outputValue :: Bool -> Value -> IO ()
outputValue False = L8.putStrLn . encode
outputValue True  = mapM_ L.putStr . ($["\n"]) . f
  where f (Array a)   = t"[" . cat (intersperse (t"\n,") (map j . V.toList $ a)) . t"]"
        f (Object o)  = t"{" . cat (intersperse (t"\n,") (map g . H.toList $ o)) . t"}"
        f x           = j x
        g (key, val)  = t (encode key) . t(L8.pack ":") . j val
        j             = t . encode
        t x           = (x:)
        cat           = appEndo . mconcat . map Endo

main :: IO ()
main = do args <- getArgs
          if "--run-tests" `elem` args then
            runTests
            else do
              -- -c is ignored
              let [arg] = args \\ ["-n","-c","--run-tests","-w"]
              mainFilter ("-w" `elem` args) ("-n" `elem` args) arg
