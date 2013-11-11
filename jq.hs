{-# LANGUAGE PatternGuards, OverloadedStrings #-}
import Control.Applicative as A
import Control.Arrow (first,(***))
import Control.Monad ((<=<))
import Data.Ord (comparing)
import Prelude hiding (filter,sequence,Ordering(..))
import Data.Maybe
import Data.Char
import Data.List ((\\),sort,sortBy)
import Data.Monoid
import Data.Aeson
import Data.Aeson.Parser (jstring, value)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashSet as S
import Data.Attoparsec.Char8 hiding (Result, parse)
import qualified Data.Attoparsec.Lazy as L
import Data.Attoparsec.Expr
import System.Environment
import Data.Traversable (Traversable(..),foldMapDefault)
import Data.Foldable (Foldable(foldMap))

type ValueOp1 = Value -> Value
type ValueOp2 = Value -> Value -> Value
type ValueOp3 = Value -> Value -> Value -> Value
type BoolOp2 = Value -> Value -> Bool
type Filter1 = Value -> [Value]
type Filter = [Value] -> [Value]

newtype Obj a = Obj { unObj :: [(a,a)] }
  deriving (Eq, Show)

instance Functor Obj where
  fmap f = Obj . fmap (f *** f) . unObj

instance Traversable Obj where
  traverse f (Obj o) = Obj <$> traverse trPair o
    where trPair (x,y) = (,) <$> f x <*> f y

instance Foldable Obj where
  foldMap = foldMapDefault

lift :: Filter1 -> Filter
lift = concatMap
-- lift f xs = [ r | x <- xs, r <- f x ]

data Kind = KNull | KNumber | KString | KBool | KArray | KObject
  deriving (Eq)

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

err1 :: Value -> (String -> [String]) -> a
err1 x msg = err (msg (show (kindOf x)))

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

-- Only integers so far
Number (I x) %| Number (I y) = Number (I (x `rem` y))
x            %| y            = err2 x y $ \x' y' -> [x', "and", y', "cannot be 'mod'ed"]

newtype NObj a = NObj (HashMap Text a)
  deriving (Eq)
instance Ord a => Ord (NObj a) where
  x <= y | x == y = True
  NObj x <= NObj y = f x <= f y where f = sortBy (comparing fst) . H.toList

instance Ord Value where
  Null     <= _        = True
  Bool   x <= Bool y   = x <= y
  Bool   _ <= _        = True
  Number x <= Number y = x <= y
  Number _ <= _        = True
  String x <= String y = x <= y
  String _ <= _        = True
  Array  x <= Array  y = x <= y
  Array  _ <= _        = True
  Object x <= Object y = NObj x <= NObj y
  Object _ <= _        = False

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

lengthOp, keysOp, addOp, negateOp, sqrtOp, floorOp, sortOp, toNumberOp, toStringOp :: ValueOp1

lengthOp = toJSON . lengthFi

keysOp (Array v)  = toJSON [0.. V.length v - 1]
keysOp (Object o) = toJSON . sort . H.keys $ o
keysOp x          = err1 x $ \x' -> [x', "has no keys"]

addOp = foldr (+|) Null . toList

negateOp (Number n) = Number (negate n)
negateOp x          = err1 x $ \x' -> [x', "cannot be negated"]

toDouble :: Number -> Double
toDouble (D d) = d
toDouble (I i) = fromIntegral i

sqrtOp (Number n) = Number (D (sqrt (toDouble n)))
sqrtOp x          = err1 x $ \x' -> [x', "has no square root"]

floorOp (Number n) = Number (I (floor (toDouble n)))
floorOp x          = err1 x $ \x' -> [x', "cannot be floored"]

sortOp (Array v) = Array (V.fromList . sort . V.toList $ v)
sortOp x         = err1 x $ \x' -> [x', "cannot be sorted, as it is not an array"]

toNumberOp n@Number{} = n
toNumberOp (String s) = either (const e) Number $ parseM "number" number (L8.pack . T.unpack $ s)
  where e = error $ "Invalid numeric literal (while parsing '" <> T.unpack s <> "'"
toNumberOp x     = err1 x $ \x' -> [x', "cannot be parsed as a number"]

toStringOp s@String{} = s
toStringOp x = String . T.pack . L8.unpack . encode $ x

at :: ValueOp2
Object o `at` String s     = fromMaybe Null $ H.lookup s o
Array  a `at` Number (I n) = fromMaybe Null $ a V.!? (fromInteger n)
Array  a `at` Number (D d) = fromMaybe Null $ a V.!? (floor d)
Null     `at` String{}     = Null
Null     `at` Number{}     = Null
x        `at` y = err2 x y $ \x' y' -> ["Cannot index", x', "with", y']

has :: BoolOp2
Object o `has` String s     = H.member s o
Array  a `has` Number (I n) = fromInteger n < V.length a
Array  a `has` Number (D d) = floor d < V.length a
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

allF :: Filter
allF = lift toList

bothF1 :: Filter -> Filter -> Filter1
bothF1 f g x = f [x] ++ g [x]

bothF :: Filter -> Filter -> Filter
bothF f g = lift (bothF1 f g)

arrayF :: Filter -> Filter
arrayF f xs = [Array (V.fromList $ f [x]) | x <- xs]

dist :: Obj [Value] -> [Obj Value]
dist = sequence

asObjectKey :: Value -> Text
asObjectKey (String x) = x
asObjectKey x          = err1 x $ \x' -> ["Cannot use", x', "as object key"]

objectF1 :: Obj Filter -> Filter1
objectF1 o x = fmap (Object . H.fromList . fmap (first asObjectKey) . unObj) . dist . fmap ($[x]) $ o

objectF :: Obj Filter -> Filter
objectF = lift . objectF1

op2VF1 :: ValueOp2 -> Filter -> Filter1
op2VF1 op f x = [ op x y | y <- f [x] ]

op2VF :: ValueOp2 -> Filter -> Filter
op2VF op f = lift (op2VF1 op f)

op2F :: Op2 -> Filter -> Filter
op2F Select   = selectF
op2F At       = op2VF at
op2F Has      = op2VF (boolOp2 has)
op2F Contains = op2VF (boolOp2 (flip contains))

op3F1 :: ValueOp3 -> Filter -> Filter -> Filter1
op3F1 op f g x = [ op x y z | z <- g [x], y <- f [x] ]

op3F :: ValueOp3 -> Filter -> Filter -> Filter
op3F op f g = lift (op3F1 op f g)

op2to3 :: ValueOp2 -> ValueOp3
op2to3 f _ y z = f y z

emptyF :: Filter
emptyF _ = []

constF :: Value -> Filter
constF v xs = [ v | _ <- xs ]

-- Filter
data F = IdF             -- .
       | CompF F F       -- f | g
       | AllF            -- .[]
       | BothF F F       -- f, g
       | ArrayF F        -- [f]
       | ObjectF (Obj F) -- {a: f, b: g}
       | EmptyF          -- empty
       | Op1F Op1        -- length, keys, add
       | Op2F Op2 F      -- .[f], select(f), has(f), ...
       | Op3F Op3 F F    -- f + g, f - g, f / g, f * g
       | ConstF Value    -- 1, "foo", null
       | ErrorF String
  deriving (Show)

data Op1 = Length | Keys | Add | Type | Min | Max | ToEntries
         | ToNumber | ToString | Negate | Floor | Sqrt | Sort
         | Not
  deriving (Show)

data Op2 = At | Has | Select | Contains
  deriving (Show)

data Op3 = Plus | Minus | Times | Div | Mod
         | LT | LE | EQ | NE | GT | GE
         | And | Or
  deriving (Show)

keyF :: Text -> F
keyF = ConstF . String

-- .key
atKeyF :: Text -> F
atKeyF = Op2F At . keyF

-- def map(f): [.[] | f];
mapF :: F -> F
mapF f = ArrayF (AllF `CompF` f)

trueValue :: Value -> Bool
trueValue (Bool b) = b
trueValue Null     = False
trueValue _        = True

selectF :: Filter -> Filter
selectF f xs = [ x | x <- xs, any trueValue (f [x]) ]

concatF, composeF :: [F] -> F

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
toEntriesOp (Array  v) = toEntries . zip (fmap (Number . I) [0..]) . V.toList $ v
toEntriesOp x = err1 x $ \x' -> [x', "has no keys"]

fromEntriesF :: F
fromEntriesF = parseF "map({(.key): .value}) | add"

op1F :: Op1 -> Filter
op1F Keys   = fmap $ keysOp
op1F Length = fmap $ lengthOp
op1F Add    = fmap $ addOp
op1F Min    = fmap $ minimum . toList
op1F Max    = fmap $ minimum . toList
op1F Type   = fmap $ String . T.pack . show . kindOf
op1F ToEntries = fmap $ toEntriesOp
op1F Negate = fmap $ negateOp
op1F Sqrt   = fmap $ sqrtOp
op1F Floor  = fmap $ floorOp
op1F Sort   = fmap $ sortOp
op1F ToNumber = fmap toNumberOp
op1F ToString = fmap toStringOp
op1F Not      = fmap $ Bool . not . trueValue

valueOp3 :: Op3 -> ValueOp3
valueOp3 = op2to3 . valueOp3need2

valueOp3need2 :: Op3 -> ValueOp2
valueOp3need2 Plus  = (+|)
valueOp3need2 Times = (*|)
valueOp3need2 Minus = (-|)
valueOp3need2 Div   = (/|)
valueOp3need2 Mod   = (%|)
valueOp3need2 LT    = boolOp2 (<)
valueOp3need2 LE    = boolOp2 (<=)
valueOp3need2 GE    = boolOp2 (>=)
valueOp3need2 GT    = boolOp2 (>)
valueOp3need2 EQ    = boolOp2 (==)
valueOp3need2 NE    = boolOp2 (/=)
valueOp3need2 And   = boolOp2' (&&)
valueOp3need2 Or    = boolOp2' (||)

filter :: F -> Filter
filter IdF           = id
filter (CompF f g)   = filter g . filter f
filter AllF          = allF
filter (BothF f g)   = bothF (filter f) (filter g)
filter (ArrayF f)    = arrayF (filter f)
filter (ObjectF o)   = objectF (fmap filter o)
filter (Op1F op)     = op1F op
filter (Op2F op f)   = op2F op (filter f)
filter (Op3F op f g) = op3F (valueOp3 op) (filter f) (filter g)
filter EmptyF        = emptyF
filter (ConstF v)    = constF v
filter (ErrorF msg)  = error msg

parseSimpleFilter, parseOpFilter, parseCommaFilter,
  parseNoCommaFilter, parseFilter, parseDotFilter, parseAtFilters,
  parseConcFilter :: Parser F

compP :: Parser F -> Parser F -> Parser F
compP p q = CompF <$> p <*> q

parseDotFilter =
   ((atKeyF <$> (skipSpace *> (bareWord <|> jstring)) <|> pure IdF)
    `compP`
    parseAtFilters)
  <?> "dot filter"

parseAtFilters =
  composeF <$>
  many (  AllF    <$  (skipSpace *> string "[]")
      <|> Op2F At <$> (tok '[' *> parseFilter <* tok ']')
       )

bareWord :: Parser Text
bareWord = T.pack <$> some (satisfy (\c -> c == '_' || isAscii c && isAlpha c))
        <?> "bare word"

parseOp0 :: Parser Value
parseOp0
   =  String        <$> jstring
  <|> Number        <$> number
  <|> Bool True     <$  string "true"
  <|> Bool False    <$  string "false"
  <|> Null          <$  string "null"
  <|> Array mempty  <$  string "[]"
  <|> Object mempty <$  string "{}"
  <?> "arity 0 operator (\"a\", 42, true, null, [], {}, ...)"

parseOp1 :: Parser F
parseOp1
   =  Op1F Length <$ string "length"
  <|> Op1F Keys   <$ string "keys"
  <|> Op1F Add    <$ string "add"
  <|> Op1F Type   <$ string "type"
  <|> Op1F Min    <$ string "min"
  <|> Op1F Max    <$ string "max"
  <|> Op1F ToEntries <$ string "to_entries"
  <|> fromEntriesF <$ string "from_entries"
  <|> Op1F Negate <$ (string "negate" <|> string "_negate")
  <|> Op1F Sqrt   <$ (string "sqrt"  <|> string "_sqrt")
  <|> Op1F Floor  <$ (string "floor" <|> string "_floor")
  <|> Op1F Sort   <$ string "sort"
  <|> Op1F ToNumber <$ string "tonumber"
  <|> Op1F ToString <$ string "tostring"
  <|> Op1F Not    <$ string "not"
  <?> "arity 1 operator (length, keys, add, ...)"

parseOp2 :: Parser (F -> F)
parseOp2
   =  Op2F Select   <$ string "select"
  <|> mapF          <$ string "map"
  <|> Op2F Has      <$ string "has"
  <|> Op2F Contains <$ string "contains"
  <?> "arity 2 operator (select, map, has, contains)"

parseOp3 :: Parser (F -> F -> F)
parseOp3 =  Op3F Plus  <$ string "_plus"
        <|> Op3F Minus <$ string "_minus"
        <|> Op3F Times <$ string "_multiply"
        <|> Op3F Div   <$ string "_divide"
        <|> Op3F Mod   <$ string "_mod"
        <|> Op3F EQ    <$ string "_equal"
        <|> Op3F NE    <$ string "_notequal"
        <|> Op3F LT    <$ string "_less"
        <|> Op3F GT    <$ string "_greater"
        <|> Op3F LE    <$ string "_lesseq"
        <|> Op3F GE    <$ string "_greatereq"
        <?> "arity 3 operator (_plus, _minus...)"

tok :: Char -> Parser Char
tok c = skipSpace *> char c

parseSimpleFilter
  =  skipSpace *>
  (  composeF <$> many1 (char '.'  *> parseDotFilter)
 <|> EmptyF   <$  string "empty"
 <|> ConstF   <$> parseOp0
 <|> parseOp1
 <|> parseOp2 <*  tok  '(' <*> parseFilter <* tok ')'
 <|> parseOp3 <*  tok  '(' <*> parseFilter <* tok ';' <*> parseFilter <* tok ')'
 <|> ArrayF   <$  char '[' <*> parseFilter <* tok ']'
 <|> ObjectF  <$> objectFilterP
 <|> char '('  *> parseFilter <* tok ')'
 <?> "simple filter"
  )

parseConcFilter = parseSimpleFilter `compP` parseAtFilters
               <?> "conc filter"

table :: [[Operator B.ByteString F]]
table   = [ [binary op AssocLeft | op <- [("*",Times),("/",Div),("%",Mod)]]
          , [binary op AssocLeft | op <- [("+",Plus),("-",Minus)]]
          , [binary op AssocNone | op <- [("<=",LE),("<",LT),("==",EQ),("!=",NE),(">=",GE),(">",GT)]]
          , [binary ("and",And) AssocRight]
          , [binary ("or",Or) AssocRight]
          ]

binary :: (B.ByteString, Op3) -> Assoc -> Operator B.ByteString F
binary (name, fun) = Infix (Op3F fun <$ skipSpace <* string name)

parseOpFilter = buildExpressionParser table parseConcFilter
             <?> "op filter"

parseCommaFilter = concatF <$> parseOpFilter `sepBy` tok ','
                <?> "comma filter"

parseNoCommaFilter = composeF <$> parseOpFilter `sepBy` tok '|'
                  <?> "no comma filter"

parseFilter = composeF <$> parseCommaFilter `sepBy1` tok '|'
           <?> "filter"

objectFilterP :: Parser (Obj F)
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

parseF :: String -> F
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

mainFilter :: Bool -> String -> IO ()
mainFilter noinput arg = do
  f <- parseIO "parsing filter" parseFilter (L8.pack arg)
  -- print f
  input <- readInput noinput
  mapM_ (L8.putStrLn . encode) $ filter f input

type TestCase = (L.ByteString,L.ByteString,[L.ByteString])

parseTestCase :: TestCase -> Either String (F,Value,[Value])
parseTestCase (prg,inp,out) =
   (,,) <$> parseM "test program" parseFilter prg
        <*> parseM "test input"   value       inp
        <*> parseM "test output"  stream      (L8.unwords out)

runTest :: Either String (F, Value, [Value]) -> IO ()
runTest (Left msg) = putStrLn msg >> putStrLn (color 31 "ERROR\n")
runTest (Right {-test@-}(f, input, reference)) =
  let output = filter f [input] in
  if output == reference then
    {-print test >>-} putStrLn (color 32 "PASS\n")
  else do
    putStrLn "was expected, but instead this is the output"
    mapM_ (L8.putStrLn . encode) output
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
  case span (not . L.null) (dropWhile L.null xss) of
    (yss,zss) -> yss : splitOnEmptyLines zss

dropComment :: L.ByteString -> L.ByteString
dropComment s
  | "#" `L.isPrefixOf` s = L.empty
  | otherwise            = s

main :: IO ()
main = do args <- getArgs
          if "--run-tests" `elem` args then
            runTests
            else do
              -- -c is ignored
              let [arg] = args \\ ["-n","-c","--run-tests"]
              mainFilter ("-n" `elem` args) arg
