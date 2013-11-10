{-# LANGUAGE PatternGuards, OverloadedStrings #-}
import Control.Applicative as A
import Prelude hiding (filter,sequence,Ordering(..))
import Data.Maybe
import Data.Char
import Data.List ((\\))
import Data.Monoid
import Data.Aeson
import Data.Aeson.Parser (jstring, value)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
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
import Data.Traversable (sequence)

type ValueOp1 = Value -> Value
type ValueOp2 = Value -> Value -> Value
type ValueOp3 = Value -> Value -> Value -> Value
type BoolOp2 = Value -> Value -> Bool
type Filter1 = Value -> [Value]
type Filter = [Value] -> [Value]
type Obj = HashMap Text

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
Number (I x) %| Number (I y) = Number (I (x `mod` y))
x            %| y            = err2 x y $ \x' y' -> [x', "and", y', "cannot be 'mod'ed"]

newtype NObj a = NObj (Obj a)
  deriving (Eq)
instance Ord a => Ord (NObj a) where
  x <= y | x == y = True
  _ <= _ = error "Not yet implemented: comparison of objects"

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

lengthFi :: Value -> Int
lengthFi Null       = 0
lengthFi (Array v)  = V.length v
lengthFi (Object o) = length . H.toList $ o
lengthFi (String s) = B.length . encodeUtf8 $ s
lengthFi x          = err1 x $ \x' -> [x', "has no length"]

lengthOp, keysOp, addOp :: ValueOp1

lengthOp = toJSON . lengthFi

keysOp (Array v)  = toJSON [0.. V.length v - 1]
keysOp (Object o) = toJSON $ H.keys o
keysOp x          = err1 x $ \x' -> [x', "has no keys"]

addOp = foldr (+|) Null . toList

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
-- TODO: substring, subarray, ...
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

objectF1 :: Obj Filter -> Filter1
objectF1 o x = fmap Object . dist . fmap (($[x])) $ o

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
op2F Contains = op2VF (boolOp2 contains)

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

data Op1 = Length | Keys | Add | Type | Min | Max
  deriving (Show)

data Op2 = At | Has | Select | Contains
  deriving (Show)

data Op3 = Plus | Minus | Times | Div | Mod | LT | LE | EQ | NE | GT | GE
  deriving (Show)

-- .key
keyF :: Text -> F
keyF = Op2F At . ConstF . String

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

valueOp1 :: Op1 -> ValueOp1
valueOp1 Keys   = keysOp
valueOp1 Length = lengthOp
valueOp1 Add    = addOp
valueOp1 Min    = minimum . toList
valueOp1 Max    = minimum . toList
valueOp1 Type   = String . T.pack . show . kindOf

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

filter :: F -> Filter
filter IdF           = id
filter (CompF f g)   = filter g . filter f
filter AllF          = allF
filter (BothF f g)   = bothF (filter f) (filter g)
filter (ArrayF f)    = arrayF (filter f)
filter (ObjectF o)   = objectF (fmap filter o)
filter (Op1F op)     = fmap (valueOp1 op)
filter (Op2F op f)   = op2F op (filter f)
filter (Op3F op f g) = op3F (valueOp3 op) (filter f) (filter g)
filter EmptyF        = emptyF
filter (ConstF v)    = constF v
filter (ErrorF msg)  = error msg

parseSimpleFilter, parseOpFilter, parseCommaFilter,
  parseNoCommaFilter, parseFilter, parseDotFilter, parseTopFilter :: Parser F

parseTopFilter = parseFilter <* skipSpace <* endOfInput
              <?> "toplevel filter"

parseDotFilter
  =  AllF    <$  string "[]"
 <|> Op2F At <$> (char '[' *> parseFilter <* tok ']')
 <|> keyF    <$> bareWord
 <|> pure IdF
 <?> "dot filter"

bareWord :: Parser Text
bareWord = T.pack <$> some (satisfy (\c -> isAscii c && isAlpha c))
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

parseOp1 :: Parser Op1
parseOp1
   =  Length <$ string "length"
  <|> Keys   <$ string "keys"
  <|> Add    <$ string "add"
  <?> "arity 1 operator (length, keys, add)"

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
  (  char '.'  *> skipSpace *> parseDotFilter
 <|> EmptyF   <$  string "empty"
 <|> ConstF   <$> parseOp0
 <|> Op1F     <$> parseOp1
 <|> parseOp2 <*  tok  '(' <*> parseFilter <* tok ')'
 <|> parseOp3 <*  tok  '(' <*> parseFilter <* tok ';' <*> parseFilter <* tok ')'
 <|> ArrayF   <$  char '[' <*> parseFilter <* tok ']'
 <|> ObjectF  <$> obj parseNoCommaFilter
 <|> char '('  *> parseFilter <* tok ')'
 <?> "simple filter"
  )

table :: [[Operator B.ByteString F]]
table   = [ [binary op AssocLeft | op <- [("*",Times),("/",Div),("%",Mod)]]
          , [binary op AssocLeft | op <- [("+",Plus),("-",Minus)]]
          , [binary op AssocNone | op <- [("<=",LE),("<",LT),("==",EQ),("!=",NE),(">=",GE),(">",GT)]]
          ]

binary :: (B.ByteString, Op3) -> Assoc -> Operator B.ByteString F
binary (name, fun) = Infix (Op3F fun <$ skipSpace <* string name)

parseOpFilter = buildExpressionParser table parseSimpleFilter
             <?> "op filter"

parseCommaFilter = concatF <$> parseOpFilter `sepBy` tok ','
                <?> "comma filter"

parseNoCommaFilter = composeF <$> parseOpFilter `sepBy` tok '|'
                  <?> "no comma filter"

parseFilter = composeF <$> parseCommaFilter `sepBy1` tok '|'
           <?> "filter"

obj :: Parser a -> Parser (Obj a)
obj p = char '{' *> objectValues (skipSpace *> (bareWord <|> jstring)) p

-- From: https://github.com/bos/aeson/blob/master/Data/Aeson/Parser/Internal.hs
objectValues :: Parser Text -> Parser a -> Parser (Obj a)
objectValues str val = do
  skipSpace
  let pair = do
        a <- str <* skipSpace
        b <- char ':' *> skipSpace *> val
        return (a,b)
  vals <- ((pair <* skipSpace) `sepBy` (char ',' *> skipSpace)) <* char '}'
  return (H.fromList vals)
{-# INLINE objectValues #-}

parseIO :: String -> Parser a -> L.ByteString -> IO a
parseIO msg p s =
    case L.parse p s of
      L.Done _ r -> return r
      L.Fail _ ctx msg' -> fail (msg <> ": " <> msg' <> " context:" <> show ctx)

stream :: Parser [Value]
stream = (value `sepBy` skipSpace) <* skipSpace <* endOfInput

main :: IO ()
main = do args <- getArgs
          let noinput = "-n" `elem` args
          -- -c is ignored
              [arg] = args \\ ["-n","-c"]
          f <- parseIO "parsing filter" parseTopFilter (L8.pack arg)
          -- print f
          input <- if noinput then
                     return [Null]
                   else
                     parseIO "JSON decoding" stream =<< L.getContents
          mapM_ (L8.putStrLn . encode) $ filter f input
