{-# LANGUAGE PatternGuards, OverloadedStrings #-}
import Control.Applicative as A
import Prelude hiding (filter)
import Data.Maybe
import Data.Char
import Data.List (transpose, (\\))
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

type ValueOp = Value -> Value
type ValueBinOp = Value -> Value -> Value
type BoolBinOp = Value -> Value -> Bool
type Filter1 = Value -> [Value]
type Filter = [Value] -> [Value]
type Obj = HashMap Text

lift :: Filter1 -> Filter
lift = concatMap
-- lift f xs = [ r | x <- xs, r <- f x ]

data Type = TyNull | TyNumber | TyString | TyBool | TyArray | TyObject

instance Show Type where
  show TyNull = "null"
  show TyNumber = "number"
  show TyString = "string"
  show TyBool = "boolean"
  show TyArray = "array"
  show TyObject = "object"

typeOf :: Value -> Type
typeOf Null     = TyNull
typeOf Number{} = TyNumber
typeOf String{} = TyString
typeOf Bool{}   = TyBool
typeOf Array{}  = TyArray
typeOf Object{} = TyObject

err :: [String] -> a
err = error . unwords

err2 :: Value -> Value -> (String -> String -> [String]) -> a
err2 x y msg = err (msg (show (typeOf x)) (show (typeOf y)))

err1 :: Value -> (String -> [String]) -> a
err1 x msg = err (msg (show (typeOf x)))

vecDiff :: Vector Value -> Vector Value -> Vector Value
x `vecDiff` y = V.filter p x
  where p = not . (`S.member`s)
        s = S.fromList (V.toList y)

(+|), (-|), (/|), ( *| ), (%|) :: ValueBinOp

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


-- TODO
-- instance Ord Value where

(<|), (<=|) :: BoolBinOp

Null     <=| _        = True
Number x <=| Number y = x < y
-- less flexible than ./jq
x        <=| y        = err2 x y $ \x' y' -> [x', "and", y', "cannot be compared"]

x <| y | x /= y    = x <=| y
       | otherwise = False

boolBinOp :: BoolBinOp -> ValueBinOp
boolBinOp f x y = Bool (f x y)

lengthFi :: Value -> Int
lengthFi Null       = 0
lengthFi (Array v)  = V.length v
lengthFi (Object o) = length . H.toList $ o
lengthFi (String s) = B.length . encodeUtf8 $ s
lengthFi x          = err1 x $ \x' -> [x', "has no length"]

lengthOp, keysOp, addOp, transposeOp :: ValueOp

lengthOp = toJSON . lengthFi

keysOp (Array v)  = toJSON [0.. V.length v - 1]
keysOp (Object o) = toJSON $ H.keys o
keysOp x          = err1 x $ \x' -> [x', "has no keys"]

addOp = foldr (+|) Null . toList
transposeOp (Array v) = Array . V.fromList
                              . map (Array . V.fromList)
                              . transpose
                              $ [ V.toList w | Array w <- V.toList v ]
transposeOp x         = err1 x $ \x' -> [x', "cannot be transposed"]

at :: Value -> Value -> Value
Object o `at` String s     = fromMaybe Null $ H.lookup s o
Array  a `at` Number (I n) = fromMaybe Null $ a V.!? (fromInteger n)
Array  a `at` Number (D d) = fromMaybe Null $ a V.!? (floor d)
Null     `at` String{}     = Null
Null     `at` Number{}     = Null
x        `at` y = err2 x y $ \x' y' -> ["Cannot index", x', "with", y']

atF :: Value -> Filter
atF key = fmap (`at`key)

toList :: Value -> [Value]
toList (Array v)  = V.toList v
toList (Object o) = H.elems o
toList x          = err1 x $ \x' -> ["Cannot iterate over", x']

allF :: Filter
allF = lift toList

bothF :: Filter -> Filter -> Filter
bothF f g xs = f xs ++ g xs

arrayF :: Filter -> Filter
arrayF f xs = [Array (V.fromList . f $ xs)]

objectF :: Obj Filter -> Filter
objectF o xs = [Object . H.fromList $ [ (k,y) | (k,f) <- H.toList o, y <- f xs ]]

{- Confusing:
$ jq -n -c '[(1,2,3,4) | .+.]'
[2,4,6,8]

$ jq -n -c '[(1,2,3,4) | .+1]'
[2,3,4,5]

But:

$ jq -n -c '[(1,2)+(2,1)]'
[3,4,2,3]

instead of [3,3]
-}
ap2F :: ValueBinOp -> Filter -> Filter -> Filter
-- ap2F op f g xs = [ op x y | x <- f xs, y <- g xs ]
ap2F op f g xs = zipWith op (f xs) (g xs)

emptyF :: Filter
emptyF _ = []

constF :: Value -> Filter
constF v xs = [ v | _ <- xs ]

-- Filter
data F = IdF             -- .
       | CompF F F       -- f | g
       | AtF Value       -- .[key]
       | AllF            -- .[]
       | BothF F F       -- f, g
       | ArrayF F        -- [f]
       | ObjectF (Obj F) -- {a: f, b: g}
       | EmptyF          -- empty
       | OpF Op          -- length, keys
       | Ap2F BinOp F F  -- f + g, f - g, f / g, f * g
       | SelectF F       -- select(f)
       | ConstF Value    -- 1, "foo", null
       | ErrorF String
  deriving (Show)

data Op = Length | Keys | Add | Transpose
  deriving (Show)

data BinOp = Plus | Minus | Times | Div | Mod | LT | LE | EQ | NE | GT | GE
  deriving (Show)

-- .key
keyF :: Text -> F
keyF = AtF . String

-- def map(f): [.[] | f];
mapF :: F -> F
mapF f = ArrayF (AllF `CompF` f)

trueValue :: Value -> Bool
trueValue (Bool b) = b
trueValue Null     = False
trueValue _        = True

selectF :: Filter -> Filter
selectF f xs = [ x | x <- xs, any trueValue (f [x]) ]

-- TODO: deal properly with "f + g - h" and "f - g + h"
binOpF :: BinOp -> [F] -> F
binOpF _  []    = IdF
binOpF _  [x]   = x
binOpF op [x,y] = Ap2F op x y
binOpF _  _     = error "binOpF: not supported yet"

concatF, composeF :: [F] -> F

concatF [] = IdF
concatF xs = foldr1 BothF xs

composeF [] = IdF
composeF xs = foldr1 CompF xs

valueBinOp :: BinOp -> ValueBinOp
valueBinOp Plus  = (+|)
valueBinOp Times = (*|)
valueBinOp Minus = (-|)
valueBinOp Div   = (/|)
valueBinOp Mod   = (%|)
valueBinOp LT    = boolBinOp (<|)
valueBinOp LE    = boolBinOp (<=|)
valueBinOp GE    = boolBinOp $ flip (<|)
valueBinOp GT    = boolBinOp $ flip (<=|)
valueBinOp EQ    = boolBinOp (==)
valueBinOp NE    = boolBinOp (/=)

valueOp :: Op -> ValueOp
valueOp Keys   = keysOp
valueOp Length = lengthOp
valueOp Add    = addOp
valueOp Transpose = transposeOp

filter :: F -> Filter
filter IdF           = id
filter (CompF f g)   = filter g . filter f
filter (AtF key)     = atF key
filter AllF          = allF
filter (BothF f g)   = bothF (filter f) (filter g)
filter (ArrayF f)    = arrayF (filter f)
filter (ObjectF o)   = objectF (fmap filter o)
filter (Ap2F op f g) = ap2F (valueBinOp op) (filter f) (filter g)
filter EmptyF        = emptyF
filter (OpF op)      = fmap (valueOp op)
filter (SelectF f)   = selectF (filter f)
filter (ConstF v)    = constF v
filter (ErrorF msg)  = error msg

parseSimpleFilter, parseOpFilter, parseCommaFilter,
  parseNoCommaFilter, parseFilter, parseDotFilter :: Parser F

parseDotFilter
  =  AllF <$ string "[]"
 <|> AtF <$> (char '[' *> value <* tok ']')
 <|> keyF <$> bareWord
 <|> pure IdF

bareWord :: Parser Text
bareWord = T.pack <$> some (satisfy (\c -> isAscii c && isAlpha c))

parseConstFilter :: Parser Value
parseConstFilter
  =  String        <$> jstring
 <|> Number        <$> number
 <|> Bool True     <$  string "true"
 <|> Bool False    <$  string "false"
 <|> Null          <$  string "null"
 <|> Array mempty  <$  string "[]"
 <|> Object mempty <$  string "{}"

parseOp :: Parser Op
parseOp =  Length    <$ string "length"
       <|> Keys      <$ string "keys"
       <|> Add       <$ string "add"
       <|> Transpose <$ string "transpose"

tok :: Char -> Parser Char
tok c = skipSpace *> char c

parseSimpleFilter
  = skipSpace *>
  (  char '.' *> skipSpace *> parseDotFilter
 <|> EmptyF <$ string "empty"
 <|> OpF <$> parseOp
 <|> SelectF <$> (string "select" *> tok '(' *> parseFilter <* tok ')')
 <|> mapF <$> (string "map" *> tok '(' *> parseFilter <* tok ')')
 <|> ConstF <$> parseConstFilter
 <|> ArrayF <$> (char '[' *> parseFilter <* tok ']')
 <|> ObjectF <$> obj parseNoCommaFilter
 <|> char '(' *> parseFilter <* tok ')'
  )

table :: [[Operator B.ByteString F]]
table   = [ [binary op AssocLeft | op <- [("*",Times),("/",Div),("%",Mod)]]
          , [binary op AssocLeft | op <- [("+",Plus),("-",Minus)]]
          , [binary op AssocNone | op <- [("<",LT),("<=",LE),("==",EQ),("!=",NE),(">",GT),(">=",GE)]]
          ]

binary :: (B.ByteString, BinOp) -> Assoc -> Operator B.ByteString F
binary (name, fun) = Infix (Ap2F fun <$ string name)

parseOpFilter = buildExpressionParser table parseSimpleFilter

parseCommaFilter = concatF <$> parseOpFilter `sepBy` tok ','

parseNoCommaFilter = composeF <$> parseOpFilter `sepBy` tok '|'

parseFilter = composeF <$> parseCommaFilter `sepBy1` tok '|'

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

parse :: Parser a -> L.ByteString -> Maybe a
parse p s =
    case L.parse p s of
      L.Done rest v | L.null rest -> Just v
      _ -> Nothing
{-# INLINE parse #-}

stream :: Parser [Value]
stream = value `sepBy` skipSpace

main :: IO ()
main = do args <- getArgs
          let noinput = "-n" `elem` args
              [arg] = args \\ ["-n"]
          let Just f = parse (parseFilter <* skipSpace) (L8.pack arg)
          -- print f
          input <- if noinput then
                     return [Null]
                   else
                     maybe (fail "JSON decoding") return . parse (stream <* skipSpace) =<< L.getContents
          mapM_ (L8.putStrLn . encode) $ filter f input
