{-# LANGUAGE PatternGuards, OverloadedStrings #-}
import Control.Applicative as A
import Prelude hiding (filter)
import Data.Maybe
import Data.Char
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
import System.Environment

type ValueOp = Value -> Value
type ValueBinOp = Value -> Value -> Value
type Filter = [Value] -> [Value]
type Obj = HashMap Text

err2 :: Value -> Value -> String -> a
err2 x y msg = error . unwords $ [show x, "and", show y, msg]

vecDiff :: Vector Value -> Vector Value -> Vector Value
x `vecDiff` y = V.filter p x
  where p = not . (`S.member`s)
        s = S.fromList (V.toList y)

(+|), (-|), (/|), ( *| ) :: ValueBinOp

Number x +| Number y = Number (x + y)
String x +| String y = String (x <> y)
Array  x +| Array  y = Array  (x <> y)
Object x +| Object y = Object (y <> x) -- Right biased
x        +| y        = err2 x y "cannot be added"

Number x -| Number y = Number (x - y)
Array  x -| Array  y = Array (x `vecDiff` y)
x        -| y        = err2 x y "cannot be subtracted"

Number x *| Number y = Number (x * y)
x        *| y        = err2 x y "cannot be multiplied"

Number x /| Number y = Number (x / y)
x        /| y        = err2 x y "cannot be divided"

lengthFi :: Value -> Int
lengthFi Null       = 0
lengthFi (Array v)  = V.length v
lengthFi (Object o) = length . H.toList $ o
lengthFi (String s) = B.length . encodeUtf8 $ s
lengthFi (Number _) = error "number has no length"
lengthFi (Bool _)   = error "boolean has no length"

lengthOp :: ValueOp
lengthOp = toJSON . lengthFi

keysOp :: ValueOp
keysOp (Array v)  = toJSON [0.. V.length v - 1]
keysOp (Object o) = toJSON $ H.keys o
keysOp Number{}   = error "number has no keys"
keysOp Null       = error "null has no keys"
keysOp Bool{}     = error "boolean has no keys"
keysOp String{}   = error "string has no keys"

at :: Value -> Value -> Maybe Value
Object o `at` String s = H.lookup s o
Array  a `at` Number mn
  | Success n <- fromJSON (Number mn) = a V.!? n
_        `at` _ = Nothing

atF :: Value -> Filter
atF key = fmap (fromMaybe Null . (`at` key))

allF :: Filter
allF xs = [ y | Array ys <- xs, y <- V.toList ys ]

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
       | ConstF Value    -- 1, "foo", null
       | ErrorF String
  deriving (Show)

data Op = Length | Keys
  deriving (Show)

data BinOp = Plus | Minus | Times | Div
  deriving (Show)

-- .key
keyF :: Text -> F
keyF = AtF . String

-- def map(f): [.[] | f];
mapF :: F -> F
mapF f = ArrayF (AllF `CompF` f)

-- TODO: deal properly with "f + g - h" and "f - g + h"
binOpF :: BinOp -> [F] -> F
binOpF _  []    = IdF
binOpF _  [x]   = x
binOpF op [x,y] = Ap2F op x y
binOpF _  _     = error "binOpF: not supported yet"

concatF, composeF, sumF, productF, minusF, divF :: [F] -> F

concatF [] = IdF
concatF xs = foldr1 BothF xs

composeF [] = IdF
composeF xs = foldr1 CompF xs

sumF     = binOpF Plus
productF = binOpF Times
minusF   = binOpF Minus
divF     = binOpF Div

valueBinOp :: BinOp -> ValueBinOp
valueBinOp Plus  = (+|)
valueBinOp Times = (*|)
valueBinOp Minus = (-|)
valueBinOp Div   = (/|)

valueOp :: Op -> ValueOp
valueOp Keys   = keysOp
valueOp Length = lengthOp

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
filter (ConstF v)    = constF v
filter (ErrorF err)  = error err

parseSimpleFilter, parseTimesFilter, parseDivFilter,
  parseMinusFilter, parsePlusFilter, parseCommaFilter,
  parseNoCommaFilter, parseFilter, parseDotFilter :: Parser F

parseDotFilter
  =  pure AllF <* string "[]"
 <|> AtF <$> (char '[' *> value <* tok ']')
 <|> keyF <$> bareWord
 <|> pure IdF

bareWord :: Parser Text
bareWord = T.pack <$> some (satisfy (\c -> isAscii c && isAlpha c))

parseConstFilter :: Parser Value
parseConstFilter
  =  String <$> jstring
 <|> Number <$> number
 <|> pure (Bool True)     <* string "true"
 <|> pure (Bool False)    <* string "false"
 <|> pure Null            <* string "null"
 <|> pure (Array mempty)  <* string "[]"
 <|> pure (Object mempty) <* string "{}"

parseOp :: Parser Op
parseOp =  pure Length <* string "length"
       <|> pure Keys   <* string "keys"

tok :: Char -> Parser Char
tok c = skipSpace *> char c

parseSimpleFilter
  = skipSpace *>
  (  char '.' *> skipSpace *> parseDotFilter
 <|> string "empty" *> pure EmptyF
 <|> OpF <$> parseOp
 <|> mapF <$> (string "map" *> tok '(' *> parseFilter <* tok ')')
 <|> ConstF <$> parseConstFilter
 <|> ArrayF <$> (char '[' *> parseFilter <* tok ']')
 <|> ObjectF <$> obj parseNoCommaFilter
 <|> char '(' *> parseFilter <* tok ')'
  )

parseTimesFilter = productF <$> parseSimpleFilter `sepBy` (tok '*')

parseDivFilter = divF <$> parseTimesFilter `sepBy` (tok '/')

parsePlusFilter = sumF <$> parseDivFilter `sepBy` (tok '+')

parseMinusFilter = minusF <$> parsePlusFilter `sepBy` (tok '-')

parseCommaFilter = concatF <$> parseMinusFilter `sepBy` (tok ',')

parseNoCommaFilter = composeF <$> parseMinusFilter `sepBy` (tok '|')

parseFilter = composeF <$> parseCommaFilter `sepBy1` (tok '|')

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
main = do [arg] <- getArgs
          let Just f = parse (parseFilter <* skipSpace) (L8.pack arg)
          -- print f
          input <- maybe (fail "JSON decoding") return . parse (stream <* skipSpace) =<< L.getContents
          mapM_ (L8.putStrLn . encode) $ filter f input
