
import Data.Char

d, a, a', o, o', s, i, r, e, spaces :: (String -> String) -> String -> String

json :: String -> String
json = d (spaces eoi)

spaces k (c:xs) | isSpace c = c : spaces k xs
spaces k xs = k xs

eoi :: String -> String
eoi [] = []
eoi xs = error $ "end of input was expected but " ++ show xs ++ " was found"

-- 'd'ocument
d k []                       = k []
d k ('[':xs)                 = '['      : a k xs
d k ('{':xs)                 = '{'      : o k xs
d k ('"':xs)                 = '"'      : s k xs
d k ('n':'u':'l':'l':xs)     = "null"  ++ k xs
d k ('t':'r':'u':'e':xs)     = "true"  ++ k xs
d k ('f':'a':'l':'s':'e':xs) = "false" ++ k xs
d k ('-':c:xs)
  | isDigit c = '-' : c : r k xs
d k (c:xs)
  | isSpace c = c : d k xs
  | isDigit c = c : r k xs
  | otherwise = error "illformed JSON document"

-- 'a'rray
a _ []       = error "unterminated array"
a k (']':xs) = ']' : k xs
a k xs       = d (a' k) xs

a' _ []       = error "unterminated array"
a' k (']':xs) = ']' : k xs
a' k (',':xs) = ',' : d (a' k) xs
a' k (c:xs)
   | isSpace c = c : a' k xs
   | otherwise = error "illformed JSON array"

-- 'o'bject
o _ []       = error "unterminated object"
o k ('}':xs) = '}' : k xs
o k ('"':xs) = '"' : s (char ':' (d (o' k))) xs
o k (c:xs)
  | isSpace c = c : o k xs
  | otherwise = error "illformed JSON object"

o' _ []       = error "unterminated object"
o' k ('}':xs) = '}' : k xs
o' k (',':xs) = ',' : char '"' (s (char ':' (d (o' k)))) xs
o' k (c:xs)
  | isSpace c = c : o' k xs
  | otherwise = error "illformed JSON object"

-- pasrse a string literal body
s _ []       = error "unterminated string"
s k ('"':xs) = '"' : k xs
s k ('\\':'u':x:y:z:t:xs) | all isHexDigit [x,y,z,t] = '\\' : 'u' : x : y : z : t : s k xs
-- lenient
s k ('\\':c:xs) = '\\' : c : s k xs
-- lenient
s k (c:xs) = c : s k xs

char :: Char -> (String -> String) -> String -> String
char c _ [] = error $ show c ++ " expected and end of input found"
char c k (x:xs)
  | c == x     = x : k xs
  | isSpace x  = x : char c k xs
  | otherwise  = error $ show c ++ " expected and " ++ show x ++ " found"

-- pasrse a integer literal body
i k [] = k []
i k (c : xs)
  | isDigit c = c : i k xs
  | otherwise = k (c : xs)

-- pasrse a rational literal body
r k [] = k []
r k ('.' : xs) = '.' : i k xs
r k ('e' : xs) = 'e' : e k xs
r k ('E' : xs) = 'E' : e k xs
r k (c : xs)
  | isDigit c = c : r k xs
  | otherwise = k (c : xs)

e k ('-' : xs) = '-' : i k xs
e k ('+' : xs) = '+' : i k xs
e k xs         = i k xs

main :: IO ()
main = interact json
