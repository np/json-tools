import Data.Aeson
import Data.ByteString.Lazy.Char8 as L8
import System.Environment

main :: IO ()
main = L8.putStrLn . encode =<< getArgs
