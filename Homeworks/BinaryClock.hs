import Control.Monad
import Data.Char (digitToInt)

binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0

intToTwoDigitString :: Int -> String
intToTwoDigitString n
  | n < 10 = '0' : show n
  | otherwise = show n

validateTime :: [String] -> String
validateTime [h1,h2,m1,m2,s1,s2]
  | h >= 24 || m >= 60 || s >= 60 = "ERROR"
  | otherwise = intToTwoDigitString h ++ ":" ++ intToTwoDigitString m ++ ":" ++ intToTwoDigitString s
  where
    h = binaryToDecimal h1 * 10 + binaryToDecimal h2
    m = binaryToDecimal m1 * 10 + binaryToDecimal m2
    s = binaryToDecimal s1 * 10 + binaryToDecimal s2

processTestCases :: Int -> IO ()
processTestCases 0 = return ()
processTestCases t = do
  replicateM_ 4 getLine
  input <- replicateM 4 getLine
  putStrLn $ validateTime input
  replicateM_ 1 getLine
  processTestCases (t - 1)

main :: IO ()
main = do
  t <- readLn
  processTestCases t
