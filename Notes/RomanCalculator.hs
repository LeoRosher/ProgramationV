-- My own implementation

module RomanCalculator where

type Roman = String

roman::[(Char,Int)]
roman = [('I',1),('V',5),('X',10),('L',50),('C',100),('D',500),('M',1000)]
--roman = [("I",1),("V",5),("X",10),("L",50),("C",100),("D",500),("M",10000)]

{-romanToInt :: Roman -> Int
romanToInt "" = 0
romanToInt (x : xs) = if current >= next then current + next else next - current
                        where
                            current = search x roman
                            next = romanToInt xs     -}      

romanToInt :: Roman -> Int
romanToInt "" = 0
romanToInt [x] = search x roman
romanToInt (x:y:xs) = if current >= next then current + romanToInt (y:xs) else next - current + romanToInt xs
                        where
                            current = search x roman
                            next = search y roman 

search :: Char -> [(Char, Int)] -> Int
search x [] = 0
search x ((a,b):resto)
    | x == a = b
    | otherwise = search x resto

intToRoman :: Int -> Roman
intToRoman 0 = ""
intToRoman x = let
                n = toList x
                size = length n
                in
                    ""

toList :: Int -> [Int]
toList 0 = []
toList x = toList (x `div` 10) ++ [x `mod` 10]

takeNumber :: Int -> Int -> Int
takeNumber x y = let
                    exp = y-1
                    in
                    x*10^exp

convert :: Int -> [(Char,Int)] -> Char
convert x (y:ys) = if x <= snd y then fst y else convert x ys

