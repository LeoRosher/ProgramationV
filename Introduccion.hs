module Introduccion where
import Data.Char(ord, chr)

suma :: Int -> Int -> Int
suma a b = a + b

suma' :: Float -> Float -> Float
suma' a b = a + b

lista :: [Int]
lista = [1,2,3,4]

lista' :: [Int]
lista' = [1..10]

unChar :: Char
unChar = 'A'

tupla :: Char -> (Char, Int)
tupla x = (x , ord x)

sumLista :: [Int] -> Int
sumLista = sum


-- Lambda Function
add = \x y -> (x + y)



-- Jueves 22 Feb


myFuctions :: [Int -> Int -> Int]
myFuctions = [(+), (-), (*), div, suma]

getFunction :: Char -> (Int -> Int -> Int)
getFunction e 
        | e == '+' = myFuctions !! 0
        | e == '-' = myFuctions !! 1
        | e == '*' = myFuctions !! 2
        | e == '/' = myFuctions !! 3
        | otherwise = myFuctions !! 4

--myExp :: Char -> (Int -> Int -> Int)
--myExp e = getFunction e

-- Q es curried function y ejemplos

-- [x^2 | x <- [1..5]]

-- [x + y | x <- [1..6], y <- [7..9]]


qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort menor ++ [x] ++ qsort mayor
                where
                    menor = [a | a <- xs, a < x]
                    mayor = [b | b <- xs, b >= x]


{--esPrimo :: Int -> Bool
esPrimo n 
    | n <= 1 = False
    | --}