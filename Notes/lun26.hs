module Example where
import Data.IntMap (insert)

    --Polimorfismo
    -- Declaracion (Num a, Eq a, Ord a, ...)

suma :: Num a => a-> a-> a
suma a b = a + b

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)


{-fac' :: Int -> Int
fac' 0 = 1
fac' n = [x | x <- [1..n],]-}

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

insertVal :: Ord a => a -> [a] -> [a]
insertVal x [] = [x]
insertVal x (y:ys)  | x <= y = x:y:ys
                    | otherwise = y : insertVal x ys

-- It uses Lazy evaluation concept

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myEven :: Int -> Bool
myEven n = mod n 2 == 0


-- Del libro programming in haskell hacer los q no tienen solucion

