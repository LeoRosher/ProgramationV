import Data.Typeable
import Data.Char

getType :: Typeable a => a -> TypeRep
getType = typeOf

--Unidad 3

--3.
second xs = head (tail xs) -- Any type
swap (x,y) = (y,x) -- Any type
pair x y = (x,y) -- Any type
double x = x*2 -- Int, Integer, Float, Double
palindrome xs = reverse xs == xs -- Any type
twice f x = f (f x) -- Any Type


-- 4. Done
-- 5. Eq solo compara resultados, no se centra en el proceso.




-- Unidad 4

--5
demo :: Bool -> Bool -> Bool
demo x y = x && y

--6
demo' :: Bool -> Bool -> Bool
demo' x y = if x then y else False

--7
mult = \x y z -> x*y*z


-- Unidad 5
--6
isPerfect :: Int -> Bool
isPerfect n = n == sum [x | x <- [1..n-1], mod n x == 0 ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]

--7 
-- concat [[(x, y) | y <- [3,4]] | x <- [1,2]]

--8


--9
scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

--10
shift :: Int -> Char -> Char
shift n c
  | isUpper c = chr ((ord c - ord 'A' + n) `mod` 26 + ord 'A')
  | isLower c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')
  | otherwise = c

caesar :: Int -> String -> String
caesar n message = map (shift n) message


-- Unidad 6

--5
--a.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--b.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

--c.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

--d.
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
--(_:xs) !! n = xs !! (n-1)

--e.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | x == y    = True
    | otherwise = elem' y xs



-- Unidad 7

