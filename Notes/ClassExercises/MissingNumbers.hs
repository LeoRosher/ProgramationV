missingNumbers :: [a] -> [a] -> [a]
missingNumbers _ [] = []
missingNumbers [] _ = []

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete x (y:ys) = if x == y then ys else delete x ys

deleteEquals :: [Int] -> [Int] -> [Int]
deleteEquals [] _ = []
deleteEquals (x:xs) y = delete x y ++ deleteEquals xs y