quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


main = do
    let unsortedList = [5, 2, 9, 1, 6, 3]
    putStrLn "Unsorted list:"
    print unsortedList
    putStrLn "Sorted list:"
    print (quicksort unsortedList)