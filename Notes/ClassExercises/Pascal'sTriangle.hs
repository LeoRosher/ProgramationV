pascal :: Int -> [Int] -> [[Int]]
pascal 0 _ = []
pascal x y = let 
                next = getNext (getList y)
                in
                next : pascal (x-1) next


getList :: [Int] -> [Int]
getList [] = []
getList [x] = [x]
getList (x:y:xs) = (x+y) : getList (y : xs)

getNext :: [Int] -> [Int]
getNext x = 1 : x

myprint xs = mapM_ putStrLn $ map (\x -> foldr (++) "" (map show x)) xs



pascal' :: Int -> [Int] -> [[Int]]
pascal' 0 _ = []
pascal' x y = getNext' y : fmap (pascal' (x-1)) (getNext' y)

getList' :: [Int] -> [Int]
getList' [] = []
getList' [x] = [x]
getList' (x:y:xs) = (x+y) : getList' (y : xs)

getNext' :: [Int] -> [Int]
getNext' x = 1 : x