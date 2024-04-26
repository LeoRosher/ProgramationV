updateList :: [Int] -> [Int]
updateList [] = []
updateList (x:xs) = if x < 0 then x*(-1):updateList xs else x:updateList xs

f = map abs