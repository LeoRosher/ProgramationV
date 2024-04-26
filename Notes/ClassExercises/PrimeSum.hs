primeSum :: Int -> Int -> String
primeSum x y = if analize x y then "Yes" else "No"

analize :: Int -> Int -> Bool
analize 1 _ = False
analize x y = findNumbers x (erastotenes x) == y

findNumbers :: Int -> [Int] -> Int
findNumbers _ [] = 0
findNumbers x (y:ys) = if mod x y == 0 then y else findNumbers x ys




erastotenes ::  Int -> [Int]
erastotenes n = erastotenes' [2 .. n] 0

erastotenes' :: [Int] -> Int -> [Int]
erastotenes' lista n
    | n == length lista-1 = lista
    | otherwise=erastotenes' [x|x <-lista,(x `mod` lista!!n)/=0||x==lista!!n] (n+1)

