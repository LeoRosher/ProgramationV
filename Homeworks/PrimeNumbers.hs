factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primeNumbers :: Int -> [Int]
primeNumbers n = [y | y <- [2..n], isPrime y]