isPrime :: Int -> Bool
isPrime n = [x | x <- [1..n], mod n x == 0] == [1,n]

primeNumbers :: Int -> [Int]
primeNumbers n = [y | y <- [2..n], isPrime y]