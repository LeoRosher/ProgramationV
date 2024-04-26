divide :: Integer -> [Integer]
divide 0 = []
divide n = mod n 10 : divide(div n 10)

sumDigits :: [Integer] -> Integer
sumDigits = sum


superDigit :: [Integer] -> Integer
superDigit [x] = x
superDigit x = let
                digitSum = sumDigits x
                in
                superDigit (divide digitSum)

f :: Integer -> Integer -> Integer
f x y = superDigit (conc (divide x) y)

conc :: [Integer] -> Integer -> [Integer]
conc x y = x ++ conc x (y-1)