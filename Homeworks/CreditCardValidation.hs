{-
Divide un numero en sus respectivos digitos.

Devuelve una lista con los residuos de la division de 10,
empezando por el ultimo.
-}
divide :: Integer -> [Integer]
divide 0 = []
divide n = mod n 10 : divide(div n 10)

{-
Devuelve una lista con los digitos del numero.

Si el numero es menor o igual a 0, devuelve una lista vacia.
-}
toDigits :: Integer -> [Integer]
toDigits n
            | n < 0 = []
            | otherwise = reverse (divide n)

{-
Devuelve una lista invertida de los digitos de un numero.

Si el numero es menor o igual a 0, devuelve una lista vacia.
-}
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
            | n < 0 = []
            | otherwise = divide n


{-
Multiplica por dos los elemetos pares o impares de una lista,
dependiendo si la lista tiene un numero par o impar de elementos.
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther lista = if even (length lista) then zipWith (*) (cycle [2,1]) lista else zipWith (*) (cycle [1,2]) lista


{-
Convierte una lista a un Integer.
-}
toNumber :: [Integer] -> Integer
toNumber = read . concatMap show

{-
Suma todos los digitos de cada elemento de una lista.
-}
sumDigits :: [Integer] -> Integer
sumDigits = sum . toDigits . toNumber


{-
Valida si el numero ingresado cumple con los requisitos.
-}
validate :: Integer -> Bool
validate n = mod (sumDigits  . doubleEveryOther  . toDigits $ n) 10 == 0