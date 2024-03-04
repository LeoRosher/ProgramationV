-- MARTES 27 Feb

-- Funciones de orden superior


-- map :: (a -> b) -> [a] -> [b]
-- (entre a y b pueden ser tipos de datos diferentes,y en este caso se devuelve el tipo del b)

-- Consejo: map es para transformar, no para iterar


mySum :: Num a => [a] -> a
mySum = foldr (+) 0

fac :: Num a => [a] -> a
fac = foldr (*) 1

