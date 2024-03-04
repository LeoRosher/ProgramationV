histograma :: [Int] -> [String]
histograma lista = [x | x <- []]

counter :: [Int] -> [Int]
counter [] = []
--counter (x:xs) = increase x y

--replicate 10 0
increase :: Int -> [Int] -> [Int]
increase a lista =
    if a < 0 || a >= length lista then lista
        else antes ++ [lista!!a+1] ++despues
        where 
            antes = take a lista
            despues = drop (a+1) lista

reemplazarEnIndice :: Int -> a -> [a] -> [a]
reemplazarEnIndice indice nuevoElemento lista =
    let (antes, _:despues) = splitAt indice lista
    in antes ++ [nuevoElemento] ++ despues


insertarEnIndice :: Int -> a -> [a] -> [a]
insertarEnIndice indice elemento lista =
    let (antes, despues) = splitAt indice lista
    in antes ++ [elemento] ++ despues