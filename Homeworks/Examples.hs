
--suma
add :: Int -> Int -> Int
add x y = x + y

--multiplicacion
multiply :: Int -> Int -> Int
multiply x y = x * y

--potencia
power :: Int -> Int -> Int
power x y = x ^ y

--concatenacion
concatenate :: String -> String -> String
concatenate str1 str2 = str1 ++ str2

--condicional
applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
applyIf condition f x = if condition x then Just (f x) else Nothing