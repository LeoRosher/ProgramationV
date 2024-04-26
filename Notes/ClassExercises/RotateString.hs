f :: String -> String
f "" = ""
f (x:xs) = xs ++ [x] ++ " " ++ f (xs)

