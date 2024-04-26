module Expression where

data Exp = Lit Int
            | Add Exp Exp
            | Mul Exp Exp
            | Sub Exp Exp
            | Div Exp Exp
            | If Cond Exp Exp
            deriving Eq

data Cond = Eq Exp Exp
            |Lt Exp Exp
            |Gt Exp Exp
            |Nt Exp Exp
        deriving Eq

instance Show Exp where
    show (Lit n) = show n
    show (Add a b) = par(show a ++ " + " ++ show b)
    show (Mul a b) = par(show a ++ " * " ++ show b)
    show (Sub a b) = par(show a ++ " - " ++ show b)
    show (Div a b) = par(show a ++ " / " ++ show b)
    show (If c a b) = "If " ++ show c ++ " then " ++ show a ++ " else " ++ show b

instance Show Cond where
    show (Eq a b) = show a ++ " == " ++ show b
    show (Lt a b) = show a ++ " < " ++ show b
    show (Gt a b) = show a ++ " > " ++ show b
    show (Nt a b) = show a ++ " != " ++ show b

par :: String -> String
par x = "(" ++ x ++ ")"


evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add a b ) = evalExp a + evalExp b
evalExp (Mul a b ) = evalExp a * evalExp b
evalExp (Sub a b ) = evalExp a - evalExp b
evalExp (Div a b ) = div (evalExp a) (evalExp b)
evalExp (If c a b) = if evalCond c then evalExp a else evalExp b


evalCond :: Cond -> Bool
evalCond (Eq a b) = evalExp a == evalExp b
evalCond (Lt a b) = evalExp a < evalExp b
evalCond (Gt a b) = evalExp a > evalExp b
evalCond (Nt a b) = evalExp a /= evalExp b

e0 = Add (Lit 1) (Mul (Lit 2) (Lit 3))
e1 = Mul (Add (Lit 1) (Lit 2)) (Lit 3)
e2 = Add e0 (Mul (Lit 3) e1)
e3 = Add (Lit 3) (Sub (Lit 5) (Lit 3))
c0 = Eq (Lit 2) (Lit 3)
e4 = If c0 (Lit 1) (Lit 2)
