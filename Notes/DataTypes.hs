module DataTypes where

natToNum :: Nat -> Int
natToNum Zero = 0
natToNum (Succ n) = 1 + natToNum n

numToNat :: Int -> Nat
numToNat 0 = Zero
numToNat n = Succ (numToNat (n-1))

suma:: Nat -> Nat -> Nat
suma n m = numToNat (natToNum n + natToNum m)

suma2:: Nat -> Nat -> Nat
suma2 Zero n = n
suma2 (Succ m) n = Succ (suma2 m n) 

data Nat = Zero 
         | Succ Nat
         deriving (Show)

data Tree a = Leaf a 
            | Node (Tree a) a (Tree a)
           deriving (Show)

arbol :: Tree Int
arbol = Node (Node (Leaf 3) 5 (Leaf 6)) 7 (Leaf 1)



-- Marzo 14
data List a = Vacio | Siguiente a (List a) deriving(Show)

myList :: List String
myList = Siguiente "" (Siguiente "" Vacio)

len :: List a -> Int
len Vacio = 0
len (Siguiente _ xs) = 1 + len xs
