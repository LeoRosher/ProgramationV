-- Marzo 12 y 15
module Main where

import System.Random (randomRIO)
data Shape = Circle Float | Rect Float Float deriving(Show)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y


data MyMaybe a = NoHay | Existe a deriving(Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (m `div` n)

getValue :: Maybe Int -> Int
getValue Nothing = 0
getValue (Just a) = a


data Estudiante = Estudiante {
    nombres:: String,
    apellidos:: String
} deriving(Show)

estudiantes :: [Estudiante]
estudiantes =  [Estudiante "Pedro" "Velasquez",
                Estudiante "Valeria" "Perez"]

seleccionarEstudiante :: [Estudiante] -> IO Estudiante
seleccionarEstudiante estudiantes = do
                                    indice <- randomRIO (0, length estudiantes -1)
                                    return $ estudiantes !! indice

main :: IO ()
main = do
        estudianteSeleccionado <- seleccionarEstudiante estudiantes
        print estudianteSeleccionado