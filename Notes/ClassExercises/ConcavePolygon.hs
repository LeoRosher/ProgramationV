import Data.List (elemIndex)
import Prelude

getAdjacents :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
getAdjacents _ [] = []
getAdjacents x list@(y:ys) =
    case getIndex x list of
        Just index ->
            if length ys == 0
                then [y]
                else [prev, x, next]
            where
                prev = if index == 0 then list !! (length list - 1) else list !! (index - 1)
                next = if index == length ys then y else list !! (index + 1)
        Nothing -> []

getIndex :: Eq a => a -> [a] -> Maybe Int
getIndex = elemIndex

getPointProduct :: (Int,Int) -> (Int,Int) -> Int
getPointProduct (a,b) (c,d) = a*c+b*d

mergeCoords :: (Int,Int) -> (Int,Int) -> (Int,Int)
mergeCoords (a,b) (c,d) = (a-b,c-d)

getNorma :: (Int, Int) -> Float
getNorma (a,b) = sqrt(fromIntegral (a^2) + fromIntegral (b^2))

getAngle :: Float -> Float -> Float
getAngle x y = acos 0/(x*y)

radianesToGrades :: Float -> Float
radianesToGrades radianes = radianes * (180 / pi)

verifyEachAngle :: [(Int,Int)] -> Int -> Bool
verifyEachAngle _ 4 = False
verifyEachAngle list index = let
                                adjacents = getAdjacents (list !! index) list
                                previusVector = mergeCoords (adjacents !! 0) (adjacents !! 1)
                                nextVector = mergeCoords (adjacents !! 1) (adjacents !! 2)
                                previusNorma = getNorma previusVector
                                nextNorma = getNorma nextVector
                                angle = getAngle previusNorma nextNorma
                                in
                                    if radianesToGrades angle > 180 then True else verifyEachAngle list (index + 1)



{-
Vectores: AB = (0 - 0, 1 - 0) = (0, 1), y AD = (1 - 0, 0 - 0) = (1, 0)
Normas de los vectores: |AB| = √(0^2 + 1^2) = 1, |AD| = √(1^2 + 0^2) = 1
Ángulo: arccos(0/(1*1)) = arccos(0) = 90 grados
-}