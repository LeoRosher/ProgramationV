module Class4Marz where

-- Semantica para q las funciones sean mas entendibles
type DiaLiteral = String
type DiaNumeral = Int
type MesNumeral = Int
type AnioNumeral = Int
type DiasDelMes = Int

-- case of, otra forma del "switch"
qDiaEs :: DiaNumeral -> DiaLiteral
qDiaEs x = case x of 
                1 -> "Domingo"
                2 -> "Lunes"
                3 -> "Martes"
                4 -> "Miercoles"
                5 -> "Jueves"
                6 -> "Vieres"
                7 -> "Sábado"
                _ -> "No se"

qDiaEs' :: DiaLiteral -> DiaNumeral
qDiaEs' x = case x of
                "Domingo" -> 1
                "Lunes" -> 2
                "Martes" -> 3
                "Miercoles" -> 4
                "Jueves" -> 5
                "Viernes" -> 6
                "Sabado" -> 7
                _ -> -1


-- uso de let in, para "variables"
calcularDiaMes :: MesNumeral -> AnioNumeral -> DiasDelMes
calcularDiaMes m a = let
                        esBisiesto = anioBisiesto a
                    in
                        case m of
                            1 -> 31
                            2 -> if esBisiesto then 29 else 28
                            3 -> 31
                            4 -> 30
                            5 -> 31
                            6 -> 30
                            7 -> 31
                            8 -> 31
                            9 -> 30
                            10 -> 31
                            11 -> 30
                            12 -> 31
                            _ -> -1
                    where
                        anioBisiesto :: AnioNumeral -> Bool
                        anioBisiesto a = (mod a 4 == 0) && (mod a 100 /= 0 || mod a 400 == 0)



anioBicisto' :: AnioNumeral -> Bool
anioBicisto' a  = let
                    p = mod a 4 == 0
                    q = mod a 100 == 0
                    r = mod a 400 == 0
                    in p && (not q || r)