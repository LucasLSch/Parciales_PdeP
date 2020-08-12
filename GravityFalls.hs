--Gravity falls

import Text.Show.Functions

------------------------------------ Parte 1 ---------------------------------

--Punto 1

data Persona = Persona {
    edad :: Int,
    items :: [Item],
    experiencia :: Int
} deriving (Show, Eq)

data Criatura = Criatura {
    peligrosidad :: Int,
    condiciones :: [Condicion]
} deriving Show

type Condicion = Persona -> Bool
type Item = String

siempreDetras :: Criatura
siempreDetras = Criatura 0 []

gnomos :: Int -> Criatura
gnomos 1 = Criatura 0 [tieneItem "soplador de hojas"]
gnomos unaCantidad = Criatura (2^unaCantidad) [tieneItem "soplador de hojas"]

fantasma :: Int -> [Condicion] -> Criatura
fantasma categoria condiciones = Criatura (20*categoria) condiciones

tieneItem :: Item -> Persona -> Bool
tieneItem unItem = elem unItem . items

aumentarExperiencia :: Int -> Persona -> Persona
aumentarExperiencia cantidad unaPersona = unaPersona { experiencia = (+ cantidad) . experiencia $ unaPersona }

--Punto 2

enfrentarCriatura :: Criatura -> Persona -> Persona
enfrentarCriatura unaCriatura unaPersona 
    | cumplirCondiciones unaPersona (condiciones unaCriatura) = aumentarExperiencia (peligrosidad unaCriatura) unaPersona
    | otherwise = escapar unaPersona

escapar :: Persona -> Persona
escapar = aumentarExperiencia 1

cumplirCondiciones :: Persona -> [Condicion] -> Bool
cumplirCondiciones _ [] = True
cumplirCondiciones unaPersona [condicion] = condicion unaPersona
cumplirCondiciones unaPersona (condicion1:condicion2:condiciones) = condicion1 unaPersona && condicion2 unaPersona && cumplirCondiciones unaPersona condiciones

--Punto 3

experienciaGanada :: [Criatura] -> Persona -> Int
experienciaGanada criaturas unaPersona = experiencia (foldr (enfrentarCriatura) unaPersona criaturas) - experiencia unaPersona


----------------------------------------- Parte 2 ----------------------------------------------------------------------------------------------------------------------

--Punto 1

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ _ [] = []
zipWithIf operacion condicion (a1:as) (b1:bs) 
    | condicion b1 = (operacion a1 b1) : zipWithIf operacion condicion as bs
    | otherwise = b1 : zipWithIf operacion condicion (a1:as) bs

--Punto 2

abecedarioDesde :: Char -> [Char]
abecedarioDesde unaLetra = [unaLetra..'z'] ++ take (letrasSobrantes unaLetra) ['a'..'z']

letrasSobrantes :: Char -> Int
letrasSobrantes unaLetra = length ['a'..'z'] - length [unaLetra..'z']

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraBase letraObjetivo = head . drop (posicionLetra (abecedarioDesde letraBase) letraObjetivo) $ ['a'..'z']

posicionLetra :: [Char] -> Char -> Int
posicionLetra (letra:letraS) letraObjetivo 
    | letraObjetivo == letra = 0
    | otherwise = 1 + (posicionLetra letraS letraObjetivo)

cesar :: Char -> String -> String
cesar letraBase textoEncriptado = descifrarSegun repeat letraBase textoEncriptado

perteneceAAbecedario :: Char -> Bool
perteneceAAbecedario unCaracter = elem unCaracter ['a'..'z']

posiblesDesencriptaciones :: String -> [String]
posiblesDesencriptaciones textoEncriptado = foldl (aplicarCesar textoEncriptado) [] ['a'..'z']

aplicarCesar :: String -> [String] -> Char -> [String]
aplicarCesar textoEncriptado textosDesencriptados letraBase = cesar letraBase textoEncriptado : textosDesencriptados

--Punto 3

vingenere :: String -> String -> String
vingenere palabraBase textoEncriptado = descifrarSegun cycle palabraBase textoEncriptado

descifrarSegun :: (a -> [Char]) -> a -> String -> String
descifrarSegun funcion valorBase = zipWithIf desencriptarLetra perteneceAAbecedario (funcion valorBase)