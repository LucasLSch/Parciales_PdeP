--                                  Escuelita de Thanos
--module EscuelitaDeThanos

import Text.Show.Functions

--Punto 1

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    planeta :: Planeta
} deriving (Show, Eq)

data Guantelete = Guantelete {
    material :: Material,
    gemas :: [Gema]
} deriving Show

type Universo = [Personaje]
type Habilidad = String
type Planeta = String
type Gema = Personaje -> Personaje
type Material = String

chasquidoDeunUniverso :: Universo -> Universo
chasquidoDeunUniverso unUniverso = take (mitadDelUniverso unUniverso) unUniverso

mitadDelUniverso :: Universo -> Int
mitadDelUniverso = flip div 2 . length

--Punto 2

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any (< 45) . map edad

energiaTotalDelUniverso :: Universo -> Int
energiaTotalDelUniverso = foldl (+) 0 . map energia . filter (tieneMasDeNHabilidades 1)

tieneMasDeNHabilidades :: Int -> Personaje -> Bool
tieneMasDeNHabilidades unNumero = (> unNumero) . cantHabilidades

cantHabilidades :: Personaje -> Int
cantHabilidades = length . habilidades

--Punto 3 

mente :: Int -> Gema
mente unValor = mapEnergia (restarEnergia unValor)

alma :: Habilidad -> Gema
alma unaHabilidad = mapEnergia (restarEnergia 10) . quitarHabilidad unaHabilidad

mapEnergia :: (Int -> Int) -> Personaje -> Personaje
mapEnergia unaFuncion unPersonaje = unPersonaje { energia = unaFuncion (energia unPersonaje)}

restarEnergia :: Int -> Int -> Int
restarEnergia unValor = max 0 . subtract unValor

quitarHabilidad :: Habilidad -> Personaje -> Personaje
quitarHabilidad unaHabilidad unPersonaje = unPersonaje { habilidades = filter (/= unaHabilidad) . habilidades $ unPersonaje}

espacio :: Planeta -> Gema
espacio unPlaneta = mapEnergia (restarEnergia 20) . mapPlaneta (enviarAPlaneta unPlaneta)

mapPlaneta :: (Planeta -> Planeta) -> Personaje -> Personaje
mapPlaneta unaFuncion unPersonaje = unPersonaje { planeta = unaFuncion (planeta unPersonaje)}

enviarAPlaneta :: Planeta -> Planeta -> Planeta
enviarAPlaneta = const

poder :: Gema
poder unPersonaje = mapEnergia debilitarPersonaje . dejarSinHabilidadesSi ((<= 2) . cantHabilidades) $ unPersonaje

dejarSinHabilidadesSi :: (Personaje -> Bool) -> Personaje -> Personaje
dejarSinHabilidadesSi unaCondicion unPersonaje | unaCondicion unPersonaje = unPersonaje { habilidades = []}
                                               | otherwise = id unPersonaje

debilitarPersonaje :: Int -> Int
debilitarPersonaje = const 0

tiempo :: Gema
tiempo = mapEdad reduceEdadALaMitad

mapEdad :: (Int -> Int) -> Personaje -> Personaje
mapEdad unaFuncion unPersonaje = unPersonaje { edad = unaFuncion (edad unPersonaje)}

reduceEdadALaMitad :: Int -> Int
reduceEdadALaMitad unValor | unValor >= 36 = div unValor 2
                           | otherwise = 18

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema . unaGema

--Punto 4

guanteDeGoma :: Guantelete
guanteDeGoma = Guantelete "Goma" [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]

--Punto 5

utilizar :: [Gema] -> Personaje -> Personaje
utilizar unasGemas unPersonaje = foldr ($) unPersonaje unasGemas

--Punto 6 

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje | (length . gemas $ unGuantelete) == 1 = head . gemas $ unGuantelete
                                         | (energia . (elementoNDe 0 . gemas $ unGuantelete) $ unPersonaje) > (energia . (elementoNDe 1 . gemas $ unGuantelete) $ unPersonaje) = gemaMasPoderosa (mapGemas (drop 1) unGuantelete) unPersonaje
                                         | otherwise = gemaMasPoderosa (mapGemas (((elementoNDe 0 . gemas $ unGuantelete) :) . drop 2) unGuantelete) unPersonaje


elementoNDe :: Int -> [a] -> a
elementoNDe n = head . drop n

mapGemas :: ([Gema] -> [Gema]) -> Guantelete -> Guantelete
mapGemas unaFuncion unGuantelete = unGuantelete { gemas = unaFuncion (gemas unGuantelete)}

--Punto 7

ciclomotor = 1
ciclomotorES = 1