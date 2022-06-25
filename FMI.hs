-- FMI 

module FMI where

import Text.Show.Functions

--Punto 1

data Pais = Pais {
    ipc :: Float,
    activaSectorPublico :: Float,
    activaSectorPrivado :: Float,
    recursosNaturales :: [Recurso],
    deuda :: Millones
} deriving (Show, Eq)

type Millones = Float
type Recurso = String

namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

argentina :: Pais
argentina = Pais 2 2 ["exceso de sexo"] 10000
--Punto 2

type Estrategia = Pais -> Pais

prestarNMillones :: Millones -> Estrategia
prestarNMillones n = mapDeuda (endeudarseEn n)

mapDeuda :: (Millones -> Millones) -> Pais -> Pais
mapDeuda unaFuncion unPais = unPais { deuda = unaFuncion . deuda $ unPais}

endeudarseEn :: Millones -> Millones -> Millones
endeudarseEn n = (+ (n*1.5))

reducirSectorPublico :: Float -> Estrategia
reducirSectorPublico cantidad = mapSectorPublico (subtract cantidad) . caidaDelIpc cantidad

mapSectorPublico :: (Float -> Float) -> Pais -> Pais
mapSectorPublico unaFuncion unPais = unPais { activaSectorPublico = unaFuncion . activaSectorPublico $ unPais}

caidaDelIpc :: Float -> Pais -> Pais
caidaDelIpc cantidad unPais 
    | cantidad > 100 = mapIpc (*0.8) unPais
    | otherwise = mapIpc (*0.85) unPais

mapIpc :: (Float -> Float) -> Pais -> Pais
mapIpc unaFuncion unPais = unPais { ipc = unaFuncion . ipc $ unPais}

regalarRecurso :: Recurso -> Estrategia
regalarRecurso unRecurso = mapDeuda (subtract 2) . mapRecursos (quitarRecurso unRecurso)

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso unRecurso = filter (/= unRecurso)

mapRecursos :: ([Recurso] -> [Recurso]) -> Pais -> Pais
mapRecursos unaFuncion unPais = unPais { recursosNaturales = unaFuncion . recursosNaturales $ unPais}

blindaje :: Estrategia
blindaje unPais = prestarNMillones ((*0.5) . pbi $ unPais) . reducirSectorPublico 500 $ unPais

pbi :: Pais -> Millones
pbi unPais = (ipc unPais * (activaSectorPublico unPais + activaSectorPrivado unPais)) / 1000000

--Punto 3

type Receta = [Estrategia]

recetaCompleja :: Receta
recetaCompleja = [prestarNMillones 200, regalarRecurso "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unaReceta unPais = foldr ($) unPais unaReceta

--Punto 4

zafan :: [Pais] -> [Pais]
zafan = filter (elem "Petroleo" . recursosNaturales)

deudaAFavor :: [Pais] -> Millones
deudaAFavor = sum . map deuda

--Punto 5

estanOrdenadas :: Pais -> [Receta] -> Bool
estanOrdenadas _ [] = True
estanOrdenadas _ [receta] = True
estanOrdenadas unPais (receta1:receta2:recetaS) = pbiResultante receta1 unPais < pbiResultante receta2 unPais && estanOrdenadas unPais (receta2:recetaS)

pbiResultante :: Receta -> Pais -> Millones
pbiResultante receta = pbi . aplicarReceta receta