module CrystalGems where

import Text.Show.Functions

data Aspecto = Aspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> Situacion -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (Aspecto tipo 0)

reemplazarAspecto :: Aspecto -> Situacion -> Situacion
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

--Punto 1

mapTipoDeAspecto :: (String -> String) -> Aspecto -> Aspecto
mapTipoDeAspecto unaFuncion unAspecto = unAspecto { tipoDeAspecto = unaFuncion . tipoDeAspecto $ unAspecto}

--es modificarAspecto
mapGrado :: (Float -> Float) -> Aspecto -> Aspecto
mapGrado unaFuncion unAspecto = unAspecto { grado = unaFuncion . grado $ unAspecto}

esMejorSituacionQue :: Situacion -> Situacion -> Bool
esMejorSituacionQue [] [] = True
esMejorSituacionQue (x:xS) lista = mejorAspecto x (buscarAspecto x lista) && esMejorSituacionQue xS (restoDeAspectos)
    where restoDeAspectos = filter (not . mismoAspecto x) lista

modificarSituacion :: (Float -> Float) -> String -> Situacion -> Situacion
modificarSituacion unaFuncion tipoDeAspecto situacion = reemplazarAspecto aspectoModificado situacion--map (modificarSiCorresponde unaFuncion tipoDeAspecto)
    where aspectoModificado = mapGrado unaFuncion . buscarAspectoDeTipo tipoDeAspecto $ situacion
--modificarSiCorresponde :: (Float -> Float) -> String -> Aspecto -> Aspecto
--modificarSiCorresponde unaFuncion tipoDeAspecto aspectoEncontrado
  --  | mismoAspectoDeTipo tipoDeAspecto aspectoEncontrado = mapGrado unaFuncion aspectoEncontrado
  --  | otherwise = aspectoEncontrado

--Punto 2

data Gema = Gema {
    nombre :: String,
    fuerza :: Int,
    personalidad :: Personalidad
} deriving Show

type Personalidad = Situacion -> Situacion

vidente :: Personalidad
vidente = mapIncertidumbre (/2) . mapTension (subtract 10)

relajada :: Float -> Personalidad
relajada nivelDeRelajamiento = mapPeligro (+ nivelDeRelajamiento) . mapTension (subtract 30)

mapTension :: (Float -> Float) -> Personalidad
mapTension unaFuncion = modificarSituacion (max 0 . unaFuncion) "Tension"

mapIncertidumbre :: (Float -> Float) -> Personalidad
mapIncertidumbre unaFuncion = modificarSituacion (max 0 . unaFuncion) "Incertidumbre"

mapPeligro :: (Float -> Float) -> Personalidad
mapPeligro unaFuncion = modificarSituacion (max 0 . unaFuncion) "Peligro"

garnet :: Gema
garnet = Gema "Garnet" 25 vidente

amatista :: Gema
amatista = Gema "Amatista" 16 (relajada 25)

--Punto 3

leGanaA :: Gema -> Gema -> Situacion -> Bool
leGanaA gema1 gema2 situacion = gema1 `esMasFuerteQue` gema2 && (gema1 `esMejorEnEsta` situacion $ gema2)

esMasFuerteQue :: Gema -> Gema -> Bool
esMasFuerteQue gema1 gema2 = fuerza gema1 >= fuerza gema2

esMejorEnEsta :: Gema  -> Situacion -> Gema -> Bool 
esMejorEnEsta gema1 situacion gema2 = (personalidad gema1 situacion) `esMejorSituacionQue` (personalidad gema2 situacion)

--Punto 4

fuuuusion :: Situacion -> Gema -> Gema -> Gema
fuuuusion situacion gema1 gema2 = Gema { 
    nombre = fusionarNombres gema1 gema2,
    fuerza = fusionarFuerza situacion gema1 gema2,
    personalidad = fusionarPersonalidad gema1 gema2
}

fusionarNombres :: Gema -> Gema -> String
fusionarNombres gema1 gema2 
    | gema1 `seLlamaIgualQue` gema2 = nombre gema1
    | otherwise = nombre gema1 ++ nombre gema2

seLlamaIgualQue :: Gema -> Gema -> Bool
seLlamaIgualQue gema1 gema2 = nombre gema1 == nombre gema2

fusionarFuerza :: Situacion -> Gema -> Gema -> Int
fusionarFuerza situacion gema1 gema2
    | sonCompatibles situacion gema1 gema2 = (fuerza gema1 + fuerza gema2) * 10
    | otherwise = (*7) . fuerza $ (gemaDominante situacion gema1 gema2)

gemaDominante :: Situacion -> Gema -> Gema -> Gema
gemaDominante situacion gema1 gema2 
    | gema1 `leGanaA` gema2 $ situacion = gema1
    | otherwise = gema2

sonCompatibles :: Situacion -> Gema -> Gema -> Bool
sonCompatibles situacion gema1 gema2 = (fusionarPersonalidad gema1 gema2 situacion) `esMejorSituacionQue` (personalidad gema1 situacion) 
    && (fusionarPersonalidad gema1 gema2 situacion) `esMejorSituacionQue` (personalidad gema2 situacion)

fusionarPersonalidad :: Gema -> Gema -> Personalidad
fusionarPersonalidad gema1 gema2 = personalidad gema1 . personalidad gema2 . restar10ATodosLosAspectosDeLaSituacion
    where restar10ATodosLosAspectosDeLaSituacion = mapPeligro (subtract 10) . mapTension (subtract 10) . mapIncertidumbre (subtract 10)

fuuuusionGrupalL :: Situacion -> [Gema] -> Gema
fuuuusionGrupalL situacion = foldl1 (fuuuusion situacion)

fuuuusionGrupalR :: Situacion -> [Gema] -> Gema
fuuuusionGrupalR situacion = foldr1 (fuuuusion situacion)

foo :: Eq e => x -> (x -> e) -> (a -> [e]) -> a -> Bool
foo x y z = any (== y x) . z


{-sonTodosIguales = True
sonTodosIguales num = mod num 10 == mod (div num 10) 10 && sonTodosIguales (div num 10)-}