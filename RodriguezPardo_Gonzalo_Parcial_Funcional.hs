module HeroesDeLeyenda where

import Text.Show.Functions

data Heroe = Heroe {
 nombreHeroe :: String,
 epiteto :: String,
 reconocimiento :: Int,
 artefactos :: [Artefacto],
 tareasRealizadas :: [Tarea]
}deriving(Show)

data Artefacto = Artefacto {
 nombreArtefacto :: String,
 rareza :: Int
}deriving(Show)
-- ------------------------------------------------------------------------------------------------------------
type MapHeroe = Heroe -> Heroe

setEpiteto :: String -> MapHeroe
setEpiteto nuevoEpiteto heroe = heroe {epiteto = nuevoEpiteto}

mapReconocimiento :: (Int -> Int)-> MapHeroe
mapReconocimiento funcion heroe = heroe {reconocimiento = funcion.reconocimiento $ heroe}

agregarReconocimiento :: Int -> MapHeroe
agregarReconocimiento valor heroe = mapReconocimiento (+ valor) heroe

mapArtefactos :: ([Artefacto]-> [Artefacto])-> MapHeroe
mapArtefactos funcion heroe = heroe {artefactos = funcion.artefactos $ heroe}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe = mapArtefactos (++[artefacto]) heroe

mapTareas :: ([Tarea]-> [Tarea])-> MapHeroe
mapTareas funcion heroe = heroe {tareasRealizadas = funcion.tareasRealizadas $ heroe}

anotarTarea :: Tarea -> MapHeroe
anotarTarea tarea heroe = mapTareas (++ [tarea]) heroe

mapRareza :: (Int -> Int) -> Artefacto -> Artefacto
mapRareza funcion artefacto = artefacto {rareza = funcion . rareza $ artefacto}
-- ------------------------------------------------------------------------------------------------------------
paseALaHistoria :: Heroe -> Heroe
paseALaHistoria heroe
 | reconocimientoEs (>1000) heroe = setEpiteto "El mitico" heroe
 | reconocimientoEs (>=500) heroe = agregarArtefacto lanzaDelOlimpo . setEpiteto "El magnifico" $ heroe
 | reconocimientoEs (>100) heroe = agregarArtefacto xiphos . setEpiteto "Hoplita" $ heroe
 | otherwise = heroe

reconocimientoEs :: (Int -> Bool) -> Heroe -> Bool
reconocimientoEs condicion heroe = condicion (reconocimiento heroe)

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza Del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50
-- ------------------------------------------------------------------------------------------------------------
type Tarea = Heroe -> Heroe

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto = agregarArtefacto artefacto . agregarReconocimiento (rareza artefacto)

escalarElOlimpo :: Tarea
escalarElOlimpo = agregarArtefacto relampagoDeZeus . filtrarArtefactos ((>1000).rareza) . triplicarRarezaArtefactos . agregarReconocimiento 500

triplicarRarezaArtefactos :: MapHeroe
triplicarRarezaArtefactos = mapArtefactos (map (mapRareza (*3)))

filtrarArtefactos :: (Artefacto -> Bool) -> Heroe -> Heroe
filtrarArtefactos condicion = mapArtefactos (filter condicion)

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "Relampago de Zeus" 500

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidad = setEpiteto ("Gros" ++ (replicate cantidad 'o'))

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe
 | debilidad bestia $ heroe = setEpiteto ("Asesino de " ++ (nombre bestia)) heroe
 | otherwise = setEpiteto "El cobarde" . perderPrimerArtefacto $ heroe
 where perderPrimerArtefacto = mapArtefactos tail
-- ------------------------------------------------------------------------------------------------------------
heracles :: Heroe
heracles = Heroe "Heracles" "Guardian del Olimpo" 700 [pistola, relampagoDeZeus] [matarAlLeonDeNemea] 

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

data Bestia = Bestia {
 nombre :: String,
 debilidad :: Heroe -> Bool
}deriving(Show)

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia leonDeNemea

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon De Nemea" debilidadLeon
 where debilidadLeon = (>20).length . epiteto

hacerUnaTarea :: Heroe -> Tarea -> Heroe
hacerUnaTarea heroe tarea = anotarTarea tarea . tarea $ heroe

presuman :: Heroe -> Heroe -> (Heroe,Heroe)
presuman heroe1 heroe2
 | diferente reconocimiento  heroe1 heroe2 = entuplarPor reconocimiento heroe1 heroe2
 | diferente sumatoriaDeRarezas heroe1 heroe2 = entuplarPor sumatoriaDeRarezas heroe1 heroe2
 | otherwise = intercambiarTareasYPresumir heroe1 heroe2
  where sumatoriaDeRarezas = sum . map rareza . artefactos

diferente :: (Eq a) => (Heroe -> a) -> Heroe -> Heroe -> Bool
diferente caracteristica heroe1 heroe2 = caracteristica heroe1 /= caracteristica heroe2

entuplarPor :: (Ord a) => (Heroe -> a) -> Heroe -> Heroe -> (Heroe,Heroe)
entuplarPor caracteristica heroe1 heroe2
 | caracteristica heroe1 > caracteristica heroe2 = (heroe1,heroe2)
 | otherwise = (heroe2, heroe1)

intercambiarTareasYPresumir :: Heroe -> Heroe -> (Heroe,Heroe)
intercambiarTareasYPresumir heroe1 heroe2 = presuman (intercambiarTareas heroe1 heroe2) (intercambiarTareas heroe2 heroe1)
 where intercambiarTareas unHeroe otroHeroe = labor (tareasRealizadas otroHeroe) unHeroe
-- ------------------------------------------------------------------------------------------------------------
{- Ejercicio 8: Al pasarle como parametros a la funcion Presuman dos heroes de mismo
reconocimiento y sin artefactos ni tareas, la funcion se quedaría evaluando de forma recursiva
y nunca llegaría a un resultado. Esto pasa porque siempre se terminaría en la sitaucion de realizar las tareas del otro y volver a presumir, y al no tener tareas, no cambiaría nada en
ninguno de los heroes, por lo que se genera un bucle infinito.
-}
-- ------------------------------------------------------------------------------------------------------------
labor :: [Tarea] -> Tarea
labor tareas heroe = foldl (hacerUnaTarea) heroe tareas
-- ------------------------------------------------------------------------------------------------------------
{- Ejercicio 10: No se podrá conocer el estado final ya que la funcion foldl
recorre toda la lista antes de arrojar un resultado, por lo que se colgaría 
generando la lista (tiene el mismo comportamiento que si quiciesemos hacer
SUM a una lista infinita)
-}