--                              Parcial de Funcional

module Parcial_Funcional where

import Text.Show.Functions


--Prototipo de funcion Map
{-mapAlgo :: (a -> a) -> DataStruct -> DataStruct
mapAlgo unaFuncion unObjeto = unObjeto { 
    algo = unaFuncion . algo $ unObjeto
}-}

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--Punto 1

data Heroe = Heroe { 
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
} deriving Show

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
} deriving (Show, Eq)

-- Punto 2

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe 
    | suReconocimientoEs (> 1000) unHeroe = mapEpiteto (ganaEpiteto "El mitico") unHeroe
    | suReconocimientoEs (> 500) unHeroe = mapEpiteto (ganaEpiteto "El magnifico") . mapArtefactos (ganaArtefacto lanzaDelOlimpo) $ unHeroe
    | suReconocimientoEs (> 100) unHeroe = mapEpiteto (ganaEpiteto "Hoplita") . mapArtefactos (ganaArtefacto xiphos) $ unHeroe
    | otherwise = id unHeroe

suReconocimientoEs :: (Int -> Bool) -> Heroe -> Bool
suReconocimientoEs unaCondicion unHeroe = unaCondicion (reconocimiento unHeroe)

ganaEpiteto :: String -> String -> String
ganaEpiteto = const

ganaArtefacto :: Artefacto -> [Artefacto] -> [Artefacto]
ganaArtefacto nuevoArtefacto artefactos = nuevoArtefacto : artefactos

mapEpiteto :: (String -> String) -> Heroe -> Heroe
mapEpiteto unaFuncion unHeroe = unHeroe { 
    epiteto = unaFuncion . epiteto $ unHeroe
}

mapArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefactos unaFuncion unHeroe = unHeroe { 
    artefactos = unaFuncion . artefactos $ unHeroe
}

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

--Punto 3

type Tarea = Heroe -> Heroe

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto = mapReconocimiento (sumarRareza unArtefacto) . mapArtefactos (ganaArtefacto unArtefacto)

sumarRareza :: Artefacto -> Int -> Int
sumarRareza unArtefacto = (+ (rareza unArtefacto))

mapReconocimiento :: (Int -> Int) -> Heroe -> Heroe
mapReconocimiento unaFuncion unHeroe = unHeroe { 
    reconocimiento = unaFuncion . reconocimiento $ unHeroe
}

escalarElOlimpo :: Tarea
escalarElOlimpo = mapReconocimiento (+ 500) . mapArtefactos (ganaArtefacto relampagoDeZeus) . mapArtefactos (desecharRarezaQue (< 1000)) . mapArtefactos (multiplicaRareza 3)

multiplicaRareza :: Int -> [Artefacto] -> [Artefacto]
multiplicaRareza unNumero = map (mapRareza (*unNumero))

mapRareza :: (Int -> Int) -> Artefacto -> Artefacto
mapRareza unaFuncion unArtefacto = unArtefacto { 
    rareza = unaFuncion . rareza $ unArtefacto
}

desecharRarezaQue :: (Int -> Bool) -> [Artefacto] -> [Artefacto]
desecharRarezaQue unaCondicion = filter (not . unaCondicion . rareza)

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "Relampago de Zeus" 500

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras = mapEpiteto (ganaEpiteto grosoConNOs)
    where grosoConNOs = "Gros" ++ (take cuadras (repeat 'o'))

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Heroe -> Bool
} deriving Show

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe 
    | (debilidad unaBestia) unHeroe = mapEpiteto (ganaEpiteto . asesinoDe $ unaBestia) unHeroe
    | otherwise = mapEpiteto (ganaEpiteto "El cobarde") . mapArtefactos (pierdeNArtefactos 1) $ unHeroe

asesinoDe :: Bestia -> String
asesinoDe unaBestia = "El asesino de " ++ (nombreBestia unaBestia)

pierdeNArtefactos :: Int -> [Artefacto] -> [Artefacto]
pierdeNArtefactos n artefactos = drop n artefactos

--Punto 4

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

heracles :: Heroe
heracles = Heroe "Guardian del Olimpo" 700 [pistola, relampagoDeZeus]  [matarLeonDeNemea]

heracles' :: Heroe
heracles' = Heroe "Grosoooooooooooooooooo" 700 [pistola, relampagoDeZeus] [ayudarACruzarLaCalle 50, escalarElOlimpo]

--Punto 5

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" (nombreConNOMasChar 20)

nombreConNOMasChar :: Int -> Heroe -> Bool
nombreConNOMasChar cantidad unHeroe = (cantidadLetras . epiteto $ unHeroe) >= cantidad

cantidadLetras :: String -> Int
cantidadLetras = length

matarLeonDeNemea :: Tarea
matarLeonDeNemea = matarUnaBestia leonDeNemea

--Punto 6

realizarTarea :: Tarea -> Heroe -> Heroe
realizarTarea unaTarea = mapTareas (agregarTareaALaLista unaTarea) . unaTarea

agregarTareaALaLista :: Tarea -> [Tarea] -> [Tarea]
agregarTareaALaLista unaTarea tareas = unaTarea : tareas

mapTareas :: ([Tarea] -> [Tarea]) -> Heroe -> Heroe
mapTareas unaFuncion unHeroe = unHeroe { 
    tareas = unaFuncion . tareas $ unHeroe
}

--Punto 7

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir unHeroe otroHeroe 
    | reconocimientoDistinto unHeroe otroHeroe = mejor_peorSegun reconocimiento unHeroe otroHeroe
    | rarezaArtefactosDistinta unHeroe otroHeroe = mejor_peorSegun rarezaTotalDeArtefactos unHeroe otroHeroe
    | otherwise = presumir (realizarTareaDeOtro unHeroe otroHeroe) (realizarTareaDeOtro otroHeroe unHeroe)

reconocimientoDistinto :: Heroe -> Heroe -> Bool
reconocimientoDistinto unHeroe otroHeroe = distintoSegun reconocimiento unHeroe otroHeroe

rarezaArtefactosDistinta :: Heroe -> Heroe -> Bool
rarezaArtefactosDistinta unHeroe otroHeroe = distintoSegun rarezaTotalDeArtefactos unHeroe otroHeroe

distintoSegun :: Eq a => (b -> a) -> b -> b -> Bool
distintoSegun unaFuncion algo otraCosa = unaFuncion algo /= unaFuncion otraCosa

mejor_peorSegun :: Ord a => (b -> a) -> b -> b -> (b, b)
mejor_peorSegun unaFuncion algo otraCosa 
    | unaFuncion algo > unaFuncion otraCosa = (algo, otraCosa)
    | otherwise = (otraCosa, algo)

rarezaTotalDeArtefactos :: Heroe -> Int
rarezaTotalDeArtefactos = sum . map rareza . artefactos

realizarTareaDeOtro :: Heroe -> Heroe -> Heroe
realizarTareaDeOtro unHeroe otroHeroe = realizarLabor unHeroe (tareas otroHeroe)

--Punto 8

{-Al estar en esa situacion los Heroes seguiran presumiendo infinitamente,
ya que como ambos tienen el mismo reconocimiento y la rareza total de sus
objetos es 0 deben de realizar las tareas del otro, pero ninguno realizo
tareas. Por lo tanto se volvera a realizar la funcion "presumir" con el
mismo caso infinitamente porque nunca se llegara a un caso no recursivo-}

--Punto 9

type Labor = [Tarea]

realizarLabor :: Heroe -> Labor -> Heroe
realizarLabor unHeroe = foldr (realizarTarea) unHeroe

--Punto 10

{-No se podra conocer el estado final del Heroe porque en la definicion
de "realizarLabor" se utiliza foldr, entonces se requiere evaluar toda
la lista para poder retornar el Heroe resultante y como esta lista es
infinita el programa jamas terminara de analizar-}