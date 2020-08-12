module LaCasaDePdePEL where

import Text.Show.Functions

type Nivel = Int
type Habilidad = String
type Arma  = Rehen -> Rehen
type Plan  = Ladron -> Ladron

data Ladron = Ladron {
    nombreLadron :: String,
    habilidades  :: [Habilidad],
    armas        :: [Arma]
} deriving(Show)

data Rehen = Rehen {
    nombreRehen  :: String,
    nivelComplot :: Nivel,
    nivelMiedo   :: Nivel,
    plan         :: Plan
} deriving(Show)


mapArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
mapArmas f ladron = ladron { armas = f $ armas ladron }


mapNombreRehen :: (String -> String) -> Rehen -> Rehen
mapNombreRehen  f rehen = rehen { nombreRehen = f $ nombreRehen rehen }

mapNivelComplot :: (Nivel -> Nivel) -> Rehen -> Rehen
mapNivelComplot f rehen = rehen { nivelComplot = max 0 . f $ nivelComplot rehen }

mapNivelMiedo :: (Nivel -> Nivel) -> Rehen -> Rehen
mapNivelMiedo f rehen = rehen { nivelMiedo = max 0 . f $ nivelMiedo rehen }


incNivelMiedo :: Nivel -> Rehen -> Rehen
incNivelMiedo cantidad = mapNivelMiedo   (+cantidad)

incNivelComplot :: Nivel -> Rehen -> Rehen
incNivelComplot cantidad = mapNivelComplot (+cantidad)

decNivelMiedo :: Nivel -> Rehen -> Rehen
decNivelMiedo   cantidad = mapNivelMiedo   (subtract cantidad)

decNivelComplot :: Nivel -> Rehen -> Rehen
decNivelComplot cantidad = mapNivelComplot (subtract cantidad)


--------------
-- Punto 01 --
--------------

tokio    = Ladron "Tokio"    ["trabajo psicológico", "entrar en moto"]      [pistola 9, pistola 9, ametralladora 30]
rio      = Ladron "Rio"      ["hackear", "hacer reir"]                      [ametralladora 45]
berlin   = Ladron "Berlin"   ["enloquecer", "dar discursos", "dar ordenes"] [pistola 45, pistola 45]
nairobi  = Ladron "Nairobi"  ["empezar el matriarcado"]                     [ametralladora 25]
profesor = Ladron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []


ariadna  = Rehen "Ariadna" 20 60 id
pablo    = Rehen "Pablo" 40 30 esconderse
arturito = Rehen "Arturo" 70 50 (atacarAlLadron pablo . esconderse)


ladrones = [tokio, rio, berlin, nairobi, profesor]
rehenes  = [ariadna, pablo, arturito]


--------------
-- Punto 02 --
--------------

esInteligente :: Ladron -> Bool
esInteligente ladron = es ladron "Profesor" || cantidadHabilidades ladron > 2

es :: Ladron -> String -> Bool
es ladron nombre = nombreLadron ladron == nombre

cantidadHabilidades :: Ladron -> Int
cantidadHabilidades = length . habilidades


--------------
-- Punto 03 --
--------------

conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma arma = mapArmas (arma :)


--------------
-- Punto 04 --
--------------

disparos :: Rehen -> Ladron -> Rehen
disparos rehen = ($ rehen) . armaMasMiedosaPara rehen

hacerseElMalo :: Rehen -> Ladron -> Rehen
hacerseElMalo rehen ladron
    | es ladron "Berlin" = (incNivelMiedo . sum . map length . habilidades) ladron $ rehen
    | es ladron "Rio"    = incNivelComplot 20 rehen
    | otherwise          = incNivelMiedo 10 rehen

armaMasMiedosaPara :: Rehen -> Ladron -> Arma
armaMasMiedosaPara rehen = maximumBy (nivelMiedo . ($ rehen)) . armas

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy f = foldl1 (maxBy f)

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y
  | f x > f y = x
  | otherwise = y


--------------
-- Punto 05 --
--------------

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas ladron = filter ((> 60) . nivelComplot) . map (flip disparos ladron)


--------------
-- Punto 06 --
--------------

puedeEscaparseDeLaPolicia :: Ladron ->  Bool
puedeEscaparseDeLaPolicia = any (empiezaCon "Disfrazarse de") . habilidades

empiezaCon :: Eq a => [a] -> [a] -> Bool
empiezaCon sublista = (== sublista) . take (length sublista)


--------------
-- Punto 07 --
--------------

pintaMal :: [Ladron] -> [Rehen] -> Bool
pintaMal ladrones rehenes = nivelComplotPromedio rehenes > nivelMiedoPromedio rehenes * cantidadTotalArmas ladrones

nivelComplotPromedio :: [Rehen] -> Int
nivelComplotPromedio = promedioSegun nivelComplot

nivelMiedoPromedio :: [Rehen] -> Int
nivelMiedoPromedio = promedioSegun nivelMiedo

promedioSegun :: (a -> Int) -> [a] -> Int
promedioSegun f xs = sum (map f xs) `div` length xs


--------------
-- Punto 08 --
--------------

rebelarseContra :: [Rehen] -> Ladron -> Ladron
rebelarseContra rehenes ladron = foldl rebelarse ladron . map (decNivelComplot 10) $ rehenes

rebelarse :: Ladron -> Rehen -> Ladron
rebelarse ladron rehen
  | esSubversivo rehen = (plan rehen) ladron
  | otherwise          = ladron

esSubversivo :: Rehen -> Bool
esSubversivo rehen = nivelComplot rehen > nivelMiedo rehen

atacarAlLadron :: Rehen -> Plan
atacarAlLadron rehenAliado = quitarArmas (cantidadLetrasNombre rehenAliado `div` 10)

cantidadLetrasNombre :: Rehen -> Int
cantidadLetrasNombre = length . nombreRehen

esconderse :: Plan
esconderse ladron = quitarArmas (cantidadHabilidades ladron `div` 3) ladron


--------------
-- Punto 09 --
--------------

planValencia :: [Rehen] -> [Ladron] -> Int
planValencia rehenes ladrones = (*1000000) . cantidadTotalArmas . map (rebelarseContra rehenes . conseguirArma (ametralladora 45)) $ ladrones

cantidadTotalArmas :: [Ladron] -> Int
cantidadTotalArmas = sum . map cantidadArmas

cantidadArmas :: Ladron -> Int
cantidadArmas = length . armas

pistola :: Int -> Arma
pistola calibre rehen = incNivelMiedo (3 * length (nombreRehen rehen)) . decNivelComplot (calibre * 5) $ rehen

ametralladora :: Int -> Arma
ametralladora balas = incNivelMiedo balas . mapNivelComplot (`div` 2)

quitarArmas :: Int -> Ladron -> Ladron
quitarArmas cantidad = mapArmas (drop cantidad)


--------------
-- Punto 10 --
--------------

-- No se puede, el plan valencia necesita saber la cantidad de armas del ladron.
-- La consulta nunca terminaría de ejecutarse y se colgaría


--------------
-- Punto 11 --
--------------

-- Puede ser sí, como no. Depende del plan que tiene el rehen.
-- Si el plan es esconderse necesita saber la cantidad de habilidades del ladron
-- Si pasa eso, ocurre lo mismo que se explicó anteriormente.


--------------
-- Punto 12 --
--------------

funcion ::     b   -> (a -> [c]) -> (b -> a -> Bool) -> Int -> ([a] -> Bool)
--           ^^^^^    ^^^^^^^^^^    ^^^^^^^^^^^^^^^^    ^^^    ^^^^^^^^^^^^^
--Tipo de:   cond        num             lista          str       retorno

funcion cond num lista str =  (> str) . sum . map (length . num) . filter (lista cond)