--Minigolfito

import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


--Punto 1

type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = Tiro {
    velocidad = 10,
    precision = modificarPresicionSegun (*2) unaHabilidad,
    altura = 0
}

madera :: Palo
madera unaHabilidad = Tiro {
    velocidad = 100,
    precision = modificarPresicionSegun (`div` 2) unaHabilidad,
    altura = 5
}

modificarPresicionSegun :: (Int -> Int) -> Habilidad -> Int
modificarPresicionSegun unaFuncion = unaFuncion . precisionJugador

hierro :: Int -> Palo
hierro n  unaHabilidad = Tiro { 
  velocidad = (* n) . fuerzaJugador $ unaHabilidad,
  precision = modificarPresicionSegun (`div` n) unaHabilidad,
  altura = max 0 (n-3)
}

palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

--Punto 2

golpe :: Palo -> Jugador -> Tiro
golpe unPalo = unPalo . habilidad

--Punto 3

type Obstaculo = Tiro -> (Tiro, Bool)

tunelConRampita :: Obstaculo
tunelConRampita unTiro 
  | superaTunelConRampita unTiro = (unTiro {
    velocidad = (*2) . velocidad $ unTiro,
    precision = 100,
    altura = 0
  }, True)
  | otherwise = fallaTiro unTiro

fallaTiro :: Tiro -> (Tiro, Bool)
fallaTiro unTiro = (unTiro {
  velocidad = 0,
  precision = 0,
  altura = 0
}, False)

precisionEs :: (Int -> Bool) -> Tiro -> Bool
precisionEs unaFuncion = unaFuncion . precision

alturaEs :: (Int -> Bool) -> Tiro -> Bool
alturaEs unaFuncion = unaFuncion . altura

velocidadEs :: (Int -> Bool) -> Tiro -> Bool
velocidadEs unaFuncion = unaFuncion . velocidad

laguna :: Int -> Obstaculo
laguna longitud unTiro 
  | superaLaguna unTiro = (unTiro { altura = (`div` longitud) . altura $ unTiro}, True)
  | otherwise = fallaTiro unTiro

hoyo :: Obstaculo
hoyo unTiro
  | superaHoyo unTiro = (unTiro { 
    velocidad =0,
    precision =0,
    altura = 0
  }, True)
  | otherwise = fallaTiro unTiro

superaHoyo :: Tiro -> Bool
superaHoyo unTiro = precisionEs (>95) unTiro && velocidadEs (between 5 20) unTiro && alturaEs (== 0) unTiro

superaLaguna :: Tiro -> Bool
superaLaguna unTiro = velocidadEs (>80) unTiro && alturaEs (between 1 5) unTiro

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita unTiro = precisionEs (>90) unTiro && alturaEs (==0) unTiro

--Punto 4

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (superaObstaculos unObstaculo . (`golpe` unJugador)) palos

superaObstaculos :: Obstaculo -> Tiro -> Bool
superaObstaculos unObstaculo = snd . unObstaculo

cuantosSupera :: [Obstaculo] -> Tiro -> Int
cuantosSupera obstaculos unTiro = length . takeWhile (`superaObstaculos` unTiro) $ obstaculos

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador obstaculos = maximoSegun (cuantosSupera obstaculos . (`golpe` unJugador)) palos

padresPerdedores :: [(Jugador, Puntos)] -> [String]
padresPerdedores = map (padre . fst) . niniosQueNoGanan

niniosQueNoGanan :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
niniosQueNoGanan puntosYJugadores 
  | (length . niniosConMasPuntos $ puntosYJugadores) > 1 = puntosYJugadores
  | otherwise = filter (/= (maximoSegun (snd) puntosYJugadores)) puntosYJugadores

niniosConMasPuntos :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
niniosConMasPuntos puntosYJugadores = filter ((== (maximum . map snd $ puntosYJugadores)) . snd) puntosYJugadores