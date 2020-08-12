--                          SIMULACRO DE PARCIAL
import Text.Show.Functions


data Ladron = UnLadron {
    nombreLadron :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
} deriving Show

data Rehen = UnRehen {
    nombreRehen :: String,
    nivelDeComplot :: Int,
    nivelDeMiedo :: Int,
    planesContraLadrones :: [Plan]
} deriving Show

type Habilidad = String
type Arma = Rehen -> Rehen
type Plan = Ladron -> Ladron


--                                  Armas

pistola :: Int -> Arma
pistola calibre unRehen = unRehen { nivelDeComplot = reduceNivelDeComplot (5*calibre) unRehen,
    nivelDeMiedo = aumentarMiedoEn (3*(longitudNombreRehen unRehen)) unRehen}

reduceNivelDeComplot :: Int -> Rehen -> Int
reduceNivelDeComplot cantidad = max 0 . (subtract cantidad) . nivelDeComplot

aumentarMiedoEn :: Int -> Rehen -> Int
aumentarMiedoEn cantidad = (+ cantidad) . nivelDeMiedo

longitudNombreRehen :: Rehen -> Int
longitudNombreRehen = length . nombreRehen

ametralladora :: Int -> Arma
ametralladora balasRestantes unRehen = unRehen { nivelDeComplot = div (nivelDeComplot unRehen) 2,
    nivelDeMiedo = aumentarMiedoEn balasRestantes unRehen }

-------------------------------------------------4) Intimidaciones--------------------------------------------------------------------------

--disparos :: Ladron -> Rehen -> Rehen
--disparos unLadron unRehen =

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo unLadron unRehen | suNombreEs "Berlin" unLadron = unRehen { nivelDeMiedo = aumentarMiedoEn (sum . map length $ (habilidades unLadron)) unRehen}
                               | suNombreEs "Rio" unLadron = unRehen { nivelDeComplot = aumentaNivelDeComplot 20 unRehen}
                               | otherwise = unRehen { nivelDeMiedo = aumentarMiedoEn 10 unRehen}

suNombreEs :: String -> Ladron -> Bool
suNombreEs unNombre = (== unNombre) . nombreLadron

aumentaNivelDeComplot :: Int -> Rehen -> Int
aumentaNivelDeComplot cantidad = (+ cantidad) . nivelDeComplot 

----------------------------------------------Planes------------------------------------------------------------------------------------------------------------------

atacarAlLadron :: Rehen -> Plan
atacarAlLadron unRehen unLadron = unLadron { armas = drop (cantidadDeArmasAQuitar unRehen) (armas unLadron)}
    where cantidadDeArmasAQuitar = (flip div 10) . length . nombreRehen

esconderse :: Plan
esconderse unLadron = unLadron { armas = drop (armasPerdidas unLadron) (armas unLadron)}
    where armasPerdidas = (flip div 3) . length . habilidades

-----------------------------------------------1) Modelaje-------------------------------------------------------------------------------------------------------------------------

--a.
tokio :: Ladron
tokio = UnLadron "Tokio" ["trabajo psicologico", "entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

--b.
profesor :: Ladron
profesor = UnLadron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []

--c.
pablo :: Rehen
pablo = UnRehen "Pablo" 40 30 [esconderse]

--d.
arturito :: Rehen
arturito = UnRehen "Arturito" 70 50 [esconderse, atacarAlLadron pablo]

---------------------------------------------2) Ladron inteligente-----------------------------------------------------------------------------------------------------------

esInteligente :: Ladron -> Bool
esInteligente unLadron = suNombreEs "Profesor" unLadron || ((> 2) . length . habilidades $ unLadron)

---------------------------------------------3) Ladron con arma nueva--------------------------------------------------------------------------------------------------------------------

nuevaArma :: Arma -> Ladron -> Ladron
nuevaArma unArma  unLadron = unLadron { armas = unArma : (armas unLadron)}

---------------------------------------------5) Calmar las aguas---------------------------------------------------------------------------------------------------------------------

--calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
--calmarLasAguas unLadron rehenes = foldr (calmarRehen unLadron) [] rehenes

--calmarRehen :: Ladron -> Rehen -> [Rehen] -> [Rehen]
--calamarRehen  unLadron unRehen rehenes | nivelDeComplot Rehen > 60 = (disparos unLadron unRehen) : rehenes
--                                       | otherwise = unRehen : rehenes

------------------------------------------------7) La cosa pinta mal --------------------------------------------------------------------------------------------------------------------

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal ladrones rehenes = (promedioDe rehenes nivelDeComplot) > ((promedioDe rehenes nivelDeMiedo) * cantArmas ladrones)

promedioDe :: [Rehen] -> (Rehen -> Int) -> Int
promedioDe rehenes unaFuncion = div (sum . map unaFuncion $ rehenes) (length rehenes)

cantArmas :: [Ladron] -> Int
cantArmas = length . concatMap armas

---------------------------------------------------- 6 -------------------------------------------------------------------------

escapaDeLaPolicia :: Ladron -> Bool
escapaDeLaPolicia = any (habilidadEmpiezaConNpalabras 2 ["disfrazarse", "de"]) . habilidades

habilidadEmpiezaConNpalabras :: Int -> [String] -> Habilidad -> Bool
habilidadEmpiezaConNpalabras unNumero palabras = (== palabras) . take unNumero . words

------------------------------------------------------------- 8 ---------------------------------------------------------------------------

empezarRebelion :: Ladron -> [Rehen] -> Ladron
empezarRebelion unLadron = foldr ($) unLadron . planDeRehenes . filter (esValiente) . map (desmotivacion)

planDeRehenes :: [Rehen] -> [Plan]
planDeRehenes = concatMap planesContraLadrones

desmotivacion :: Rehen -> Rehen
desmotivacion unRehen = unRehen { nivelDeComplot = reduceNivelDeComplot 10 unRehen}

esValiente :: Rehen -> Bool
esValiente unRehen = (nivelDeComplot unRehen) > (nivelDeMiedo unRehen)


------------------------------------------------------------ 9 --------------------------------------------------------------------------------------

planValencia :: [Rehen] -> [Ladron] -> Int
planValencia rehenes = (* 1000000) . cantArmas . map (flip empezarRebelion rehenes) . map armarse

armarse :: Ladron -> Ladron
armarse unLadron = unLadron {armas = (ametralladora 45) : (armas unLadron)}

------------------------------------------------------------ 10--------------------------------------------------------------

{-No se ejecutaria el plan valencia, pues nunca se terinara de evaluar la lista con todas las armas debido a que necesitamos
evaluar TODAS las arma. Por lo tanto no aplica el lazy evaluation-}

------------------------------------------------------------- 11 ------------------------------------------------------------

{-Dependera de si alguno de los rehenes tiene el plan de esconderse, ya que de ser el caso hay que analizar cuantas habilidades tiene
y se tiene que recorrr toda la lista. Si ninguno planea esconderse no importa la cantidad de habilidades ya que no se utilizan
en la funcion para este caso.-}

-------------------------------------------------------------- 12 --------------------------------------------------------------------------------

funcion :: b -> (a -> [c]) -> (b -> a -> Bool)  -> Int -> [a] -> Bool
funcion cond num lista str = (> str) . sum . map (length . num) . filter (lista cond)


