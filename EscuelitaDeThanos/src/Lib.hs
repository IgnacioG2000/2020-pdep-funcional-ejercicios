module Lib where
import Text.Show.Functions

laVerdad = True

-- 1)
type Gema = Personaje -> Personaje

data Guantelete = UnGuantelete {
    gemas :: [Gema],
    material :: String
}deriving (Show)

data Personaje = UnPersonaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show)

type Universo = [Personaje]


chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | guanteleteCompleto guantelete = reducirMitad universo
                              | otherwise = universo

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo


-- 2)

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((<=45).edad)

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . masDeUnaHabilidad

masDeUnaHabilidad :: Universo -> Universo
masDeUnaHabilidad = filter ((>1).length.habilidades)


-- 3)

laMente :: Int -> Gema
laMente = quitarEnergia

quitarEnergia :: Int -> Personaje -> Personaje
quitarEnergia cantidad personaje = personaje {energia = energia personaje - cantidad}

elAlma :: String -> Gema
elAlma habilidad = quitarEnergia 10 . eliminarUnaHabilidadDeseada habilidad

eliminarUnaHabilidadDeseada :: String -> Personaje -> Personaje
eliminarUnaHabilidadDeseada habilidad personaje = personaje {habilidades = filter (/=habilidad) (habilidades personaje)}

elEspacio :: String -> Gema
elEspacio planeta = quitarEnergia 20 . transportarAlPlaneta planeta

transportarAlPlaneta :: String -> Personaje -> Personaje
transportarAlPlaneta planeta personaje = personaje {planeta = planeta}

elPoder :: Gema
elPoder personaje = (eliminarHabilidades. quitarEnergia (energia personaje))  personaje

eliminarHabilidades :: Personaje -> Personaje
eliminarHabilidades personaje   | ((<=2).length.habilidades) personaje = personaje {habilidades = []}
                                | otherwise = personaje


elTiempo :: Gema
elTiempo  = quitarEnergia 50 . reducirEdadALaMitad 

reducirEdadALaMitad :: Personaje -> Personaje
reducirEdadALaMitad personaje = personaje {edad = max 18 (edad personaje `div` 2)}

gemaLoca :: Gema -> Gema
gemaLoca gemas = gemas.gemas


-- 4)

guanteletePunto4 :: Guantelete
guanteletePunto4 = UnGuantelete {
    gemas = [elTiempo , (elAlma "usar Mjolnir") , gemaLoca (elAlma "programacion en Haskell")],
    material = "goma"
}

-- 5)

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldr ($) personaje gemas


-- 6)

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelete = gemaMasPoderosaDe personaje (gemas guantelete)

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gema) 
    | (energia.gema1) personaje < (energia.gema1) personaje = gemaMasPoderosaDe personaje (gema1:gema)
    | otherwise = gemaMasPoderosaDe personaje (gema2:gema)



personajePrueba :: Personaje
personajePrueba = UnPersonaje {
    edad = 20,
    energia = 60,
    habilidades = ["Integrales","Derivadas","Matematica"],
    nombre = "Nacho",
    planeta = "Tierra"
}



infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete (infinitasGemas elTiempo) "vesconite"

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete


