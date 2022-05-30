module Lib where
import Text.Show.Functions

laVerdad = True


-- Punto 1)
type Gema = Personaje -> Personaje

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}deriving (Show)

data Personaje = UnPersonaje {
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show)

type Universo = [Personaje]


personajePrueba = UnPersonaje {
    edad = 20,
    energia = 60,
    habilidades = ["usar Mjolnir","programacion en Haskell","Derivadas"],
    nombre = "Ignacio",
    planeta = "Tierra"
}


guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = ((==6).length.gemas) guantelete && material guantelete == "uru"

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | guanteleteCompleto guantelete = reducirMitad universo
                              | otherwise = universo

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo

-- Punto 2)

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((<45).edad)

energiaTotal :: Universo -> Float
energiaTotal = sum. map energia


-- Punto 3)

restarEnergia :: Personaje -> Float -> Float
restarEnergia personaje cantidad = energia personaje - cantidad

quitarHabilidadEspecifica :: String -> [String] -> [String]
quitarHabilidadEspecifica habilidad = filter (/=habilidad)

reducirEdad :: Personaje -> Int
reducirEdad personaje = (edad personaje) `div` 2

sacarHabilidades :: [String] -> [String]
sacarHabilidades habilidades | ((<=2).length) habilidades = []
                             | otherwise = habilidades

-- ======================GEMAS==========================================================
mente :: Float -> Gema
mente cantidad personaje = personaje {energia = restarEnergia personaje cantidad}

alma :: String -> Gema
alma habilidad personaje = personaje {energia = restarEnergia personaje 10, habilidades = quitarHabilidadEspecifica habilidad (habilidades personaje)}

espacio :: String -> Gema
espacio planeta personaje = personaje {planeta = planeta}

poder :: Gema
poder personaje = personaje {energia = restarEnergia personaje (energia personaje), habilidades = sacarHabilidades (habilidades personaje)}

tiempo :: Gema
tiempo personaje = personaje {edad = max 18 (reducirEdad personaje)}

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

-- ======================================================================================


-- Punto 4)

guanteletePrueba :: Guantelete
guanteletePrueba = UnGuantelete {
    material = "goma",
    gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]
}

-- Punto 5)

listaGemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]

utilizar :: Personaje -> [Gema] -> Personaje
utilizar personaje = foldr ($) personaje 

-- Punto 6)

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = gemaMasPoderosaDe personaje (gemas guantelete) 

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gemas) | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
                                                | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)


gemaMasPoderosaKate :: Guantelete -> Personaje -> Gema
gemaMasPoderosaKate ( UnGuantelete _ [gema] ) _ = gema
gemaMasPoderosaKate ( UnGuantelete material (gema1:gema2:gemas) ) personaje | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaKate (UnGuantelete material (gema1:gemas)) personaje 
                                                                     | otherwise = gemaMasPoderosaKate (UnGuantelete material (gema2:gemas)) personaje 