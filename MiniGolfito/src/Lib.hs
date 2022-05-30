module Lib where
import Text.Show.Functions

laVerdad = True
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

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- 1)
-- a)

putter :: Palo
putter habilidad = UnTiro {
    velocidad = 10,
    precision = precisionJugador habilidad * 2,
    altura = 0
}

madera :: Palo
madera habilidad = UnTiro {
    velocidad = 100,
    precision = precisionJugador habilidad `div` 2,
    altura = 5
}

hierros :: Int -> Palo
hierros numero habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * numero,
    precision = precisionJugador habilidad `div` numero,
    altura = max 0 (numero-3)
}

-- b)
type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter,madera] ++ map hierros [1..10]

-- 2)

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador