module Lib where
import Text.Show.Functions

laVerdad = True

-- 1)
type CondicionViaje = Viaje -> Bool
type Fecha = (Int, Int, Int)

data Chofer = UnChofer {
    nombreChofer :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicionViaje :: CondicionViaje
}deriving (Show)

data Viaje = UnViaje {
    fecha :: Fecha,
    cliente :: Cliente,
    costo :: Float
}deriving (Show)

data Cliente = UnCliente {
    nombreCliente :: String,
    localidad :: String
}deriving (Show)


-- 2)

cualquierViaje :: CondicionViaje
cualquierViaje _ = True

viajesMasDe200 :: CondicionViaje
viajesMasDe200 = (>200).costo

viajesConNombreLargo :: Int -> CondicionViaje 
viajesConNombreLargo n = (>n).nombreLargo 
nombreLargo :: Viaje -> Int
nombreLargo = length.nombreCliente.cliente

clienteViveEnUnaZonaNoDeterminada :: String -> CondicionViaje
clienteViveEnUnaZonaNoDeterminada zona = (/=zona).localidad.cliente

noViveEn'VersionPau :: String -> CondicionViaje
noViveEn'VersionPau zona = not.((==zona).localidad.cliente)
-- 3)

lucas :: Cliente
lucas = UnCliente {
    nombreCliente = "Lucas",
    localidad = "Victoria"
}

daniel :: Chofer
daniel = UnChofer {
    nombreChofer = "Daniel",
    kilometraje = 23500,
    viajes = [UnViaje (20,04,2017) lucas 150],
    condicionViaje = clienteViveEnUnaZonaNoDeterminada "Olivos"
}

alejandra :: Chofer
alejandra = UnChofer {
    nombreChofer = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicionViaje = cualquierViaje
}

-- 4) 
tomaViaje :: Chofer -> Viaje -> Bool
tomaViaje chofer viaje = condicionViaje chofer $ viaje

-- 5)

liquidacionChofer :: Chofer -> Float
liquidacionChofer = sum . map costo . viajes

-- 6)
-- a)

realizarViaje viaje = filter (tomaViaje viaje)

-- b)

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes chofer1 chofer2 
    | cantViajes chofer1 > cantViajes chofer2 = chofer2
    | otherwise                               = chofer1

menosViajesHizo :: [Chofer] -> Chofer
menosViajesHizo = foldl1 menosViajes 

cantViajes :: Chofer -> Int
cantViajes = length.viajes


-- 7)
repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoInfy :: Chofer
nitoInfy = UnChofer {
    nombreChofer = "Nito Infy",
    kilometraje = 70000,
    viajes = repetirViaje (UnViaje (11,03,2017) lucas 50),
    condicionViaje = viajesConNombreLargo 3
}