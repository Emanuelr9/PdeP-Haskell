import Text.Show.Functions

--1
data Chofer = Chofer{
    nombreChofer :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condiciones :: [Condicion]
}deriving (Show)

data Viaje = Viaje {
    fecha :: Fecha,
    cliente :: Cliente,
    costo :: Int
}deriving (Show)

data Cliente = Cliente {
    nombreCliente :: String,
    direccion :: String
}deriving (Show)

type Fecha = (Int, Int, Int)
type Condicion = Viaje -> Bool

--2
valeTodo _ = True 
amarrete = (>200) . costo --Aplicacion parcial y composicon

--clienteConNombreLargo :: Int -> Condicion
clienteConNombreLargo n = (>n) . length . nombreCliente . cliente

--clienteNoViveEn :: String -> Condicion
clienteNoViveEn lugar = (/= lugar) . direccion . cliente

--3
lucas = Cliente {
    nombreCliente = "Lucas",
    direccion = "Victoria"
}

viaje = Viaje {
    fecha = (20,4,2017),
    cliente = lucas,
    costo =  150
}

daniel = Chofer {
    nombreChofer = "Daniel",
    kilometraje = 23500,
    viajes = [viaje, viaje],
    condiciones = [ clienteNoViveEn "Olivos"]
}

alejandra = Chofer {
    nombreChofer = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condiciones = [valeTodo]
}

noTiene = Chofer {
    nombreChofer = "",
    kilometraje = 0,
    viajes = [],
    condiciones = []
}

--4
--foldr ($) 0 [(^ 6 ), ( 2 *), ( 1 +)]
--puedeTomarViaje chofer viaje = foldr ($) viaje (condicione chofer).       Es como el sum

puedeTomarViaje viaje chofer = all (True ==) (map ($viaje) (condiciones chofer))

--5
liquidacion chofer = sum $ map costo $ viajes chofer
--liquidacion = sum.map $ costo viajes Point free

--6
realizarViaje viaje choferes = filter (puedeTomarViaje viaje) choferes

--choferMenorViaje::Ord a => [Chofer] -> 0

choferMenorViaje [] = noTiene
choferMenorViaje choferes = head (filter (tieneMinimo (cantMenorViaje choferes)) choferes)
tieneMinimo cantMinima chofer = cantMinima == length (viajes chofer)
cantMenorViaje = minimum.map length.map viajes

efectuarViaje viaje chofer = chofer {viajes = viajes chofer ++ viaje}

--7
viajeInfinito = Viaje {
    fecha = (11,3,2017),
    cliente = lucas,
    costo =  50
} 

nito = Chofer {
    nombreChofer = "Nito Infy",
    kilometraje = 70000,
    viajes = [],
    condiciones = [valeTodo]
}
-- Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y 
-- toma cualquier viaje donde el cliente tenga al menos 3 letras. Puede ayudarse con esta función: