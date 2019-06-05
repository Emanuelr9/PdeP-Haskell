import Text.Show.Functions

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

valeTodo _ = True 
amarrete = (>200) . costo --Aplicacion parcial y composicon

-- Hacer esto no es util para lo que ya defini como condicion
-- nombreDeClienteMayor viaje cantidadDeLetras =  (> cantidadDeLetras) ((length . nombreCliente . cliente) viaje)
-- nombreDeClienteMayor2 viaje cantidadDeLetras =  ((> cantidadDeLetras).length . nombreCliente . cliente) viaje

clienteConNombreLargo :: Int -> Condicion
clienteConNombreLargo n = (>n) . length . nombreCliente . cliente

clienteNoViveEn :: String -> Condicion
clienteNoViveEn donde = (/= donde) . direccion . cliente

lucas = Cliente {
    nombreCliente = "Lucas",
    direccion = "Victoria"
}

lista = [1,2,3,4]

daniel = Chofer {
    nombreChofer = "Daniel",
    kilometraje = 23500,
    viajes = [ Viaje {
        fecha = (20,4,2017),
        cliente = lucas,
        costo =  150
        } ],
    condiciones = clienteNoViveEn "olivos"
}

alejandra = Chofer {
    nombreChofer = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condiciones = valeTodo
}



