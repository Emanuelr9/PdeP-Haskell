import Text.Show.Functions

--1--

ho = [1, 2, 3] --Solo para listas numericas.

hol = [1..10]

--2--

hola = [1, 3..16]

--3--
hola2 = [8,7.. -2]

--4-- La funcion head se aplica directamente en la consola

{- >head [1,2,3]
1

--5-- La funciones se aplican directamente en la consola

>tail [1,2,3]
3
-}

--6--
tupla = ("hola",150,True)
tupla1 = ("hola",10)

tupla2 = (hola,150,True)

--7-- Las funciones se aplican directamente en la consola

head (x:_) = x

--8--
soloSnd (_,n) = n
soloTrd (_,_,n) = n

--9--
type Complejo = (Float,Float)

--si no pongo el tipo de complejo 1 y 2 no me va a entrar en la funcion que ya declare que funciona con complejos
complejo :: (Float,Float)
complejo = (1,2)
complejo2 :: (Float,Float)
complejo2 = (4,-1)

sumarComplejos :: Complejo -> Complejo -> Complejo
sumarComplejos (real1,imaginario1) (real2, imaginario2) = (real1 + real2 , imaginario1 + imaginario2)

--10--
minPar (a,b) = min a b
--minPar (a,b,c) = min a b c

--11--
data Persona = Persona String Int

--(Persona "Santiago" 20) --Asi se ingresa en la consola un valor del tipo data persona

--12--
nombre (Persona n e) = n
nombrePF (Persona n _) = n
edad (Persona n e) = e
edadPF (Persona _ e) = e

--13-- Hacer :t en tupla y hacer :t en una Persona viendo que pasa si se completa o no todo

--14--
mayorDeEdad edad = edad > 18

esMayorEdad :: Persona -> Bool
esMayorEdad = mayorDeEdad . edad
--Recordar que en la consola para ingresar a una persona se debe poner (nombrededato tipo1 tipo2)

--15--
data Persona1 = Persona1 String Int String String (Int, Int, Int) Bool Float
domicilio :: Persona1 -> String
domicilio (Persona1 _ _ dom _ _ _ _) = dom
juan = Persona1 "Juan" 29 "Ayacucho 556" "45232598" (17,7,1988) True 30.0
--graba un dato Persona1

--16--
data PersonaOk = PersonaOk{
    nombreOk :: String,
    edadOk :: Int,
    domicilioOk :: String,
    telefonoOk :: String,
    fechaNacimientoOk :: (Int,Int,Int),
    buenaPersonaOk :: Bool,
    plataOk :: Float
} deriving (Show)

juancho = PersonaOk {
    nombreOk = "Juan",
    telefonoOk = "45232598",
    domicilioOk = "Ayacucho 555",
    fechaNacimientoOk = (17,7,1988),
    buenaPersonaOk = True,
    edadOk = 29,
    plataOk = 30.0
}

--17--
--Se agrega en --16-- el deriving show para poder mostrar en consola al escribir juancho

--18--

data Sexo = Femenino | Masculino

--19--

data Vehiculo = Propio Float | Contratado String Float

type Flota = [Vehiculo]
vehiculos :: Flota
vehiculos = [Propio 9, Propio 15, Contratado "Remixes" 4, Propio 12]

valorLitroNafta :: Float
valorLitroNafta = 16.5

costoPorKm :: Vehiculo -> Float
costoPorKm (Propio consumo)     = valorLitroNafta * (consumo / 10)
costoPorKm (Contratado _ costo) = costo

--20--

data Parcial = Parcial String Int deriving (Show)

materia (mat, _) = mat
cantidadPreguntas (_, cant) = cant


data Alumno = Alumno {
    ejNombre :: String,
    ejFechaNacimiento :: (Int, Int, Int),
    ejLegajo :: Int,
    ejMateriasQueCursa :: [String],
    criterioEstudio :: CriterioEstudio
    } deriving (Show)
    
type CriterioEstudio = Parcial -> Bool

estudioso :: CriterioEstudio
estudioso _ = True

hijoDelRigor :: Int -> CriterioEstudio
hijoDelRigor n (Parcial _ preguntas) = preguntas > n

cabulero :: CriterioEstudio
cabulero (Parcial materia _) = (odd . length) materia 

nico = Alumno {
    ejFechaNacimiento = (10, 3, 1993),
    ejNombre = "Nico",
    ejMateriasQueCursa = ["sysop", "proyecto"],
    criterioEstudio = estudioso,
    ejLegajo = 124124
}

cambiarCriterioEstudio nuevoCriterio alumno = alumno { 
criterioEstudio = nuevoCriterio 
}

--20.x
-- Para que funcione hay que poner en la consola "cambiarCriterioEstudio (hijoDelRigor 5) nico"

--20.X2--
estudia :: Parcial -> Alumno -> Bool
estudia parcial alumno = (criterioEstudio alumno) parcial
parcialPDP = Parcial "PDP" 3

--PAra evaluar poner en el main (estudia parcialPDP . cambiarCriterioEstudio (hijoDelRigor 5)) con cantidad de materias par

