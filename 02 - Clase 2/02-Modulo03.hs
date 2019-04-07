--1--

hol = [1, 2, 3] --Solo para listas numericas.

--2--

hola = [1, 3..16]

--3--
hola2 = [8,7.. -2]

--4-- La funcion head se aplica directamente en la consola

--5-- La funciones se aplican directamente en la consola

--6--
tupla = ("hola",150,True)
tupla1 = ("hola",10)

tupla2 = (hola,150,True)

--7-- Las funciones se aplican directamente en la consola

--8--
soloSnd (_,n) = n
soloTrd (_,_,n) = n

--9--
type Complejo = (Float,Float)

sumarComplejos :: Complejo -> Complejo -> Complejo
sumarComplejos (real1,imaginario1) (real2, imaginario2) = (real1 + real2 , imaginario1 + imaginario2)

--10--
minPar (a,b) = min a b
--minPar (a,b,c) = min a b c

--11--
data Persona = Persona String Int

--Persona "Santiago" 20 Asi se ingresa en la consola un valor del tipo data persona

--12--
nombre (Persona n e) = n
edad (Persona n e) = e

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