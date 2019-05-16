import Text.Show.Functions

--1--
doble x = x * 2
cuadruple x = (doble . doble) x
cuadruplePF = doble . doble

--2--

sumoDos x = x + 2
sumoDosPF = (+2) -- Funciona por aplicacion parcial

--3--
dobleMasDos = sumoDos . doble 
dobleMasDosPF = (+2) .  (*2)

--4--
dobleMasDosAR = doble . sumoDos

--5--
nombrePar nombre = (even . length) nombre

--nombreParPF = even . length

--6--
tupla = ("hola", 15, sexo)
sexo = "m"
edad = snd

--7--
mayorDeEdad edad = edad > 18
esMayorDeEdadSinPF tupla2 = mayorDeEdad . edad tupla2 --Sin point Free
esMayorDeEdad = mayorDeEdad . edad

--8--
type Persona = (String, Integer)

laura = ("laura",41)

--9--

type Persona2 = (String, Integer, String)


laura2 = ("Laura", 41, "Medrano 951 CABA")
edad2 (_,edad2,_) = edad2 --Pattern matching para que se pueda usar con los 3 valores de persona2

--10--
esMenorEdad persona = (not . mayorDeEdad . edad) persona
esMenorEdadPF = not . mayorDeEdad . edad
esMenorEdad2PF = not . mayorDeEdad . edad2
esMenorEdad2 persona = (not . mayorDeEdad . edad2) persona

--11--
--Mod se utiliza en la consola directamente
-- >Mod 10 5 
--0

--12--
siguiente = (1 +)
nuevoDoble = (2 *)
cuadrado = (^ 2)
cubo = (^ 3)

--13--
esP = ('p' ==) --Usando el metodo aplicacion parcial
palabraEsP = (('p' ==) . head)

--Ahora, la verdad es que no hace falta escribir todo eso porque puedo codear directamente en el main: (('p'==) . head) "Palabras"

--14--

costoEstacionamiento horas = ((*50) . max 2) horas