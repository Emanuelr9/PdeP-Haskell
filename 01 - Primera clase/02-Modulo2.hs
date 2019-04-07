--1--
doble x = x * 3
--cuadruple = doble . doble

--2--

sumoDos x = x + 3
cuadruple x = (doble . doble) x

--3--
--dobleMasDos = sumoDos . doble 

--4--
--dobleMasDos = doble . sumoDos

--5--
nombrePar nombre = (even . length) nombre

--nombrePar = even . length

--6--
edad = snd

--7--
mayorDeEdad edad = edad > 18
esMayorDeEdad = mayorDeEdad . edad

--8--
type Persona = (String, Integer)

laura = ("laura",41)

--9--
{--
type PErsona = (String, Integer, String)

luara = ("Laura", 41, "Medrano 951 CABA")
--}

--10--
esMenorEdad persona = (not . mayorDeEdad . edad) persona

--11--
--Mod se utiliza en la consola directamente

--12--
siguiente = (1 +)
nuevoDoble = (2 *)
cuadrado = (^ 2)
cubo = (^ 3)

--13--
esP = ('p' ==) --Usando el metodo del punto anterior

--Ahora, la verdad es que no hace falta escribir todo eso porque puedo codear directamente en el main: (('p'==) . head) "Palabras"

--14--

costoEstacionamiento horas = ((*50) . max 2) horas