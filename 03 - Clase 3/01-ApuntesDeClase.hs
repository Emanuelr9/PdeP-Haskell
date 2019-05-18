--doble x = x*2
--1-- 30:00 al 38:00 diferencia entre Data y type

type Alumno = (String, Int)
nota::Alumno->Int
nota = snd

--2-- Funcion factorial

factorial 0 = 1
factorial n = n * factorial (n-1)

factorialGuardas n
    | n==0 =1
    | otherwise = n*factorialGuardas (n-1)

--3-- Triangulo de pascal

--pascal 1 1 = 1
{--
pascal _ 1 = 1
pascal fila columna
    |fila == columna = 1
    |otherwise = pascal (fila -1) (columna-1) + pascal (fila-1) columna--}

--4--
muchosDe n = n:muchosDe n --Esto es para generar una concatenacion de elementos sobre la lista

prim x y = x

cuadr x = x*x
--prim (cuadr3 , cuadr4)

--5--
foo x y
    |x>1 = x
    |otherwise =y

--6--
{--
data  Hamburguesa = Hamburguesa{
    nombre :: String,
    ingredientes :: [String]
}

basica = Hamburguesa {nombre = "Basica", ingredientes =[]}
personalizada= Hamburguesa {nombre = "Personalizada",ingredientes=[...]}
--}
--6.2

data Hamburguesa = Basica | Personalizada Extras deriving (Show,Eq) -- el Eq lo que pasa es que la guarda no se puede igualar con algo.
type Extras = [String]

--miBasica = Basica
--MiPersonalizada = Personalizada ["Lechuga","Queso"]

--6.3-- Hamburguesa completa
{--
completa = Personalizada ["Lechuga", "Tomate"]

agregar extra hambur
    | hambur == Basica = Personalizada [extra]
    | otherwise = Personalizada (extra: extras hambur)
extras (Personalizada lista) = lista -- este es el pattern matching (Personalizada lista)
--}

--7--
agregar extra Basica = Personalizada [extra]
agregar extra (Personalizada listaExtras) = Personalizada (extra:listaExtras)    

--8--Redefinir la hamburgueza

-- completa = agregar "lechuga" (agregar "tomate" Basica) --Esta no tiene composicion

--completa = (agregar "lechuga" . agregar "tomate") Basica -- Esta seria la funcion
completa = agregar "lechuga" . agregar "tomate" $Basica

--9--
{--
quitar _ Basica = Basica -- esto nos resuelve el primer caso
--quitar extra (Personalizada listaExtras)
quitar extraAQuitar (Personalizada[extra]) -- esta guarda nos resuelbe el segundo cas
    |extraAquitar==extra = Basica
    |otherwise = Personalizada [extra]
quitar extraAquitar (Personalizada(extra:extras))
    |extraAQuitar == extra = quitar extraAQuitar (Personalizada extras)
    |otherwise              = agregar extra (quitar extraAQuitar (Personalizada extras))
--}

--10--

precioAPagar []=0
--precioAPagar (precioAPagar : listaHamburs)=percioUnitario hambur + precioAPagar listaHamburs
percioUnitario Basica = 100
percioUnitario (Personalizada extras) = percioUnitario Basica + 50 * (length extras) 

--11--
preparar = Basica:Basica:completa:preparar

--12--
--Ingresando en haskell precioAPagar (take100 preparar)
--(precioAPAgar.take100)preparar
--precioAPAgar.take100 $preparar
