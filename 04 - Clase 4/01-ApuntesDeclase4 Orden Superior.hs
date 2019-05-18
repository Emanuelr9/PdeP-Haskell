--1--
{-
{-
data Cliente = Cliente {
    nombre :: String,
    deuda :: Float,
    facturas :: [Float]
} deriving (Show)

clientes = [Cliente "Leo" 20000 [5,15],
            Cliente "Pepe" 50 [2]]
-}
--1--
tieneDeudaMayor cliente plata = ((> plata).deuda) cliente
     
--2--
{-
clientesQueDeben [] monto = []
clientesQueDeben (cliente:clientes) monto
    |tieneDeudaMayor cliente monto = cliente : clientesQueDeben clientes monto
    |otherwise = clientesQueDeben cliente monto
-}

--3--
palindromo palabra = palabra == reverse palabra -- hay que hacer el reverse de hace 2 clases.

--4--

clientesPalindromos [] = []
clientesPalindromos (cliente:clientes)
    |(palindromo . nombre) = cliente : clientesPalindromos clientes
    |otherwise = clientesPalindromos cliente
-}
{-
--6--  Sabemos que es un booleano por eso ponemos un bool lado derecho del parentesis. 
filter :: ( a -> Bool )-> [a] -> [a]

--5--
filter _ [] = []
filter f (x:xs)
|f x = x : filter f xs
|otherwise = filter xs

--7--
clientesQueDeben clientes monto = filter (tieneDeudaMayor monto) clientes

--8--
clientesPalindromos clientes = filter (palindromo . nombre) clientes
-}
{-
--9-- 
--() se pone () porque recibe una funcion // a partir de un cliente da una deuda (a -> b) // lista de a va a lista de b a->b
map :: (a -> b)-> [a]-> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

deudas clientes = map deuda clientes
--deudas = map deuda 

--10--
deudaTotal {-cliente-} = sum . deudas {-cliente-}
-}
--11--

--any :: (a -> Bool) -> [a] -> Bool
--any _ [] = Flase
--any F (x:xs) = F x || any F xs

--12--

--PARTE 2 DE LA CLASE--

data Dinosaurio = Dinosaurio {
nombre :: String,
altura :: Float,
peso :: Float,
dieta :: [String]
} deriving (Show, Eq)

d1 = Dinosaurio "Fede" 4 3 ["carne"]

ds = [Dinosaurio "T-Rex" 7 400 ["Animales"]]

--1--
--esHerbivoro dino = elem "plantas" (dieta dino)
--esHerbivoro dino = (elem "plantas") . dieta dino
esHerbivoro = (elem "plantas") . dieta
herbivoros dinos = filter esHerbivoro dinos

--2--

--pesoTotal = Sum. pesos
pesos = map peso

--3--

--esBajo dino = (<4) . altura dino
esBajo = (<4) . altura
--sonTodosBajos = all esBajo

--4--
esCarnivoro = elem "animales" . dieta
--esOmivoro dino = esHerbivoro dino && esCarnivoro
--hayOmnivoro = any esOmnivoro

--5--
esPesado = (>200) . peso
--nombresPesados dinos = map nombre (filter esPesado dinos) 
nombresPesados = map nombre . filter esPesado

--6--
--alturaPar = even . altura
--todos = all alturaPar

--7--
--algunAlto = any (not . esBajo) --forma 1 pero no cumple la segunda parte
--algunAltoCarnivoro = any (not . esBajo) . filter esCarnivoro --forma 1 final
--algunAltoCarnivoro = any altoCarnivoro --forma 2 final
--altoCarnivoro d = not . esBajo $d && esCarnivoro d --altenativas o es una que funciona y otra que no
altoCarnivoro d = (not . esBajo) d && esCarnivoro d --altenativas o es una que funciona y otra que no

--8--
alimentar dino = dino {peso = peso dino +50}
-- >(all espesado . map alimentar . filter esCarnivoro) dinosaurio

--9--
    --a--
 --   transformarHerbivoros f = map f . filter esHervivoro
  --  transformarHerbivoros :: (dino -> a) -> [dino] -> [a] -- tiene que ser tipo dino porque estpoy usando dieta entocnes tiene que ser si o si dino -> a
    --b--
  --  todosConElPrimeroOAlgunoSegundo :: ([a] -> a) -> -> ->
  --  todosConElPrimeroOAlgunoSegundo criterio1 criterio2 dinos = all criterio1 dinos || criterio2 dinos
    --c--
 --   transformadoCumple c t = filter c . map t
    --d--
  --  esOmnivoro = all (==True) . pam [esCarnivoro,esHerbivoro]
--    pam funciones valor = map ($valor) funciones ({-F$v = Fv-})
    

    