import Text.Show.Functions

data Cliente = Cliente {
    nombre :: String, 
    deuda :: Float,
    facturas :: [Float]
} deriving (Show)

clientes = [ 
    Cliente "Biasutto" 6000 [4000, 5000],
    Cliente "Colombatti" 15000 [500,30000],
    Cliente "Marabotto" 200 [500000, 140000],
    Cliente "AnA" 200 [500,150]
 ]


--1--
clientesQueDeben plata [] = []
clientesQueDeben plata (cliente:clientes) 
   | ((> plata) . deuda) cliente 
                         = cliente:clientesQueDeben plata clientes
   | otherwise           = clientesQueDeben plata clientes


--2--
clientesPalindromos [] = []
clientesPalindromos (cliente:clientes) 
   | (palindromo . nombre) cliente 
                          = cliente:clientesPalindromos clientes
   | otherwise            = clientesPalindromos clientes

palindromo nombre = nombre == (reverse nombre)

--3--
clientesConFacturaDe plata [] = []
clientesConFacturaDe plata (cliente:clientes) 
   | ((elem plata) . facturas) cliente 
                    = cliente:clientesConFacturaDe plata clientes
   | otherwise      = clientesConFacturaDe plata clientes

--4--
{- Lo comenteo porque no hace faltan definirlas en haskell vienen por default
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) | f x       = x : filter f xs
                | otherwise = filter f xs
-}

--filter (elem 500000 . facturas) clientes


--5--
-- >filter palindromo ["neuquen", "salta", "anana"]

--6--
{-
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
-}

--6.1--
sumarDesde n = (n +):sumarDesde (n + 1) -- Ojo que asi solo revienta haskell hacer >(head . sumarDesde 3) 2 


--7--
--upperCase palabra = map toUpper palabra

--8--

sumarPalabras palabras = (sum . map length) palabras
--sumarPalabrasPF = (sum . map length)
