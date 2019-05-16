--1--

factorial n 
  | n == 0     = 1
  | n > 0      = n * factorial (n - 1)


--2--

otroFactorial 0  =  1
otroFactorial n  =  n * factorial (n - 1)

--3--
fibonacci 0  = 1
fibonacci 1  = 1
fibonacci n  = fibonacci (n - 1) + fibonacci (n - 2)

--4--
primo :: Int ->  Bool
noHayDivisores :: Int -> Int -> Int -> Bool

primo 1 = False
primo 2 = True
primo n = noHayDivisores 2 (n - 1) n 
-- Es primo si no hay divisores de ese numero desde 2 hasta ese numero menos uno

noHayDivisores minimo maximo n 
    | mod n minimo == 0  = False --Si hay divisiores de ese numero se corta
--    | esDivisor minimo n = False --Esta es otra forma de hacer la linea de arriba pero mas expresiva.
    | minimo == maximo   = True --Si no hay divisores y llego al final entonces es primo
    | otherwise          = noHayDivisores (minimo + 1) maximo n --La recursividad hace que se repita todo 

esDivisor unNumero otroNumero = mod otroNumero unNumero == 0

--5--
{-    
length [] = 0
length (x:xs) = 1 + (length xs)
--Algo esta pasando con el length
-}

--sum :: Num a => [a] -> a

-- sum [] = 0
-- sum (x:xs) = x + sum xs


-- last [x] = x
-- last (x:xs) = last xs

--6--

muchosDe n = n:(muchosDe n) -- Esta funcion no termina nunca OJO!!!

-- hacer en el main: (take 10  . muchosDe) 5

