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

noHayDivisores minimo maximo n 
    | mod n minimo == 0  = False
    | minimo == maximo   = True
    | otherwise          = noHayDivisores (minimo + 1) maximo n

esDivisor unNumero otroNumero = mod otroNumero unNumero == 0

    --5--
    
    -- length [] = 0
    -- length (x:xs) = 1 + length xs
