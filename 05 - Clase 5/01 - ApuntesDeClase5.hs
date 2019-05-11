{-
--1 1:06:00--
foldl1 :: ( a -> a -> a )-> [a] -> a
foldr1 :: ( a -> a -> a )-> [a] -> a

(+) :: Int -> Int -> Int

--2--
sum = foldl1 (+)
--sum lista = foldl1 (+) lista

--3--
prod = foldl1 (*)
--prod lista = foldl1 (*) lista
-}

--4--
{-
type Guerrero = (String, Integer)
type Entrenador = Guerrero -> Guerrero

fran = ("Fran",1000)
leo (nombre , fuerza) = (nombre , fuerza +50)
mati (nombre , fuerza) = (nombre, fuerza *10)

entrenadores = [leo, mati]

entrenar :: Entrenador -> Guerrero -> Guerrero
entrenar entrenador guerrero = entrenador guerrero
-}

--5--
{-
foldr _ s [] = s {-s es la semilla-}
foldr f s (x:xs) = f x (foldr f s xs)

foldl _ s [] = s
foldl f s (x:xs) = foldl f (f s x) xs
-}

--6--
data Dinosaurio = Herbivoro {nombre :: String, peso :: Float} | Carnivoro {nombre :: String, dientes :: Integer, peso :: Float}

pesoTotal = sum . pesos

pesos = map peso

--pesoTotal' = foldl (+) 0 . pesos {-pesos aplica a todo lo anterior al punto-}
--pesoTotal' ds = foldl (+) 0 . (pesos ds)
{-
pesoTotal' = foldl sumarPeso 0

sumarPeso s = (s +) . peso
--sumarPeso s dino = (s +) . peso dino
-}
caminar dino kms = dino {peso = peso dino - kms/4} 

ejercitarDino dino tramos = foldl caminar dino tramos
--ejercitarDino = foldl caminar

pasear dinos kms = map (flip caminar kms) dinos 

comerseA d1 d2 = d1 {peso = peso d1 + peso d2/2}
-- @ es tengo 2 datos para poner con arroba, tiene que coicidir con patron
--combatir (dino@Herbivoro n1 p1) (Herbivoro n2 p2) = dino -- dino es el primero me ahorra poner los 2 dino con los n1 p1
--combatir (dino@Herbivoro _ _) (Herbivoro _ _) = dino
combatir (h @ (Herbivoro _ _)) (c @ (Carnivoro _ _ _)) = comerseA c h
--combatir (c @ Carnivoro _ _ _) (h @ Herbivoro _ _) = comerseA c h
--combatir (h @ Herbivoro _ _) (c @ Carnivoro _ _ ) = comerseA c h

--torneo = foldl1 combatir 

type Evolucion = Dinosaurio -> Dinosaurio