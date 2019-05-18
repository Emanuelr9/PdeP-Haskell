--1--

--l1 l2 = filter (flip elem l1) l2 --T 39:00
--l1 l2 = filter (\e -> elem l1) l2

--2-- 

--($) funcion parametro
-- flip ($) parametro funcion

--4--

maximoSegun z a b
    | z a > z b = a
    |otherwise = b

--7--

data Dinosaurio = Dinosaurio {
nombre :: String,
altura :: Float,
peso :: Float,
dieta :: [String]
} --deriving (Show) -- puedo agregar el Eq y funciona o puedo definir cuando son eq 

instance Eq Dinosaurio where
    (Dinosaurio n1 _ _ _) == (Dinosaurio n2 _ _ _) = n1 ==n2

instance Ord Dinosaurio where --porque me dice
    (Dinosaurio _ h1 _ _) > (Dinosaurio _ h2 _ _) = h1 > h2
    (Dinosaurio _ h1 _ _) <= (Dinosaurio _ h2 _ _) = h1 > h2

instance Show Dinosaurio where
    show (Dinosaurio n _ _ _) = "Dino " ++ n


d1 = Dinosaurio "Fede" 4 3 ["carne"]

ds = [Dinosaurio "T-Rex" 7 400 ["Animales"]]

