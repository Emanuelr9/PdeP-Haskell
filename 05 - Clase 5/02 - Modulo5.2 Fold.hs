import Text.Show.Functions

--1--
lista = [1,2,3,4,5]


--2-- 

funcion a = foldr (:) [] a 

--3--

type Guerrero = (String, Int)
type Entrenador = Guerrero -> Guerrero

entrenar :: Entrenador -> Guerrero -> Guerrero
entrenar entrenador guerrero = entrenador guerrero


chickenNorris = ("Chicken Norris", 1000)
marcelito (nombre, fuerza) = (nombre, fuerza +50)

