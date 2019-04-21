--Punto 1
data Planta = Planta {
nombre::String,
puntosDeVida :: Int,
generaSoles :: Int,
poderDeAtaque :: Int
}deriving (Show)

data Zombie = Zombie {
nombreZombie :: String,
nivelDeMuerte :: Int,
articulos :: Int,
poderDeMordida :: Int
}deriving (Show)

data LineaDeDefensa = LineaDeDefensa { 
plantas ::[Planta], 
zombies :: [Zombie]
} deriving (Show)

peaShooter = Planta {nombre = "Peashooter" , puntosDeVida = 5, generaSoles =0, poderDeAtaque = 2}
repeater = Planta {nombre = "Repeater" , puntosDeVida = 5, generaSoles =0, poderDeAtaque = 4} -- hacer con copia?
sunFlower = Planta {nombre = "Sunflower" , puntosDeVida = 7, generaSoles =1, poderDeAtaque = 0}
nut = Planta {nombre = "Nut" , puntosDeVida = 100, generaSoles =0, poderDeAtaque = 0}

zombieBase = Zombie {nombreZombie = "Zombie", nivelDeMuerte = 6, articulos=0, poderDeMordida=1}
ballonZombie = Zombie {nombreZombie = "Pepe colgado", nivelDeMuerte = 12, articulos=1, poderDeMordida=1}
paperZombie = Zombie {nombreZombie = "Betl el chismoso", nivelDeMuerte = 16, articulos=1, poderDeMordida=2}
gargantuar = Zombie {nombreZombie = "Gargantuar Hulk Smash Puni god", nivelDeMuerte = 30, articulos=2, poderDeMordida=30}

--Punto 2
especialidad :: Planta -> String
esPeligroso :: Zombie -> Bool

--Item a
especialidad (Planta _ puntosDeVida generaSoles poderDeAtaque ) | (poderDeAtaque*2)>puntosDeVida = "Atacante" | generaSoles==1 = "Provedora" | otherwise = "Defensiva"

--Item b
esPeligroso (Zombie _ nivelDeMuerte articulos _) = nivelDeMuerte > 10 || articulos > 1

--Punto 3
linea1 = LineaDeDefensa { 
plantas =[sunFlower, sunFlower, sunFlower], 
zombies =[]
}

linea2 = LineaDeDefensa { 
plantas = [peaShooter, peaShooter, sunFlower, nut], 
zombies = [zombieBase, paperZombie]
}

linea3 = LineaDeDefensa { 
plantas = [sunFlower, peaShooter], 
zombies = [gargantuar, zombieBase, zombieBase]
}

linea4 = LineaDeDefensa { 
plantas = [peaShooter], 
zombies = [zombieBase]
}

agregarPlantaA :: Planta -> LineaDeDefensa -> [Planta]
agregarZombieA :: Zombie -> LineaDeDefensa -> [Zombie]

--Item a
agregarPlantaA planta linea = agregarPlantaALista (plantas linea) planta
agregarPlantaALista  [] planta = [planta]
agregarPlantaALista (x:xs) planta = x : agregarPlantaALista xs planta

agregarPlanta2 planta linea  = reverse $ planta : (reverse . plantas $ linea) --otra forma media rara de agregar al final
agregarPlanta3 planta linea  = (plantas linea) ++ [planta]                    --deberíamos usar esta forma

agregarZombieA zombie linea = (zombies linea) ++ [zombie]

--Item b
ataqueDePlantasEsMenor linea = (totalDeAtaque . plantas $ linea) < (totalDeMordiscos . zombies $ linea)
totalDeMordiscos [] = 0
totalDeMordiscos (x:xs) =  poderDeMordida x + totalDeMordiscos xs
totalDeAtaque [] = 0
totalDeAtaque (x:xs) = poderDeAtaque x + totalDeAtaque xs

todosLosZombiesSonPeligrosos linea = hayPeligro . zombies $ linea
hayPeligro [] = True
hayPeligro (x:xs) = esPeligroso x && hayPeligro xs

hayZombies linea = (length . zombies $ linea) > 0

estaEnPeligro linea = ataqueDePlantasEsMenor linea  || (todosLosZombiesSonPeligrosos linea && hayZombies linea)

--Item c
necesitaSerDefendida linea = esProvedora . plantas $ linea
esProvedora [] = True
esProvedora (x:xs) = (especialidad x == "Provedora") && esProvedora xs

--Item d
--i. En el caso de comparar la sumatoria del ataque de las plantas con respecto a la sumatoria de mordiscos de los zombies, esta se detendrá cuando la sumatoria de los zombies sea mayor al de las plantas sin la necesidad de recorrer de forma infinita los zombies, por el concepto de evaluación diferida.
--   Y si contemplamos que los zombies son infinitos siempre serán superiores con respecto a la sumatoria del ataque de las plantas de modo tal que evaluar que todos los zombies son peligrosos y que exista al menos 1 no sería necesario para determinar si la línea està en peligro.
--ii. Al consultar si una línea con cantidad infinita de Peashooter necesita ser defendida, esta daría falso y no necesitaría recorrer toda la lista por concepto de evalación diferida.
--    Pero si hablamos de una cantidad infinita de Sunflower, está recorrerá toda la lista sin dar respuesta porque se pide saber si todas las plantas son proveedores, y en este caso todas incluye a infinitos.

--Punto 4