{-- --Punto 1.
data Planta = Planta { nombrePlanta::String, puntosDeVida::Int, generaSoles::Int, poderDeAtaque::Int} deriving (Show)
data Zombie = Zombie { nombreZombie::String, nivelDeMuerte::Int, articulos::Int, poderDeMordida::Int} deriving (Show)

peaShooter = Planta {nombrePlanta="Peashooter", puntosDeVida=5, generaSoles=0, poderDeAtaque=2}
repeater = Planta { nombrePlanta="Repeater", puntosDeVida=5, generaSoles=0, poderDeAtaque=4}
sunFlower = Planta { nombrePlanta="Sun Flower", puntosDeVida=7, generaSoles=1, poderDeAtaque=0}
nut = Planta { nombrePlanta="Nut", puntosDeVida=100, generaSoles=0, poderDeAtaque=0}
zombieBase = Zombie { nombreZombie="Zombie", nivelDeMuerte=6, articulos=0, poderDeMordida=1}
ballonZombie = Zombie { nombreZombie="Pepe colgado", nivelDeMuerte=12, articulos=1, poderDeMordida=1}
newspaperZombie = Zombie { nombreZombie="Beto el chismoso", nivelDeMuerte=16, articulos=1, poderDeMordida=2}
gargantuar = Zombie { nombreZombie="Gargantuar Hulk Smash Puny God", nivelDeMuerte=30, articulos=2, poderDeMordida=30}

--Punto 2.
especialidad puntosDeVida poderDeAtaque generaSoles |(poderDeAtaque*2)>puntosDeVida = "Atacante" |generaSoles==1 = "Proovedora" |otherwise "Defensiva"
--}

--Punto 1 --

data Planta = Planta {
    nombre::String,puntosDeVida :: Int,
    generaSoles :: Int,
    poderDeAtaque :: Int
}deriving (Show)

data Zombie = Zombie {
    nombreZombie :: String,
    nivelDeMuerte :: Int,
    articulos :: Int,
    poderDeMordida :: Int
}deriving (Show)

peaShooter = Planta {nombre = "Peashooter" , puntosDeVida = 5, generaSoles =0, poderDeAtaque = 2}
repeater = Planta {nombre = "Repeater" , puntosDeVida = 5, generaSoles =0, poderDeAtaque = 4} -- hacer con copia?
sunFlower = Planta {nombre = "Sunflower" , puntosDeVida = 7, generaSoles =1, poderDeAtaque = 0}
nut = Planta {nombre = "Nut" , puntosDeVida = 100, generaSoles =0, poderDeAtaque = 0}

zombieBase = Zombie {nombreZombie = "Zombie", nivelDeMuerte = 6, articulos=0, poderDeMordida=1}
ballonZombie = Zombie {nombreZombie = "Pepe colgado", nivelDeMuerte = 12, articulos=1, poderDeMordida=1}
paperZombie = Zombie {nombreZombie = "Betl el chismoso", nivelDeMuerte = 16, articulos=1, poderDeMordida=2}
gargantuar = Zombie {nombreZombie = "Gargantuar Hulk Smash Puni god", nivelDeMuerte = 30, articulos=2, poderDeMordida=30}