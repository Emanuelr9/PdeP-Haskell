import Text.Show.Functions

--TP1
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
articulos :: [String], --creo que habria que listar los accesorios y no solo contarlos, ya que a futuro se pueden agregar a la lista mas accesorios, y con un length alcanza para saber la cantidad
poderDeMordida :: Int
}deriving (Show)

data LineaDeDefensa = LineaDeDefensa { 
plantas ::[Planta], 
zombies :: [Zombie]
} deriving (Show)

peaShooter = Planta {nombre = "PeaShooter" , puntosDeVida = 5, generaSoles =0, poderDeAtaque = 20}
repeater = peaShooter {nombre = "Repeater", poderDeAtaque = 2*(poderDeAtaque peaShooter)} --con copia, si cambia el ataque del peashooter tambien el del repeater.
sunFlower = Planta {nombre = "Sunflower" , puntosDeVida = 7, generaSoles =1, poderDeAtaque = 0}
nut = Planta {nombre = "Nut" , puntosDeVida = 100, generaSoles =0, poderDeAtaque = 0}
cactus = Planta {nombre = "Cactus" , puntosDeVida = 9, generaSoles =0, poderDeAtaque = 0}

zombieBase = Zombie {nombreZombie = "Zombie", nivelDeMuerte = calcularNivelDeMuerte zombieBase, articulos=[], poderDeMordida=1}
balloonZombie = zombieBase {nombreZombie = "Pepe Colgado", nivelDeMuerte = calcularNivelDeMuerte balloonZombie, articulos = ["globo"], poderDeMordida= calcularDañoBalloonZombie balloonZombie}
paperZombie = Zombie {nombreZombie = "Beto el chismoso", nivelDeMuerte = calcularNivelDeMuerte paperZombie, articulos = ["diario"], poderDeMordida= calcularDañoPaperZombie paperZombie}
gargantuar = Zombie {nombreZombie = "Gargantuar Hulk Smash Puny God", nivelDeMuerte = calcularNivelDeMuerte gargantuar, articulos = ["un poste de tendido de cableado","un zombie enano"], poderDeMordida=calcularDañoGargantuar gargantuar}

calcularNivelDeMuerte  = length . nombreZombie  
cantidadDeArticulos = length . articulos

--Punto 2
especialidad :: Planta -> String
esPeligroso :: Zombie -> Bool

--Item a
especialidad (Planta _ puntosDeVida generaSoles poderDeAtaque ) 
   | (poderDeAtaque*2)>puntosDeVida = "Atacante" 
   | generaSoles==1 = "Provedora" 
   | otherwise = "Defensiva"

--Item b
esPeligroso zombie = nivelDeMuerte zombie > 10 || cantidadDeArticulos zombie > 1

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

lineaA = LineaDeDefensa { 
  plantas = [sunFlower, peaShooter], 
  zombies = repeat zombieBase
}

lineaB = LineaDeDefensa { 
  plantas = repeat peaShooter, 
  zombies = [gargantuar, zombieBase, zombieBase]
}

lineaC = LineaDeDefensa { 
  plantas = repeat sunFlower, 
  zombies = [gargantuar, zombieBase, zombieBase]
}

agregarPlantaA :: Planta -> LineaDeDefensa -> [Planta]
agregarZombieA :: Zombie -> LineaDeDefensa -> [Zombie]

--Item a
--Recursividad comentada
--agregarPlantaA planta linea = agregarPlantaALista (plantas linea) planta
--agregarPlantaALista  [] planta = [planta]
--agregarPlantaALista (x:xs) planta = x : agregarPlantaALista xs planta

--agregarPlanta2 planta linea  = reverse $ planta : (reverse . plantas $ linea) --otra forma media rara de agregar al final
agregarPlantaA planta linea  = (plantas linea) ++ [planta]
agregarZombieA zombie linea = (zombies linea) ++ [zombie]

--Item b
ataqueDePlantasEsMenor linea = (totalDeAtaque . plantas $ linea) < (totalDeMordiscos . zombies $ linea)

--Recursividad comentada
--totalDeMordiscos [] = 0
--totalDeMordiscos (x:xs) =  poderDeMordida x + totalDeMordiscos xs
--totalDeAtaque [] = 0
--totalDeAtaque (x:xs) = poderDeAtaque x + totalDeAtaque xs

totalDeMordiscos = foldl sumarMordidas 0
                  where sumarMordidas acumulador = (acumulador + ).poderDeMordida
totalDeAtaque = foldl sumarAtaques 0
                  where sumarAtaques acumulador = (acumulador + ).poderDeAtaque

todosLosZombiesSonPeligrosos linea = hayPeligro . zombies $ linea
--Recursividad comentada
--hayPeligro [] = True
--hayPeligro (x:xs) = esPeligroso x && hayPeligro xs

hayPeligro zombies = foldl (&&) True (map esPeligroso zombies) 

hayZombies linea = (length . zombies $ linea) > 0
estaEnPeligro linea = ataqueDePlantasEsMenor linea  || (todosLosZombiesSonPeligrosos linea && hayZombies linea)

--Item c
necesitaSerDefendida linea = esProvedora . plantas $ linea
--Recursividad comentada
--esProvedora [] = True
--esProvedora (x:xs) = (especialidad x == "Provedora") && esProvedora xs
esProvedora plantas = foldl (&&) True (map obtenerEspecialidad plantas) 
                      where obtenerEspecialidad = ("Provedora" ==).especialidad

--Item d
{--i. Si se consulta con una linea de infinitos zombies el programa se colgara al no poder terminar de recorrer la lista en la funcion hayPeligro, ya que esta es recursiva y su salida solo se realiza con la lista vacia "[]". 
--ii. Al consultar si una línea con cantidad infinita de Peashooter necesita ser defendida, esta daría falso y no necesitaría recorrer toda la lista por concepto de evalación diferida.
  Pero si hablamos de una cantidad infinita de Sunflower, está recorrerá toda la lista sin dar respuesta porque se pide saber si todas las plantas son proveedores, y en este caso todas incluye a infinitos.
--}

--Punto 4
lineaMixta linea = distintaEspecialidad (plantas linea)
 
distintaEspecialidad [] = True
distintaEspecialidad [x] = False
distintaEspecialidad (x:y:ys) = (especialidad x) /= (especialidad y) && distintaEspecialidad ys

--Punto 5
--a.
ataquePlantaA planta zombie 
  | (nombre planta == "Nut") = ataqueNutA zombie
  | (nombre planta == "Cactus") = ataqueCactusA zombie
  | otherwise = ataqueComunPlantaA planta zombie  

quitarleLetrasAlNombre planta zombie = drop (poderDeAtaque planta) (nombreZombie zombie)

---funcion extra, incial del nombre de zombie, para usar en caso de prueba
incialDelZombie zombie = head . nombreZombie $ zombie

--b.
ataqueZombieA zombie planta = planta {puntosDeVida = (puntosDeVida planta - poderDeMordida zombie) }


--TP2

--Punto 1 
--Se realiza modificación en el punto 5 en el ataque de plantas y zombies, y se agregan métodos
ataqueNutA zombie = zombie {poderDeMordida=disminuirMordida (poderDeMordida zombie)}
disminuirMordida poderDeMordida  
  | poderDeMordida > 1 = poderDeMordida - 1
  | otherwise = poderDeMordida

ataqueCactusA zombie = zombie {articulos= quitarArticulo (articulos zombie)}
quitarArticulo = filter (/= "globo") 

ataqueComunPlantaA planta zombie = zombie {nombreZombie=quitarleLetrasAlNombre planta zombie ,nivelDeMuerte = length (quitarleLetrasAlNombre planta zombie)}

--Se diseñan las funciones para determinar su daño a partir de su mordida
calcularDañoBalloonZombie zombie 
  | length ( filter (== "globo") (articulos zombie)) >= 1 = 5
  | otherwise = 2

calcularDañoPaperZombie zombie = length (concatenarAccesorios (articulos zombie)) 
concatenarAccesorios = foldl (++) ""  

calcularDañoGargantuar zombie = 30 + length (articulos zombie)

--Punto 2
--Las correspondientes modificaciones se encuentran en el archivo

--Punto 3
filtrarPlantas plantas [] = plantas
filtrarPlantas plantas  (condicion:condiciones) = filtrarPlantas (condicion plantas) condiciones

--Consulta 1
consulta1 = length (filtrarPlantas (plantas linea1) [filter (("Provedora"==).especialidad)])

--Consulta 2
consulta2 = filtrarPlantas (plantas linea2) [filter ((>4).puntosDeVida),  filter (('P' ==).head.nombre)]

--Consulta3
consulta3 = filtrarPlantas (plantas linea2) [filter ((nombre (head (plantas linea2))==).nombre),  filter (("Provedora"==).especialidad)] 

--Punto 4
data Jardin = Jardin { 
  lineas :: [LineaDeDefensa] 
}deriving Show

miJardin = Jardin {
  lineas = [linea1, linea2, linea3, linea4]
}

--Funciones con potenciadores
aplicarPotenciador jardin [] = jardin
aplicarPotenciador jardin (funcion:funciones) = aplicarPotenciador (funcion jardin) funciones

--Funcion para agregar el artfacto
navidadZombie jardin = jardin {lineas = map aplicarNavidadZombie (lineas jardin)}
aplicarNavidadZombie linea  = linea {zombies = map agregarArtefacto (zombies linea)}

agregarArtefacto zombie = zombie {articulos = (articulos zombie) ++ ["barba"]}

catenaccio jardin = jardin {lineas = map aplicarCatenaccio (lineas jardin)}
aplicarCatenaccio linea = linea {plantas =  agregarPlantaA nut linea }


--Funciones para realizar el riego
aplicarRiego jardin = jardin{ lineas = map aplicarLineaRiego (lineas jardin)}
aplicarLineaRiego linea = linea { plantas= map aumentarVidaPlanta (plantas linea)}

aumentarVidaPlanta planta 
  | head (nombre planta) == 'P' = aumentarVida planta 10
  | especialidad planta == "Defensiva" = aumentarVida planta 5
  | otherwise = aumentarVida planta 0

aumentarVida planta cantidad = planta{puntosDeVida = (puntosDeVida planta + cantidad) }

--Punto 5
--Item a. 
plantaMejorValorada valorar planta1 planta2 
 |valorar planta1 > valorar planta2 = planta1
 |otherwise = planta2 
--Item b.
mvp valorar = foldl1 (plantaMejorValorada valorar)
--Item c: mvp puntosDeVida $ concat.map plantas.lineas $ miJardin

--Punto 6
-- ataqueLinea linea = linea {plantas = (plantas linea), zombies = (zombies linea)}
ataqueLinea linea = ataqueZombieAPlanta (ataquePlantasAZombie linea)

ataqueZombieAPlanta linea 
  | (length (zombies linea) > 0 && nivelDeMuerte (head (zombies linea2)) < 0) = linea {zombies = tail (zombies linea)}
  | length (zombies linea) > 0 = destruirPlanta (ataquePosteriorZombieAPlanta linea)
  | otherwise = linea

ataquePosteriorZombieAPlanta linea = linea {plantas = (reverse (tail (plantas linea))) ++ [ataqueZombieA (head (zombies linea)) (last (plantas linea))]}

destruirPlanta linea 
  | puntosDeVida (last (plantas linea)) < 0 = linea {plantas = reverse (tail (reverse (plantas linea)))} 
  | otherwise = linea

ataquePlantasAZombie linea = linea {zombies =  existeZombie linea}
existeZombie linea 
  | length (zombies linea) == 0 = zombies linea
  | otherwise = [ataqueGrupalAZombie (totalDeAtaque (plantas linea)) (head (zombies linea)) ] ++ tail (zombies linea) 

ataqueGrupalAZombie ataquePlantas zombie = zombie {nivelDeMuerte = (nivelDeMuerte zombie) - ataquePlantas}

