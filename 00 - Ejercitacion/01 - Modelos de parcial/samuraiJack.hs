import Text.Show.Functions

data Elemento = Elemento { 
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje) 
}deriving (Show)
                  
data Personaje = Personaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}deriving (Show)

ariel = Personaje {nombre = "Ariel" , salud = 100, elementos = [], anioPresente = 2019}
pepe = Personaje {nombre = "Ariel" , salud = 100, elementos = [], anioPresente = 2019}

mandarAlAnio año personaje = personaje {anioPresente = año}
meditar personaje = agregarSalud (salud personaje * 1.5) personaje
causarDanio cantidad personaje
    | salud (agregarSalud (cantidad * (-1)) personaje) > 0 = agregarSalud (cantidad * (-1)) personaje
    | otherwise = agregarSalud (salud personaje * (-1)) personaje

agregarSalud valor personaje = personaje {salud = salud personaje + valor}

--2
--a 
esMalvado personaje = any (=="Maldad") (map tipo (elementos personaje))

--b
danioQueProduce:: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = (salud personaje) - salud (ataque elemento $ personaje)

--c
enemigosMortales:: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (puedeVencer personaje) enemigos
puedeVencer personaje enemigo = any (0==)  (map (danioQueProduce personaje) (elementos enemigo))

--3
aku :: Int -> Float -> Personaje
sinTransformar personaje = personaje 

meditacion cantidad = Elemento {tipo = "Magia", defensa = concentracion cantidad, ataque = sinTransformar}
katanaMagica = Elemento {tipo = "Magia", defensa = sinTransformar, ataque = causarDanio 1000}
portalFuturo año = Elemento {tipo = "Magia", defensa = generarAku, ataque = mandarAlAnio (año + 2800)}
 
esbirro = Elemento {tipo = "Maldad", defensa = sinTransformar, ataque = causarDanio 1}

--a
concentracion :: Int -> Personaje -> Personaje
concentracion cantidad personaje = aplicarMeditacion personaje (take cantidad (repeat meditar))

aplicarMeditacion personaje [] = personaje
aplicarMeditacion personaje (meditacion:meditaciones) = aplicarMeditacion (meditacion $ personaje) meditaciones 

--b
esbirrosMalvados cantidad = take cantidad (repeat esbirro)

--c
jack = Personaje {nombre = "Jack" , salud = 300, elementos = [ meditacion 3, katanaMagica], anioPresente = 200}

--d
generarAku personaje = aku (anioPresente personaje) (salud personaje)

aku año cantSalud = Personaje {nombre = "Aku", salud=cantSalud, elementos = [meditacion 4, portalFuturo año] ++ esbirrosMalvados (1 * año), anioPresente = año}--Cambiar a 100

--4
luchar :: Personaje -> Personaje -> (Personaje, Personaje)

luchar atacante defensor 
    | not (puedeVencer (aplicarDefensa defensor) atacante) = luchar (aplicarAtaque atacante (aplicarDefensa defensor)) atacante 
    | otherwise = (aplicarAtaque atacante (aplicarDefensa defensor), atacante)

aplicarDefensa personaje = aplicandoElementos personaje (map defensa (elementos personaje))
aplicarAtaque atacante defensor = aplicandoElementos defensor (map ataque (elementos atacante))


aplicandoElementos personaje [] = personaje
aplicandoElementos personaje (funcion:funciones) = aplicandoElementos (funcion personaje) funciones