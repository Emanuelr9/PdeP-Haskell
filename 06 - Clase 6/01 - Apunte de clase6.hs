import Text.Show.Functions

{-comienzanCon char = filter ((char ==) . head)
--comienzanCon = filter (\ word -> head word == {-char-}) -- \ significa que es una expresion lamda λ entocnes la primera letra de word (parametro) quiero que sea igual a char
comienzaConVocal = filter (flip elem "aeiou" . head)
comienzaConVocal2 = filter (`elem` "aeiou" . head)
comienzaConVocal3 = filter (\word -> elem (head word) "aeiou")
comienzaConVocal4 ws = concat (map (\ v -> comienzanCon v ws) "aeiou")

empiezanConvocal5 = filter empiezanConvocal
    where empiezanConvocal w = elem (head w) "aeiou"
          empiezanConvocal (h:_) = elem h "aeiou" --la palabra es una lista de caracteres por eso se puede hacer el (h:_)
-}

{-
numerosEntre mn mx = filter (\n -> 30 >= mn && 5 <= mx)

pesoTotal = fold facum 0 
    where facum acum = (tacum).peso

pesoTotal1 = foldl (\acum dino -> acum + peso dino) 0


--torneo de dinos--
nuevoTorneo = [(d1,d2),(d3,d4),(d5,d6),(d7,d8)] son tuplas

torneo1 = map combatirPar

combatirPar (d1.d2) = combatir d1 d2 
-}

pares = [(4,5), (7,9), (1,1)]

-- >filter ((>10).uncurry (+)) pares devuelve la tupla (7,9)


--EJERCICIO FEDEX--

data Envio = Envio {origen :: Lugar , destino :: Lugar , peso :: Int , precio :: Int ,
                    categorias :: [String], impuestos :: [Impuesto]  } deriving (Show)

data Lugar = Lugar {pais :: String , ciudad :: String} deriving (Show)

type Impuesto = Envio -> Int --Es 
--type Cargo = Envio -> Int -- Opcion 1 si lo hago en base a int deberia mapear
type Cargo = Envio -> Envio -- Opcion 2  

cargoTecnologico = cargoCategorico "Tecnologia" 18
cargoCategorico categoria porcentaje envio
    |elem categoria $ categorias envio = aumentarPrecio envio (porcentaje * precio envio `div` 100) --esto como venia con este cambio
    |otherwise = envio
-- |elem categoria $ categorias envio = envio {precio = precio envio + porcentaje * precio envio `div` 100} --esto como venia con este cambio

cargoSobrepeso pesoMaximo envio 
    |pesoMaximo >= peso envio = envio
    |otherwise = envio {precio = precio envio + (peso envio - pesoMaximo)  * 80 }
 
--aumentarPeso envio (max 0 (peso envio - pesomaximo) *80 )
aumentarPrecio envio importe = envio {precio = precio envio + importe}

cargoArbitrario envio = aumentarPrecio envio 50
--cargoArbitrario = flip aumentarPrecio 50 --Opcion point free con flip

envio2B = Envio {
    origen = Lugar {
        pais = "Argentina",
        ciudad = "Buenos Aires"
        },

    destino = Lugar {
        pais = "Paises Bajos",
        ciudad = "Ultrecht"
        },
    peso = 2,
    precio = 1500,
    categorias = ["Musica", "Tecnologia"],
    impuestos = []
}

envio2C = Envio{
    origen = Lugar{
        pais = "Estados Unidos",
        ciudad = "California"
    },
    destino = Lugar {
        pais = "Estados Unidos",
        ciudad = "Miami"
    },
    peso = 5,
    precio = 1500,
    categorias = ["Libros"],
    impuestos = [iva, impuestoExtranio]

}

agregarImpuesto envio impuesto = aumentarPrecio envio (impuesto envio)

iva envio = precio envio * 0.2

impuestoExtranio envio
    |odd precio envio = aumentarPrecio envio (envio*10/100)
    |otherwise = envio