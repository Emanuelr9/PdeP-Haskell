import Test.Hspec
import PlantasVsZombies

testEntrega1 = hspec $ do 

 describe "Tests Punto 1" $ do
 it "El poder de ataque de un PeaShooter debe ser 2, el de un Repeater 4, la cantidad de soles que otorga un Sunflower 1, los soles que da un Nut 0, los articulos que tiene un Ballon Zombie 1, el poder de mordida de un Newspaper Zombie 2 y el nivel de muerte de un Gargantuar 30" $ do
  (ataque peaShooter) `shouldBe` 2
  (ataque repeater) `shouldBe` 4
  (soles sunflower) `shouldBe` 1
  (ataque nut) `shouldBe` 0
  ((length.accesorio) balloonZombie) `shouldBe` 1
  (mordida newspaperZombie) `shouldBe` 2
  (nivelDeMuerte gargantuar) `shouldBe` 30

 describe "Tests Punto 2" $ do
 it "La especialidad de un Nut debe ser Defensiva, la especialidad de un Sunflower debe ser Proveedora, la especialidad de un Repeater debe ser Atacante, un zombie base no es peligroso, un gargantuar es peligroso" $ do
  (especialidad nut) `shouldBe` "Defensiva"
  (especialidad sunflower) `shouldBe` "Proveedora"
  (especialidad repeater) `shouldBe` "Atacante"
  (esPeligroso zombieBase) `shouldBe` False
  (esPeligroso gargantuar) `shouldBe` True

 describe "Tests Punto 3" $ do
 it "Agregar una planta a la linea 1 da un total de 4, agregar un zombie a la linea2 da un total de 3, la linea3 esta en peligro, la linea1 debe ser defendida y la linea 2 no debe ser defendida" $ do
  length(planta(agregarPlanta sunflower linea1)) `shouldBe` 4
  length(zombie(agregarZombie zombieBase linea2)) `shouldBe` 3
  (estaEnPeligro linea3) `shouldBe` True
  (necesitaSerDefendida linea1) `shouldBe` True
  (necesitaSerDefendida linea2) `shouldBe` False

 describe "Tests Punto 4" $ do
 it "la linea 2 no debe ser mixta, la linea 3 debe ser mixta, la linea 4 no debe ser mixta" $ do
  (lineaMixta linea2) `shouldBe` False
  (lineaMixta linea2) `shouldBe` False
  (lineaMixta linea4) `shouldBe` False

