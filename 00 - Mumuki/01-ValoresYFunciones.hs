
maximoEntreTres dia1 dia2 dia3 = ((max dia1).(max dia2)) dia3
minimoEntreTres dia1 dia2 dia3 = ((min dia1).(min dia2)) dia3
dispersion dia1 dia2 dia3 = (maximoEntreTres dia1 dia2 dia3) - (minimoEntreTres dia1 dia2 dia3)

-- Ejercicio 15 -- 
diasParejos dia1 dia2 dia3 = (dispersion dia1 dia2 dia3) < 30
diasLocos dia1 dia2 dia3 = (dispersion dia1 dia2 dia3) > 100
diasNormales dia1 dia2 dia3 = not (diasParejos dia1 dia2 dia3) && not (diasLocos dia1 dia2 dia3)

--Ejecicio 16--
metrosACentimetros altura = altura *100
alturaChica alturaPino = min (metrosACentimetros alturaPino) 300
alturaGrande alturaPino = (max (metrosACentimetros alturaPino) 300) - 300
pesoPino alturaPino = (alturaChica alturaPino * 3) + (alturaGrande alturaPino * 2)
esPesoUtil pesoPino = pesoPino > 400 && pesoPino < 1000
--sirvePino alturaPino = (esPesoUtil (pesoPino alturaPino)) 
sirvePino = esPesoUtil . pesoPino  
