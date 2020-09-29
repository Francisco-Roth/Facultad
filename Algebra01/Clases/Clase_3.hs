--Mecanismo de reduccion
module Clase_3
where
import Clase_2

unidades :: Integer -> Integer
unidades n = mod (abso n) 10


sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 n1 n2 n3 = unidades n1 + unidades n2 + unidades n3


todosImpares :: Integer -> Integer -> Integer -> Bool

-- todosImpares i1 i2 i3 = (esImpar i1) && (esImpar i2) && (esImpar i3)

todosImpares i1 i2 i3 = esImpar (i1*i2*i3)


alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar i1 i2 i3 = (esImpar i1) || (esImpar i2) || (esImpar i3)

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares i1 i2 i3 = ((esImpar i1) && (esImpar i2)) || ((esImpar i1) && (esImpar i3)) || ((esImpar i2) && (esImpar i3))

alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares i1 i2 i3 = not (alMenosDosImpares i1 i2 i3)

--7
r1 :: Integer -> Integer -> Bool
r1 a b = esPar a /= esImpar b 

r2 :: Integer -> Integer -> Bool
r2 a b = mod (2*a + 3*b) 5 == 0

r3 :: Integer -> Integer -> Bool
r3 a b = (unidades a /= unidades b) && (unidades (a*b) /= unidades a) && (unidades (a*b) /= unidades b)

--8
xRy :: Float -> Float -> Bool
xRy x y = (x < 3 && y < 3) || (x >= 3 && y >= 3)

--9
yRz :: Float -> Float -> Bool
yRz y z = (y < 3 && z < 3) || (y >= 3 && z >= 3 && y < 7 && z < 7) || (y >= 7 && z >= 7)


--10
f10 :: (Integer, Integer) -> (Integer, Integer) -> Bool
f10 p1 p2 
    | p1 /= (0, 0) && p2 /= (0, 0) = (fst p1) * (snd p2) == (snd p1) * (fst p2)


