module Clase_2

where 

abso n 
    | n > 0 = n
    | otherwise = (-n)

esPar :: Integer -> Bool
esPar n = (mod n 2) == 0

esImpar :: Integer -> Bool
esImpar n = (mod n 2) == 1

esMultiploDe :: Integer -> Integer -> Bool

esMultiploDe n1 n2 
    | n2 == 0 = False
    | otherwise = (mod n1 n2) == 0




normaVectorial :: (Float, Float) -> Float
normaVectorial p = sqrt((fst p)^2 + (snd p)^2)

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir v = (snd v, fst v)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos v1 v2 = normaVectorial (fst v1 - fst v2, snd v1 - snd v2)




f1 :: Float -> (Float, Float, Float)
f1 x = (2*x, x^2, x-7)

-- f2 :: Integer -> Integer
-- f2 n
--     | esPar n   = div n 2
--     | otherwise = n + 1





-- f :: Float -> Float
-- f n
--     | mod n 6 == 0 = div (n^2) 2
--     | otherwise = 3*n + 1







