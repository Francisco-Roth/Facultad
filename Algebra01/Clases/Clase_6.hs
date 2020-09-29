
--Clase 6

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _       = False


oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _         = True


--Implicacion logica    p -> q  
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _        = True


sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana(n-1)


algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (x, y, z) = x*y*z == 0
--algunoEsCero (0, _, _) = True
--algunoEsCero (_, 0, _) = True
--algunoEsCero (_, _, 0) = True
--algunoEsCero (_, _, _) = False


productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, y1) (x2, y2) = x1*x2 + y1*y2


sumaDigitos :: Integer -> Integer
sumaDigitos n
    | n < 10 = n
    | n >= 10 = unidad + sumaDigitos(div (n - unidad) 10)
    where unidad = mod n 10

sonIguales :: Integer -> Integer -> Bool
sonIguales a b = a == b

--Emprolijar

digitosIguales :: Integer -> Bool
digitosIguales n 
    | n < 10 = True
    | n >= 10 = sonIguales unidad (mod (div (n-unidad) 10) 10) && digitosIguales(div (n-unidad) 10)
    where unidad = mod n 10






