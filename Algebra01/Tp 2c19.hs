--Integrantes: Francisco Roth 
--	       Sebastian Puglisi

type Circulo = [Integer]

rotarCirculo :: Circulo -> Circulo --aplica una rotacion
rotarCirculo (x:xs) = xs ++ [x]

sonCirculosIgualesAux :: Circulo -> Circulo -> Bool --compara que el head y el tail de los circulos sean iguales, sino aplica una rotacion
sonCirculosIgualesAux circulo1 circulo2
    | head circulo1 == head circulo2 = circulo1 == circulo2
    | otherwise = sonCirculosIgualesAux circulo1 (rotarCirculo circulo2)

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales circulo1 circulo2
    | length circulo1 /= length circulo2 = False
    | otherwise = sonCirculosIgualesAux circulo1 circulo2
----
insertarEnTodos :: Integer -> [[Integer]] -> [[Integer]] --Devuelve listas, cada una con un numero n insertado en posiciones diferentes
insertarEnTodos n [x]    = insertarEnTodosAux n (fromIntegral(length x) + 1) x
insertarEnTodos n (x:xs) = insertarEnTodosAux n (fromIntegral(length x) + 1) x ++ (insertarEnTodos n xs)

insertarEnTodosAux :: Integer -> Integer -> [Integer] -> [[Integer]] --Devuelve listas con un numero n en el lugar k, k-1,k-2..de cada lista
insertarEnTodosAux n 1 xs = [n:xs]
insertarEnTodosAux n k xs = (insertarEn xs n k) : (insertarEnTodosAux n (k-1) xs)

insertarEn :: [Integer] -> Integer -> Integer -> [Integer] --Devuelve una lista con un numero n en una posicion k.
insertarEn xs n 1 = (n:xs)
insertarEn (x:xs) n k = x : (insertarEn xs n (k-1))

permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = insertarEnTodos n (permutaciones (n-1))
----

esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux n 2 = not ((mod n 2) == 0)
esPrimoAux n k 
    | mod n k == 0 = False
    | otherwise    = esPrimoAux n (k-1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo n = esPrimoAux n (n-1) 

ult :: [Integer] -> Integer --ultimo elemento de una lista
ult [x] = x
ult (x:xs) = ult xs

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo (x:xs) = esPrimo (x + ultimo) && esCirculoPrimoAux (x:xs)
                       where ultimo = ult (x:xs)

esCirculoPrimoAux :: Circulo -> Bool --chequea si dos pares adyacentes suman un primo
esCirculoPrimoAux [a,b]    = esPrimo (a+b)
esCirculoPrimoAux (x:y:xs) = esPrimo (x+y) && esCirculoPrimoAux (y:xs)
-----
estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero [x]      = False
estaRepetidoPrimero (x:y:xs) = (sonCirculosIguales x y) || estaRepetidoPrimero (x:xs)
-----------
quitarCirculoRepetido :: [Circulo] -> [Circulo] --quita los circulos repetidos porque permutaciones devuelve repetidos
quitarCirculoRepetido [x] = [x]        
quitarCirculoRepetido (x:xs)
    | estaRepetidoPrimero (x:xs) = quitarCirculoRepetido xs
    | otherwise                  = x : quitarCirculoRepetido xs

listarCirculosPrimosAux :: [Circulo] -> [Circulo] --Devuelve todos los circulos que son primos de la lista de circulos
listarCirculosPrimosAux [] = []
listarCirculosPrimosAux (x:xs) 
    | esCirculoPrimo x = x : (listarCirculosPrimosAux xs)
    | otherwise = listarCirculosPrimosAux xs

listarCirculosPrimos :: Integer -> [Circulo]
listarCirculosPrimos n = listarCirculosPrimosAux (quitarCirculoRepetido (permutaciones n))
--------
contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = fromIntegral(length (listarCirculosPrimos n))

------------
--optativo
--Esta funcion invierte una lista
--Si la lista es de la forma [a1,a2,...,an-1,an]
--Devuelve la lista [an,an-1,...,a2,a1]
--Agregando recursivamente el primer elemento al final
invertirCirculo :: Circulo -> Circulo
invertirCirculo []     = []
invertirCirculo (x:xs) = (invertirCirculo xs) ++ [x]


--Esta funcion filtra los circulos que no tienen a su espejado
--Invirtiendo el primero y viendo si este se repite, si se repite se agrega a si mismo
--y a su inverso, si no, no se agrega y se continua con la recursion
eliminarCirculosNoEspejados :: [Circulo] -> [Circulo]
eliminarCirculosNoEspejados [] = []
eliminarCirculosNoEspejados (x:xs)
    | estaRepetidoPrimero ((invertirCirculo x):xs) = (x) : (invertirCirculo x) : (eliminarCirculosNoEspejados xs)
    | otherwise                                     = eliminarCirculosNoEspejados xs


--Esta funcion toma una lista de circulos primos de orden n y la filtra con la funcion
--eliminarCirculosNoEspejados
listaCirculosPrimosEspejados :: Integer -> [Circulo]
listaCirculosPrimosEspejados n = eliminarCirculosNoEspejados (listarCirculosPrimos n)

------------
