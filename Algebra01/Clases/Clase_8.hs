--Clase 8

type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar n xs
    | elem n xs = xs
    | otherwise = n : xs


incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (x:xs) ys = (elem x ys) && (incluido xs ys) 
    

iguales :: Set Integer -> Set Integer -> Bool
iguales xs ys = (incluido xs ys) && (incluido ys xs)

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
agregarC xs [] = xs : []
agregarC xs (ys:yys)
    | iguales xs ys = ys : yys
    | otherwise     = ys : (agregarC xs yys)

pertenece :: Set Integer -> Set (Set Integer) -> Bool
pertenece xs [] = False
pertenece xs (ys:yys)
    | iguales xs ys = True
    | otherwise = pertenece xs yys

--Eliminar repetidos

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos _ [] = []
agregarATodos n (xs:cls) = (agregar n xs) : (agregarATodos n cls)

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes(n-1) ++ (agregarATodos n (partes (n-1)))


union :: Set Integer -> Set Integer -> Set Integer
union [] ys = ys
union (x:xs) ys
    | elem x ys = union xs ys
    | otherwise = union xs (x:ys)


unionC :: Set (Set Integer) -> Set (Set Integer) -> Set (Set Integer)
unionC [] yys = yys
unionC (xs:xxs) yys
    | pertenece xs yys = unionC xxs yys
    | otherwise        = unionC xxs (xs:yys)


productoCartesianoAux :: Integer -> Set Integer -> Set (Integer, Integer)
productoCartesianoAux x []     = []
productoCartesianoAux x (y:ys) = (x, y) : (productoCartesianoAux x ys)

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano [] _      = []
productoCartesiano (x:xs) ys = (productoCartesianoAux x ys) ++ (productoCartesiano xs ys)
-- o con union

agregarElem :: Integer -> Set [Integer] -> Set [Integer]
agregarElem _ []     = []
agregarElem n (x:xs) = (n:x) : (agregarElem n xs)  

agregarTodos :: Set Integer -> Set [Integer] -> Set [Integer]
agregarTodos (x:[]) ys = (agregarElem x ys)
agregarTodos (x:xs) ys = (agregarElem x ys) ++ (agregarTodos xs ys)

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones xs 0 = [[]]
variaciones xs n = agregarTodos xs (variaciones xs (n-1)) 

--despues del parcial vere