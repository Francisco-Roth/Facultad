--Clase 7 Listas


listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

listar2 :: a -> a -> a -> [a]
listar2 a b c = a : b : c : []

pertenece :: Eq a => a -> [a] -> Bool
pertenece a l
    | length l == 0 = False
    | a == head l = True
    | otherwise = pertenece a (tail l)

pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 a l
    | length l == 0 = False
    | otherwise = (a == head l) || (pertenece a (tail l))



listaDecreciente :: Integer -> [Integer]
listaDecreciente n
    | n == -100 = [-100]
    | n >  -100 = n : listaDecreciente (n - 1)

primerMultiplode45345 :: [Integer] -> Integer
primerMultiplode45345 xs | mod (head xs) 45345 == 0 = head xs
                         | otherwise = primerMultiplode45345 (tail xs)




--SIN PATTERN MATCHING

productoria :: [Integer] -> Integer
productoria xs
    | (length xs) == 0 = 1
    | otherwise = (head xs) * productoria (tail xs)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n xs
    | (length xs) == 0 = [] 
    | otherwise = (head xs) + n : sumarN n (tail xs)


ultimo :: [Integer] -> Integer
ultimo xs
    | (length xs) == 1 = head xs
    | otherwise = ultimo (tail xs)


sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN (head xs) xs

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs


pares :: [Integer] -> [Integer]
pares xs
    | (length xs) == 0 = []
    | mod (head xs) 2 == 0 = (head xs) : (pares(tail xs))
    | otherwise = pares (tail xs)


multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n xs
    | (length xs) == 0 = []
    | mod (head xs) n == 0 = head xs : multiplosDeN n (tail xs)
    | otherwise = multiplosDeN n (tail xs)


quitar :: Integer -> [Integer] -> [Integer]
quitar n xs
    | n == (head xs) = tail xs
    | otherwise = head xs : quitar n (tail xs)

  
hayRepetidos :: [Integer] -> Bool
hayRepetidos xs
    | length xs == 0 = False
    | otherwise      = pertenece (head xs) (tail xs) || hayRepetidos (tail xs)


quitarTodos :: Integer -> [Integer] -> [Integer]
quitarTodos n xs
    | length xs == 0 = [] 
    | n == (head xs) = quitarTodos n (tail xs)
    | otherwise      = head xs : quitarTodos n (tail xs)


eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos xs
    | length xs == 0 = []
    | pertenece (head xs) (tail xs) = head xs : eliminarRepetidos (quitarTodos (head xs) (tail xs))
    | otherwise = head xs : eliminarRepetidos (tail xs)



maximo :: [Integer] -> Integer
maximo xs
    | length xs == 1 = head xs
    | length xs == 2 && (head xs) >= head(tail xs) = head xs
    | length xs == 2 && (head xs)  < head(tail xs) = head(tail xs)
    | head xs   >= head(tail xs)   = maximo(head xs : tail(tail xs)) 
    | otherwise                    = maximo(tail xs)


minimo :: [Integer] -> Integer
minimo xs
    | length xs == 1 = head xs
    | length xs == 2 && (head xs) <= head(tail xs) = head xs
    | length xs == 2 && (head xs)  > head(tail xs) = head(tail xs)
    | head xs   <= head(tail xs)   = minimo(head xs : tail(tail xs)) 
    | otherwise                    = minimo(tail xs)



ordenar :: [Integer] -> [Integer]
ordenar xs
    | length xs == 1 = xs
    | otherwise      = mx : ordenar (quitar mx xs)
    where mx = (minimo xs)


quitarUltimo :: [Integer] -> [Integer]
quitarUltimo xs
    | length xs == 1 = []
    | otherwise      = (head xs) : quitarUltimo (tail xs)


reverso :: [Integer] -> [Integer]
reverso xs
    | length xs == 1 = xs
    | otherwise      = ultimo xs : reverso (quitarUltimo xs)


--CON PATTERN MATCHING

pMproductoria :: [Integer] -> Integer
pMproductoria []     = 1
pMproductoria (x:xs) = x*(pMproductoria xs)

pMsumarN :: Integer -> [Integer] -> [Integer]
pMsumarN n []     = []
pMsumarN n (x:xs) = (x+n) : (pMsumarN n xs)

pMsumarElPrimero :: [Integer] -> [Integer]
pMsumarElPrimero (x:xs) = pMsumarN x (x:xs)

pMsumarElUltimo :: [Integer] -> [Integer]
pMsumarElUltimo (x:xs) = pMsumarN (ultimo xs) (x:xs)

pMpares :: [Integer] -> [Integer]
pMpares [] = []
pMpares (x:xs)
    | mod x 2 == 0 = x : pMpares xs
    | otherwise    = pMpares xs  


pMmultiplosDeN :: Integer -> [Integer] -> [Integer]
pMmultiplosDeN n [] = []
pMmultiplosDeN n (x:xs)
    | mod x n == 0 = x : pMmultiplosDeN n xs
    | otherwise    = pMmultiplosDeN n xs


pMquitar :: Integer -> [Integer] -> [Integer]
pMquitar _ [] = []
pMquitar n (x:xs)
    | n == x    = xs
    | otherwise = x : pMquitar n xs 


pMhayRepetidos :: [Integer] -> Bool
pMhayRepetidos [] = False
pMhayRepetidos (x:xs) = pertenece x xs || pMhayRepetidos xs


pMeliminarRepetidos :: [Integer] -> [Integer]
pMeliminarRepetidos [] = []
pMeliminarRepetidos (x:xs) = x : pMeliminarRepetidos (quitarTodos x xs)


pMmaximo :: [Integer] -> Integer
pMmaximo [x] = x
pMmaximo (x:y:xs)
    | x >= y = pMmaximo (x : xs)
    | x <  y = pMmaximo (y : xs)

pMminimo :: [Integer] -> Integer
pMminimo [x] = x
pMminimo (x:y:xs)
    | x >= y = pMminimo (y : xs)
    | x <  y = pMminimo (x : xs)



pMordenar :: [Integer] -> [Integer]
pMordenar [] = []
pMordenar xs = minimo xs : pMordenar (pMquitar (pMminimo xs) xs)


pMreverso :: [Integer] -> [Integer]
pMreverso [] = []
pMreverso (x:xs) = pMreverso xs ++ [x] 