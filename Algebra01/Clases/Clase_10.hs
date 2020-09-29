
--Clase 10
--Polinomios y complejos

type Polinomio = [Float]


grado :: Polinomio -> Integer
grado (x:xs) = fromIntegral (length (x:xs) - 1) 

evaluar :: Polinomio -> Float -> Float
evaluar [] _   = 0
evaluar (p:ps) x = (x^(grado (p:ps))) * p + evaluar ps x


derivada :: Polinomio -> Polinomio
derivada []   = []
derivada [x0] = []
derivada (p:ps) = p*gr : (derivada ps) 
    where gr = fromInteger(grado (p:ps))

derivadaN :: Integer -> Polinomio -> Polinomio
derivadaN 0 p = p
derivadaN n p = derivada (derivadaN (n-1) p)

rotar :: Polinomio -> Polinomio
rotar [] = []
rotar (x:xs) = rotar(xs) ++ [x]

sumaAux :: Polinomio -> Polinomio -> Polinomio
sumaAux p1 []         = p1 
sumaAux [] p2         = p2 
sumaAux (x:xs) (y:ys) = (sumaAux xs ys) ++ [x+y]

limpiar :: Polinomio -> Polinomio
limpiar [] = []
limpiar (x:xs)
    | x == 0 = limpiar xs
    | x /= 0 = (x:xs)

suma :: Polinomio -> Polinomio -> Polinomio
suma p1 p2 = limpiar (sumaAux (rotar p1) (rotar p2))

--Viendo con grado mayor, menos cosas

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar _ []     = []
productoPorEscalar 0 _      = []
productoPorEscalar k [x]    = [k*x]
productoPorEscalar k (x:xs) = k*x : (productoPorEscalar k xs)

listaNCeros :: Integer -> Polinomio
listaNCeros 0 = []
listaNCeros i = 0 : listaNCeros (i-1)

productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (0, _) _ = []
productoPorMonomio _ []     = []
productoPorMonomio (a, i) p = (productoPorEscalar a p) ++ (listaNCeros i)


producto :: Polinomio -> Polinomio -> Polinomio
producto [] _ = []
producto _ [] = []
producto (x:xs) y = suma (productoPorMonomio (x, grado (x:xs)) y) (producto xs y)




type Complejo = (Float, Float)


sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (a, b) (c, d) = (a+c, b+d)

productoComplejo :: Complejo -> Complejo -> Complejo
productoComplejo (a, b) (c, d) = (a*c - b*d, a*d + b*c)

potenciaCompleja :: Complejo -> Integer -> Complejo
potenciaCompleja _ 0 = (1, 0)
potenciaCompleja z n = productoComplejo z (potenciaCompleja z (n-1))

