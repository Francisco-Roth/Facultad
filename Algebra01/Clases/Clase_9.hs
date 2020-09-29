--Clase 9

--Algoritmo de euclides 
mcd :: Integer -> Integer -> Integer
mcd a b
    | b == 0 = a
    | otherwise = mcd b (mod a b)

--Restando 1 

--Toma b < a, podría no y hacer pasos de mas
mcdRestandoAux :: Integer -> Integer -> Integer -> Integer
mcdRestandoAux a b c
    | (mod a c == 0) && (mod b c == 0) = c
    | otherwise                        = mcdRestandoAux a b (c-1)

mayorDivisorComun :: Integer -> Integer -> Integer
mayorDivisorComun a b 
    | b <  a  = mcdRestandoAux a b b
    | b >= a = mcdRestandoAux b a a

--Factorizando en primos

menAux :: Integer -> Integer -> Integer
menAux a b
    | a == b = a
    | mod a b == 0 = b
    | otherwise    = menAux a (b+1)

menorDivisor :: Integer -> Integer
menorDivisor a = menAux a 2

mcdPrimos :: Integer -> Integer -> Integer
mcdPrimos a 1 = 1
mcdPrimos 1 b = 1
mcdPrimos a b
    | menorDivisor a == menorDivisor b = mcdPrimos (div a (menorDivisor a)) (div b (menorDivisor b))
    | menorDivisor a > menorDivisor b  = mcdPrimos a (div b (menorDivisor b))
    | menorDivisor a < menorDivisor b  = mcdPrimos (div a (menorDivisor a)) b

fst3 :: (a, a, a) -> a
fst3 (a, b, c) = a

snd3 :: (a, a, a) -> a
snd3 (a, b, c) = b

trd3 :: (a, a, a) -> a
trd3 (a, b, c) = c

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g, s, t)
         where (g, sp, tp) = emcd b (mod a b) 
               q = div a b
               s = tp
               t = sp - tp*q


tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = mod b (mcd a m) == 0


--Tambien se podia coprimir 
solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m
    | (tieneSolucion a b m) = s * (div b g)
                            where (g, s, t) = emcd a m



--Debe ser con recursion ?¡?
solucionGeneralAux :: Integer -> Integer -> Integer -> Integer
solucionGeneralAux a b m
    | mcd a b == 1 = m
    | otherwise    = solucionGeneralAux (div a c) (div b c) (div m c)
                   where c = mcd a m

solucionGeneral :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionGeneral a b m
    | tieneSolucion a b m = (solucionParticular a b m, solucionGeneralAux a b m)