
--Clase 4 Recursion


factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial(n -1)
--Si otherwise halt

esPar :: Integer -> Bool
esPar n 
    | n == 0 = True
    | n == 1 = False
    | n > 1 = esPar(n-2)

fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib(n - 1) + fib(n - 2)


--3)a)

--t = n+1
rec_1 :: Integer -> Integer
rec_1 t 
    | t == 1 = 2
    | t > 1 = 2 * (t-1) * rec_1(t-1) + 2^t * factorial(t-1)


--3)b)

--t = n+2
rec_2 :: Integer -> Integer
rec_2 t
    | t == 1 = 1
    | t == 2 = 2
    | t > 2 = (t-2) * rec_2(t-1)  +  2 * (t-1) * rec_2(t-2)  



rec_3 :: Integer -> Integer
rec_3 n
    | n == 1 = -3
    | n == 2 = 6
    | not (esPar n) = -rec_3(n-1) - 3
    | not (esPar n) = rec_3(n-1) + 2*rec_3(n-2) + 9

--Sumatoria


sumatoria :: Integer -> Integer
sumatoria n
    | n == 1 = 1
    | n > 1 = n + sumatoria(n-1)

f1 :: Integer -> Integer
f1 n
    | n == 0 = 1
    | n > 0 = 2^n + f1(n-1)


f2 :: (Integer, Float) -> Float
f2 (n, q) 
    | n == 1 = q
    | n > 1 = q^n + f2(n-1, q)


f3 :: (Integer, Float) -> Float
f3 (n, q)
    | n == 0 = 0
    | n > 0 = q^(2*n) + f2(2*n-1, q)


f4 :: (Integer, Float) -> Float
f4 (n, q) = f3(n, q) + f2(n, q) - q^n



mul3 :: Integer -> Bool
mul3 n
    | n == 3 = True
    | n < 3 = False
    | n > 3 = mul3 (n-3)


sumaImpares :: Integer -> Integer
sumaImpares n 
    | n == 1 = 1
    | n > 1 = (2*n - 1) + sumaImpares(n-1)


medioFactorial :: Integer -> Integer
medioFactorial n
    | n == 1 = 1
    | n == 2 = 2
    | n > 2 = n * medioFactorial(n - 2)
