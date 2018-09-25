-- Clase 1
f x y = x * x + y * y

-------------
-- Clase 2 
letra l | l == 'm' = "eme"
        | l == 'n' = "ene"
        | otherwise = "no se"

-- 1 2 3 4 5 6
-- 1 2 3 5 8 13

fibo n | n > 1 = fibo (n-1) + fibo (n-2)
       | otherwise = 1

--

signo :: Float -> Float
signo n | n < 0 = (-1)
        | n > 0 = 1
        | otherwise = 0

absoluto :: Float -> Float
absoluto n = signo(n) * n 

maximo :: Float -> Float -> Float
maximo x y | x > y = x
           | otherwise = y

maximo3 :: Float -> Float -> Float -> Float
maximo3 x y z = maximo (maximo (x) (y)) (z)


repe n | n > 0 = repe (n-1)
       | otherwise = 0

doble :: Num n => n -> n
doble x = 2 * x


cuadruple x = doble (doble x)

--doble :: Char -> Char
--doble x = x:x:[]


esPar :: Integer -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod y x == 0 = True
                 | otherwise = False


identidad :: a -> a
identidad x = x


normaVectorial :: (Float, Float) -> Float
normaVectorial t = sqrt ((fst t)**2 + (snd t)**2)

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir p = (snd p, fst p)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos p1 p2 = normaVectorial((fst p1 - fst p2), (snd p1 - snd p2))

--let divisores = []
--buscaD n m | mod n buscaD n (n-1) == 0 = m:divisores
--		   | otherwise 

-- Ejercicio 32.iii
f1 :: Floating n => n -> (n, n, n)
f1 x = (2*x, x**2, x-7)

-- 32.iv
f2 :: Integer -> Integer
f2 n | esPar n = div n 2
     | otherwise = n+1

-- 33.i
-- f
f3 :: Integer -> Integer
f3 n | mod n 6 == 0 = div (n * n) 2
     | otherwise = 3 * n + 1

-- g
g3 :: (Integer, Integer) -> Integer
g3 p = (fst p) * ((snd p) + 1)

--h 
h :: (Integer, Integer) -> Integer
h p = f3 (g3 p)

-- h hereda las signaturas de las funciones que la componen
-- manteniendo el mismo dominio y codominio
-- aunque es una buena practica tipar las signaturas
----------------------------------------------------

