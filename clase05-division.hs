-- dividiendo = divisor * cociente + resto 
division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d  = (0, a)
             | otherwise = (1 + fst(cocres), snd(cocres))  
             where cocres = division (a-d) d

-- extender bla bla

-- TAREA

-- suma divisores hasta
sumDivHasta :: Integer -> Integer -> Integer
sumDivHasta n k | k == 1 = 1
                | mod n k == 0 = k + recur 
                | otherwise = recur 
                where recur = sumDivHasta n (k-1) 

-- estaria piola graficar el arbol de recursion y ver 
-- como vuelve por un solo camino una vez llegado al caso base
-- ... creo.

sumDiv :: Integer -> Integer
sumDiv n = sumDivHasta n n


-- Ejercicios
-- 1. menorDivisor
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n-k == 0 = k
                      | mod n k /= 0 = menorDivisorDesde n (k+1)
                      | otherwise = k
-- otra forma mas prolija
-- 1. menorDivisor
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

-- 2.
--- es primo
esPrimo :: Integer -> Bool
esPrimo n = n == (menorDivisor n)

-- podria haber usado sumaDivisores tambien, siempre que de ==1+n
-- 3- 
expoFunDesde :: Integer -> Integer -> Integer -> Integer -> Integer
expoFunDesde n m i j | i == 1 = 1
			         | j == 1 = 1
				     | otherwise = i^j + expoFunDesde n m i (j-1)



expoFun1 :: Float -> Float
expoFun1 n m | i == 1 = 1
             | j == 1 = 1
             | otherwise = i^j + expoFun1 n (m-1)

expoFun2 :: Float -> Float
expoFun2 n m | i == 1 = 1
             | j == 1 = 1
             | otherwise = i^j + expoFun2 n (m-1)