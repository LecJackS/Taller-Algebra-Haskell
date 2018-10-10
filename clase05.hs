-- Implementar una funcion recursiva para:
-- f(n) = sum(2*i-1) desde i=1..n
induc :: Integer -> Integer
induc n | n == 1 = 1
        | otherwise = induc (n-1) + (2*n-1)
-- al usar recursion antes que calcular el valor,
-- evito usar memoria en el proceso de recursion?

-- Ejercicios
-- Implementar una funcion eAprox que aproxime e
-- a partir de: sum(1/i!) desde i=0..n
fact :: Integer -> Integer
fact n | n == 1 = 1
       | otherwise = n*fact (n-1)

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = (1/fromInteger(fact(n))) + eAprox (n-1)

-- parteEntera : calcula parte entera de un numero 
parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera (n-1)

-- parteEntera : igual que parteEntera, pero TAMBIEN con negativos 
parteEnteraNega :: Float -> Integer
parteEnteraNega n | n >= 0 && n < 1  = 0
                  | n > 0     =  1 + parteEntera (n-1)
                  | otherwise = -1 + parteEntera (n+1)
                  --malisismo
                  --| otherwise = signo(n)*1 + parteEntera (n+((-1)*signo(n)))
				  --where signo a | a < 0 = (-1)
				   --             | otherwise = 1

partNega2 :: Float -> Integer
partNega2 n | n >= 0 && n < 1  = 0
            | otherwise = signo(n)*1 + parteEntera (n+((-1)*signo(n)))
	        where signo a | a < 0 = (-1)
	                      | otherwise = 1

--5.5
--5.5 >> +1 >> 4.5 >> +1 >> 3.5 >> +1 >> 2.5 >> +1 >> 1.5 >> +1 >> 0.5 >> vuelve y suma

---3.5

---3.5 >> -2.5 -1.5 -0.5
--  -1 + -1 + -1


-- dividiendo = divisor * cociente + resto 
division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d  = 0
             | otherwise = (1 + division ((a-d) d) , 55)

--15 - 3 * 4

---16 / 4
-- 0   4

---5 / 4
-- 3   3

--14 / 4
 --2   3

--13 / 4
-- 1   3

--12 / 4
-- 0   3

--11 / 4
-- 3   2

-- el resto es el mismo
-- el cociente baja 1
