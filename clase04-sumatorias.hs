-- Ejercicios: otras sumatorias (19/24)
-- 1:
f1 :: Integer -> Integer
f1 n | n == 0 = 1
     | otherwise = 2^n + f1 (n-1)
-- 2**n + 2**(n-1) + 2**(n-2) + ... + 2**0

-- 2:
f2 :: Integer -> Float -> Float
f2 n q | n == 1 = q
       | otherwise = q^n + (f2 (n-1) q)

-- 3:
f3 :: Integer -> Float -> Float
f3 n q | n == 0 = 1
       | otherwise = q^(2*n) + q^(2*n-1) + (f3 (n-1) q)
-- q**2n + q**((2n)-1) + q**((2n)-2) + q**((2n)-2) + ... + q**(2) + q**(1)
-- q**((2n)-1) == q**((2n)-1)
-- el profe hizo | n == 0 = q^1 + q^2 porque comenzaba desde i=1

f3BienPiola :: Integer -> Float -> Float
f3BienPiola = f2 (2*n) q
 
-- 4:
f4 :: Integer -> Float -> Float
f4 n q = f3 n q - f2 n q

-- q**2n + q**2(n-1) + ... + q**(n+1) + q**n