mcd :: Integer -> Integer -> Integer
mcd a b = mcd' a b

mcd' :: Integer -> Integer -> Integer
mcd' a 0 = a
mcd' a b = mcd' b r
           where r = mod a b

---------------------------
-- funcion de mcd monstruosa

mcdBeta :: Integer -> Integer -> Integer
mcdBeta a b = mcdBeta' a b a

-- supongo a < b
mcdBeta' :: Integer -> Integer -> Integer -> Integer
mcdBeta' 1 _ _ = 1
mcdBeta' _ 1 _ = 1
mcdBeta' _ _ 1 = 1
mcdBeta' a b m | mod maxDivB a == 0 = maxDivB
               | otherwise = mcdBeta' a b maxDivB
               where maxDivB = (mayorDivisorDesde b m)

mayorDivisorDesde :: Integer -> Integer -> Integer
mayorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = mayorDivisorDesde n (k-1)

mayorDivisor :: Integer -> Integer
mayorDivisor n | n == 1 = 1
               | otherwise = mayorDivisorDesde n n-1

-- tercer intento, mas eficiente y claro, consejo del profe
mcdTercero :: Integer -> Integer -> Integer
mcdTercero a b = mcdTercero' a b a

mcdTercero' :: Integer -> Integer -> Integer -> Integer
mcdTercero' a b k | ((mod a k) == 0) && ((mod b k) == 0) = k
                  | otherwise = mcdTercero' a b (k-1)

-----
{- TERMINAR
-- a = p1*p2*p3*...*pn; b = q1*q2*q3*...*qm

mcd4 1 k = 1
mcd4 a b | mod b (minD a) = (minD a) * mcd4 (div a minD) (div b minD)
        where minD = 0

-}

-------

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
--   a b = (g, s, t)
emcd a 0 = (a, 1, 0)
emcd 0 b = (b, 0, 1)
emcd a b = (g, t, s-t*q) 
         where (g,s,t) = emcd b r
               q = div a b
               r = mod a b

--
trd :: (Integer,Integer,Integer) -> Integer
trd (a,b,c) = c

--

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = (mod b (mcd a m)) == 0

-- a === (a:m) (mod m)
-- a*x + m*k = (a:m)
--  x:=s de antes
--  k:=t de antes
-- b=(a:m)*d
--mul por d 
-- d*a === d*(a:m) (mod m)

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m | tieneSolucion a b m = s * div b g
                         | otherwise = 0
                         where (g,s,t) = emcd a m