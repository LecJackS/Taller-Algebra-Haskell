esPar :: Integer -> Bool
esPar n | n == 0 = True
        | n < 2  = False
        | otherwise = esPar (n-2)

--otra. importante, ver el seguimiento de los valores
esPar2 :: Integer -> Bool
esPar2 n | n == 0 = True
         | otherwise = not (esPar (n-1))
-- n = 2 >> 

mul3 :: Integer -> Bool
mul3 n | n == 0 = True
       | n < 3  = False
       | otherwise = mul3 (n-3)

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | otherwise = (2*n-1) + sumaImpares (n-1)
              

-- n*2; ej:3*2=6 --> 6 [5] 4 [3] 2 [1]

