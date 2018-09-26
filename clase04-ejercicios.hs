-- Implementar la funcion es Par::Integer -> Bool que determine si un numero
-- natural espar.  No esta permitido utilizar mod ni div .
esPar :: Integer -> Bool
esPar n | n == 0 = True
        | n < 2  = False
        | otherwise = esPar (n-2)

--otra. importante, ver el seguimiento de los valores
esPar2 :: Integer -> Bool
esPar2 n | n == 0 = True
         | otherwise = not (esPar (n-1))
-- n = 2 >> 

-- Escribir una funci ́on para determinar si un n ́umero natural es
-- m ́ultiplo de 3.  No estapermitido utilizarmodnidiv.
mul3 :: Integer -> Bool
mul3 n | n == 0 = True
       | n < 3  = False
       | otherwise = mul3 (n-3)

-- Implementar la funcion sumaImpares :: Integer -> Integer que dado n∈N sume
-- los primeros n numeros impares.  Ej: sumaImpares 3 --> 1+3+5 --> 9.
sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | otherwise = (2*n-1) + sumaImpares (n-1)
              
-- n*2; ej:3*2=6 --> 6 [5] 4 [3] 2 [1]


--Escribir una funci ́on dobleFact que dado n∈N calcula n!! = n(n−2)(n−4)...
dobleFact :: Integer -> Integer
dobleFact n | n <= 0 = 1 
            | otherwise = n * (dobleFact (n-2))
-- si en dobleFact uso 'n == 0 = 1' como en el 'fact' de antes, con los impares
-- entro en un loop infinito por llegar a '-1' en el ultimo llamado recursivo
-- 5-2 >> 3-2 >> 1-2 >> -1 != 1 >> -1-2 >> -3-2 >> ...

--Escribir una funci ́on recursiva que no termine si se la ejecuta con enteros
-- negativos (y en cambio s ́ı termine para el resto de los enteros).
-- why...
soloPositivos :: Integer -> Integer
soloPositivos n | n == 0 = 0
                | otherwise = (-1)*n + soloPositivos (n-1)
-- si n < 0, entrara en otherwise por siempre.
-- si n >=0, termina devolviendo la suma de los valores de 0 a n con signo opuesto
