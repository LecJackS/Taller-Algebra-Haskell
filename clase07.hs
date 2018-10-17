-- Tipar y evaluar las siguientes expresiones:

--head [(1,2), (3,4), (5,2)]
-- >> (1,2) > tupla de integers : (Integer, Integer)

-- tail [1,2,3,4,4,3,2,1] 
-- >> [2,3,4,4,3,2,1] >> lista de integers : [Integer]

-- head []
-- el tipo es a, por mas que evaluarlo tire error.
-- error porque no hay elementos para extraer, no puede devolver un "Integer vacio"

-- head [1,2,3] : [2,3]
-- head [1,2,3] devuelve 1, luego:
-- 1 : [2,3]
-- [1,2,3] lista de integer : [Integer]

-- [True, True] ++ [False, False]
-- [True, True, False, False] >> lista de Bool : [Bool]

-- [1,2] : []
-- [[1,2]] >> lista de lista de integer : [[Integer]]

-- otros ejemplos:
-- length [3,2,1,0] : [3,2,1,0]

-- tail (tail (tail [3,2,1]))
-- tail (tail [2,1])
-- tail [1]
-- []

-- tail [True]

-- tail [1] == tail [True]

-- head (head (tail [[[1]],[[2]],[[3]]]))
-- head (head [ [[2]], [[3]] ])
-- head [[2]]
-- [2]

-- Ejercicios:
-- Definir la funcion listar :: a -> a -> a -> [a]
-- que toma 3 elementos y los convierte
-- en una lista.

listar :: a -> a -> a -> [a]
listar a b c = [a,b,c]

-- porque si:
otroListar :: a -> a -> a -> [a]
otroListar a b c = a:b:c:[]

otroMas :: a -> a -> a -> [a]
otroMas a b c = (a:[])++(b:[])++(c:[])

-- Escribir una expresion que denote la lista estrictamente
-- decreciente de enteros que comience con 1 y
-- termine con -100
expre :: [Integer]
expre = [1,0..(-100)]

-- otro porque si:
expre2 :: [Float]
expre2 = [0,pi..5*pi]
-- lista de n*pi con n=[0..5]


-- Ejercicio: pertenece con Pattern Matching
pertenecePM :: Integer -> [Integer] -> Bool
pertenecePM _ [] = False
pertenecePM b (x:xs) = (b == x) || (pertenecePM b xs)

--
productoria :: [Integer] -> Integer 
productoria [] = 1
productoria (x:xs) = x * productoria xs

--
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (x+n):(sumarN n xs)

--
-- funcion aux que devuelve el ultimo elemento de una lista de integers
elUltimo :: [Integer] -> Integer
elUltimo [x] = x
elUltimo (x:xs) = elUltimo xs

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN ultimo xs
                   where ultimo = elUltimo xs

--

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN primero xs
                    where primero = head xs

--
pares :: [Integer] -> [Integer]
pares (x:xs) | (length xs == 0) && ((mod x 2) == 0) = [x]
             | (length xs == 0) = []
             | (mod x 2) == 0 = x:(pares xs)
             | otherwise = pares xs

-- mas lindo y como la gente
pares' :: [Integer] -> [Integer]
pares' [] = []
pares' (x:xs) | mod x 2 == 0 = x:(pares' xs)
              | otherwise = pares' xs

--
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x:(multiplosDeN n xs)
                      | otherwise = multiplosDeN n xs

-- quita primera aparicion de n
quitar :: Integer -> [Integer] -> [Integer]
quitar _ [] = []
quitar n (x:xs) | x == n = xs
                | otherwise = x:(quitar n xs)

--yapa quita todos n
quitarTodos :: Integer -> [Integer] -> [Integer]
quitarTodos _ [] = []
quitarTodos n (x:xs) | x == n = quitarTodos n xs
                     | otherwise = x:(quitarTodos n xs)

-- 
-- primeroRepetido: compara un numero n con los elementos de una lista 
primeroRepetido :: Integer -> [Integer] -> Bool
primeroRepetido _ [] = False
primeroRepetido n (x:xs) | n == x = True
                         | otherwise = primeroRepetido n xs

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | primeroRepetido x xs = True
                    | otherwise = hayRepetidos xs
---
eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | hayRepetidos (x:xs) == False = (x:xs)
                         | primeroRepetido x xs == True = eliminarRepetidos xs
                         | otherwise = x:eliminarRepetidos xs
----
maxAux :: Integer -> [Integer] -> Integer
maxAux m [] = m 
maxAux m (x:xs) | m >= x = maxAux m xs
                | otherwise = maxAux x xs 

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) = maxAux x xs

----
-- chequea si ordenados
ordenados :: [Integer] -> Bool
ordenados [x] = True
ordenados (x:xs) | x > head(xs) = False
                 | otherwise = ordenados xs
-- compara de a dos de izq a derecha, y mueve el mayor hacia la derecha
ordenarAux :: Integer -> [Integer] -> [Integer]
ordenarAux n [] = [n]
ordenarAux n (x:xs) | n <= x = n:ordenarAux x xs
                    | otherwise = x:ordenarAux n xs
-- hace pasadas de ordenarAux hasta que la lista este ordenada
ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar (x:xs) | ordenados (x:xs) == True = (x:xs)
               | otherwise = ordenar (ordenarAux x xs)



