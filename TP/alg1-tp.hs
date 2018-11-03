type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20,(-10)]]

sopa3 :: Tablero
sopa3 = [[10,5,15],[-1,7,2],[2,12,3]]

camino1 :: Camino 
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino 
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,2),(2,2),(3,2)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 

------
-- David
maximo :: Tablero -> Integer 
maximo [[x]] = x
maximo [x,y] = max (maximoDeUnaFila x) (maximoDeUnaFila y)
maximo (x:y:xs) | maximoDeUnaFila x > maximoDeUnaFila y = maximo (x:xs)
                | otherwise = maximo (y:xs) 
--   where maxFilx = maximoDeUnaFila x
--         maxFily = maximoDeUnaFila y


maximoDeUnaFila :: [Integer] -> Integer
maximoDeUnaFila [x] = x
maximoDeUnaFila (x:xs) = max x (maximoDeUnaFila xs) 


--- Jack
{- 
masRepetido :: Tablero -> Integer

--

masRepetido' :: Tablero -> Integer -> Integer
masRepetido' (t:ts) m =  

--[1,2,1,5]
filaATDO [] = []
filaATDO (n:ns) = (n, 1):filaATDO ns

sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]
camino1 = [(1,1),(1,2),(2,2),(2,3)]


-- sopa a Tuplas de Ocurrencias (n, 1)
sopaATDO [] = []
sopaATDO ([]:ts) = filaATDO ts
sopaATDO ((n:ns):ts) = (n, 1):sopaATDO (ns:ts)

uneTuplas ((a,i):(b,j):cs) | a == b = uneTuplas (a, i+j):cs
                           | otherwise = uneTuplas (a, i+j):cs

-}
-- masRepetido
-- devuelve el numero que mas veces aparece en el tablero
-- dependencias: escanearTab, cuentaOcu
-- 
masRepetido :: Tablero -> Integer
masRepetido t = escanearTab t 0 0 []

-- escanea el tablero en comparando las repeticiones de cada uno de sus valores
-- con el siguiente, solo quedandose con el mayor
-- m: valor que mas aparece
-- i: cantidad de apariciones de m 
-- hist: lista elementos ya contados, para no contar 2 veces el mismo
escanearTab :: Tablero -> Integer -> Integer -> [Integer] -> Integer
escanearTab [] m i hist = m
escanearTab ([]:ts) m i hist = escanearTab ts m i hist
escanearTab ((n:ns):ts) m i hist | elem n hist = ignorN
                                 | ocuDeN < i = ignorN
                                 | otherwise  = escanearTab (ns:ts) n ocuDeN (n:hist)
                                 where tabler = ((n:ns):ts)
                                       ocuDeN = cuentaOcu tabler n
                                       ignorN = escanearTab (ns:ts) m i hist
-- cuenta las ocurrencias del valor v en un tablero
cuentaOcu :: Tablero -> Integer -> Integer
cuentaOcu [] v = 0
cuentaOcu ([]:ts) v = cuentaOcu ts v
cuentaOcu ((n:ns):ts) v | v == n = 1 + cuentaOcu (ns:ts) v
                        | otherwise = cuentaOcu (ns:ts) v
----------------------------------------------------------
-- fin de masRepetido 

numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino _ [] = []
numerosDeCamino t (c:cs) = (valor t c):(numerosDeCamino t cs)

--
-- caminoDeFibonacci:
-- compara los primeros de un camino dos elementos y verifica que sumen el tercero
-- si son iguales, 'corre' todo un elemento hacia la derecha y repite
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci _ [] = True
caminoDeFibonacci _ (c:[]) = True
caminoDeFibonacci _ (c1:c2:[]) = True
caminoDeFibonacci t (c1:c2:c3:cs) | (valor t c1 + valor t c2) == (valor t c3) = caminoDeFibonacci t (c2:c3:cs)
                                  | otherwise = False


-- Sebas
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t [] = True
caminoSinRepetidos t (x:xs) | noHayRepeticionesDe t (x:xs) = noHayRepeticionesDe t xs
                            | otherwise = False

noHayRepeticionesDe :: Tablero -> Camino -> Bool
noHayRepeticionesDe t [x] = True
noHayRepeticionesDe t (x1:x2:xs) | (valor t x1) /= (valor t x2) = noHayRepeticionesDe t (x1:xs)
                                 | otherwise = False