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

-- Da la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Da la cantidad columnas de un tablero.
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
-- compara los primeros dos elementos de un camino y verifica que sumen el tercero
-- si son iguales, 'corre' todo un elemento hacia la derecha y repite
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci _ [] = True
caminoDeFibonacci _ (c:[]) = True
caminoDeFibonacci _ (c1:c2:[]) = True
caminoDeFibonacci t (c1:c2:c3:cs) | (valor t c1 + valor t c2) == (valor t c3) = caminoDeFibonacci t (c2:c3:cs)
                                  | otherwise = False

---
-- Sebas
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t [] = True
caminoSinRepetidos t (x:xs) | noHayRepeticionesDe t (x:xs) = noHayRepeticionesDe t xs
                            | otherwise = False

noHayRepeticionesDe :: Tablero -> Camino -> Bool
noHayRepeticionesDe t [x] = True
noHayRepeticionesDe t (x1:x2:xs) | (valor t x1) /= (valor t x2) = noHayRepeticionesDe t (x1:xs)
                                 | otherwise = False

-- David
{-
unCaminoDeFibonacci:: Tablero -> Posicion -> Camino
unCaminoDeFibonacci x p | valor p + valor (siguienteDerecha x p) == valor = []  
                         where sig2Der = siguienteDerecha x (siguienteDerecha x p)                       
siguienteDerecha :: Tablero -> Posicion -> Posicion
siguienteDerecha (t:ts) (p1,p2) | pertenece (t:ts) (p1+1,p2) = (p1+1,p2)
                                | otherwise = (0,0)
   
siguienteAbajo :: Tablero -> Posicion -> Posicion 
siguienteAbajo (t:ts) (p1,p2) | pertenece (t:ts) (p1,p2+1) = (p1,p2+1)
                              | otherwise = (0,0)
-}
pertenece :: Tablero -> Posicion -> Bool
pertenece  (t:ts) (x,y) = x <= fromIntegral(length t) && y <= fromIntegral(length (t:ts))
--------------------
{-
secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t k = secue t k (creaMapa row col 1 1)
                                     where row = cantidadFilas t
                                           col = cantidadColumnas t
-}

-- (m:ms) := mapa
{-
secue :: Tablero -> Integer -> [Posicion] -> Conjunto [Posicion]
secue t 0 _ = []
secue t _ [] = []
secue t 1 (m:ms) = [m]:(secue t 1 ms)
secue t 2 (m:ms) = creaCamiAdj t p (getAdj t m)

                 where valorPos = valor t m -- no lo uso, pero es lo que iria en los elementos de la lista
-}
--auxTemp = 

---------------------------------------------------
--  devuelve la secuencia fibonacci de mayor cantidad de elementos en t
mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci t = buscaMayorFib t 3

-- toda secuencia de 2 elementos es secuencia fibonacci, por lo que
-- podemos ignorar los primeros 2 k y comenzar con k=3
buscaMayorFib :: Tablero -> Integer -> [Integer]
buscaMayorFib t k | (secuenciasDeFibonacciDeLongitudK t k) == [] = secueKAnt
                  | otherwise = buscaMayorFib t (k+1)
                  where secueKAnt = head (secuenciasDeFibonacciDeLongitudK t (k-1))
---------------------------------------------------
---------------------------------------------------
secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t k = borraDuplicados (secueFiboKConRepes t k)

-- borra duplicados de la lista de lista de valores
-- para tener un conjunto (sin repetidos) y no una lista
borraDuplicados :: [[Integer]] -> Conjunto [Integer]
borraDuplicados [] = [] 
borraDuplicados (v:vs) = v:borraDuplicados(borraOcurrencias v vs)

-- borra las ocurrencias de una lista l en una lista de listas (v:vs)
borraOcurrencias :: [Integer] -> [[Integer]] -> [[Integer]]
borraOcurrencias _ [] = []
borraOcurrencias l (v:vs) | l == v = borraOcurrencias l vs
                          | otherwise = v:borraOcurrencias l vs

-- crea una secuencia de fibonacci, solo que con repeticiones, que
-- vienen de caminos diferentes
secueFiboKConRepes :: Tablero -> Integer -> [[Integer]]
secueFiboKConRepes t k = numerosDeConjunto t (crearSecuenciasK t k)

-- convierte un conjunto de caminos a un conjunto de numeros
numerosDeConjunto :: Tablero -> Conjunto Camino -> [[Integer]]
numerosDeConjunto _ [] = []
numerosDeConjunto t (c:cs) = (numerosDeCamino t c):(numerosDeConjunto t cs)

-- crea caminos de fibonacci de largo k
crearSecuenciasK :: Tablero -> Integer -> Conjunto Camino
crearSecuenciasK _ 0 = []
crearSecuenciasK t 1 = caminosIniciales t
crearSecuenciasK t k = expandCaminos t (crearSecuenciasK t (k-1))

-- devuelve los caminos 'iniciales' de un solo elemento cada uno
-- o sea, cada posicion del mapa en un conjunto de caminos de un elemento
caminosIniciales :: Tablero -> Conjunto Camino 
caminosIniciales t = mapaToCami (creaMapa row col 1 1)
                     where row = cantidadFilas t
                           col = cantidadColumnas t
-- recibe la lista de todas las posiciones del tablero, y las convierte en
-- un conjunto de caminos
mapaToCami :: [Posicion] -> Conjunto Camino 
mapaToCami [] = []
mapaToCami (m:ms) = [m]:(mapaToCami ms)
-- recibe conjunto de caminos, y devuelve un conjunto con TODOS 
-- los nuevos caminos posibles, que sean fibonacci (checkea en getCaminos)
expandCaminos :: Tablero -> Conjunto Camino -> Conjunto Camino 
expandCaminos _ [] = []
expandCaminos t (cami:cs) = nuevosCami++(expandCaminos t cs)
                            where lastPos = lastPosCami cami
                                  nuevosCami = getCaminos t cami (getAdj t lastPos)

--devuelve la ultima posicion de un camino
lastPosCami :: Camino -> Posicion
lastPosCami (c:[]) = c
lastPosCami (c:cs)   = lastPosCami cs
-- recibe un camino y una lista de nuevos destinos adjacentes,
-- y devuelve la lista de los nuevos caminos posibles, SOLO SI
-- el nuevo camino es un camino de fibonacci
getCaminos :: Tablero -> Camino -> [Posicion] -> Conjunto Camino
getCaminos _ _ [] = [] 
getCaminos t cami (a:adjs) | caminoDeFibonacci t nuevoCami = nuevoCami:(getCaminos t cami adjs)
                           | otherwise = getCaminos t cami adjs
                            where nuevoCami = cami ++ [a]

-- Devuelve los casilleros (validos) adjacentes a una posicion en un mapa

getAdj :: Tablero -> Posicion -> [Posicion]
getAdj t (x,y) = checkAdj t [(x+1,y), (x,y+1)]
-- si pudiera movese en cualquier direccion:
-- getAdj t (x,y) = checkAdj t [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]

checkAdj :: Tablero -> [Posicion] -> [Posicion]
checkAdj _ [] = []
checkAdj t (p:ps) | posValida t p = p:(checkAdj t ps)
                  | otherwise = checkAdj t ps
-- crea un mapa de posiciones del tablero de row x col
-- row := cantidadFilas t
-- col := cantidadColumnas t
-- r y c son contadores iniciales que arrancan en 1
-- Ej> creaMapa 2 3 1 1
-- [(1,1),(1,2),(1,3), (2,1),(2,2),(2,3)]

creaMapa :: Integer -> Integer -> Integer -> Integer -> [Posicion]
creaMapa row col r c | c <= col && r <= row = (r, c):nuevaCol
                     | r <= row = nuevaRow
                     | otherwise = []
                     where nuevaRow = creaMapa row col (r+1) 1
                           nuevaCol = creaMapa row col r (c+1)
---------------------------------------------------
---------------------------------------------------