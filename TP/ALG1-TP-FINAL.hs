-- Subo el TP porque me parecio un enfoque interesante a funciones recursivas
-- Si estas cursando la materia, usalo con responsabilidad (no copypastees porfa).
-- Si no se entiende algo, podes abrir un nuevo issue/problema y preguntar sin dramas.

-- para testeo luego
import Test.HUnit

type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6],
         [7,  13, 32, 6],
         [22, 20, 14, 7],
         [7,  33, 53, 16],
         [27, 2,  8,  18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)],
         [   0,    10,    20],
         [(-10), (-10),    0],
         [  10,    20,  (-10)]]

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

---TP INICIA ACA

-- maximo es una funcion que devuelve el numero mas grande de un tablero dado
maximo :: Tablero -> Integer 
maximo [] = error "Tablero vacio"
maximo (x:[]) = maximoDeUnaFila x
maximo (x:xs) = max (maximoDeUnaFila x) (maximo xs)

-- maximoDeUnaFila es una función que devuelve el número más grande de una lista
-- (en este caso una fila del tablero)
maximoDeUnaFila :: [Integer] -> Integer
maximoDeUnaFila (x:[]) = x
maximoDeUnaFila (x:xs) = max x (maximoDeUnaFila xs) 

-- masRepetido es una función que devuelve el número que más veces aparece en un tablero dado. 
-- si hay empate devuelve cualquiera de ellos.
masRepetido :: Tablero -> Integer
masRepetido t = escanearTab t 0 0 []

-- escanearTab es una función escanea el tablero comparando las repeticiones de cada uno de sus valores
-- con el siguiente, solo quedándose con el mayor
-- m: valor que mas aparece
-- i: cantidad de apariciones de m 
-- historial: lista elementos ya contados, para no contar 2 veces el mismo
escanearTab :: Tablero -> Integer -> Integer -> [Integer] -> Integer
escanearTab [] m i hist = m
escanearTab ([]:ts) m i hist = escanearTab ts m i hist
escanearTab ((n:ns):ts) m i hist | elem n hist = ignorN
                                 | ocuDeN < i = ignorN
                                 | otherwise  = escanearTab (ns:ts) n ocuDeN (n:hist)
                                 where tabler = ((n:ns):ts)
                                       ocuDeN = contar tabler n
                                       ignorN = escanearTab (ns:ts) m i hist  
                                    
-- contar es una funcion que cuenta las ocurrencias del valor v en un tablero
contar :: Tablero -> Integer -> Integer
contar [] v = 0
contar ([]:ts) v = contar ts v
contar ((n:ns):ts) v | v == n = 1 + contar (ns:ts) v
                     | otherwise = contar (ns:ts) v
----------------------------------------------------------
-- fin de masRepetido

-- numerosDeCamino es una funcion que devuelve los números de los casilleros de un camino.
numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino _ [] = []
numerosDeCamino t (c:cs) = (casillero):(numerosDeCamino t cs)
                           where casillero = valor t c

-- caminoSinRepetidos es una función que devuleve True si y solo si en un camino
-- NO aparecen números repetidos
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t (c:[]) = True 
caminoSinRepetidos t (c:cs) | primeroNoRepetido t (c:cs) = caminoSinRepetidos t cs
                            | otherwise = False

primeroNoRepetido :: Tablero -> Camino -> Bool
primeroNoRepetido t (c:[]) = True
primeroNoRepetido t (c1:c2:cs) | valor1 == valor2 = False
                               | otherwise = primeroNoRepetido t (c1:cs)
                                where valor1 = valor t c1
                                      valor2 = valor t c2

  
-- caminoDeFibonacci es una funcion que determina si los números de los casilleros
-- de un camino forman un camino de Fibonacci.
-- caminoDeFibonacci  compara si la suma de las dos primeras posiciones de un camino
-- es igual al valor de la tercera posicion  
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci _ [] = True
caminoDeFibonacci _ (c1:[]) = True
caminoDeFibonacci _ (c1:c2:[]) = True
caminoDeFibonacci t (c1:c2:c3:cs) | (valor1+valor2) == valor3 = caminoDeFibonacci t (c2:c3:cs) 
                                 | otherwise = False
                                 where valor1 = valor t c1
                                       valor2 = valor t c2
                                       valor3 = valor t c3
  
-- mayorSecuenciaDeFibonacci
-- Devuelve la secuencia fibonacci de mayor cantidad de elementos en t
mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci t = buscaMayorFib t 3

-- Obs: toda secuencia de 2 elementos es secuencia fibonacci,
-- por lo que podemos ignorar los primeros 2 k y comenzar con k=3
buscaMayorFib :: Tablero -> Integer -> [Integer]
buscaMayorFib t k | (secuenciasDeFibonacciDeLongitudK t k) == [] = secueKAnt
                  | otherwise = buscaMayorFib t (k+1)
                  where secueKAnt = head (secuenciasDeFibonacciDeLongitudK t (k-1))

-- secuenciasDeFibonacciDeLongitudK:
-- Devuelve el conjunto de las listas de numeros
-- de todos los caminos de Fibonacci de longitud k
-- Descripcion:
-- El algoritmo completo comienza a partir del tamaño del tablero.
-- A partir de eso, crea un camino de largo k=1 por cada casillero del tablero.
-- Partiendo de cada camino, crea 0, 1 o 2 nuevos caminos de largo k=2,
-- dependiendo de si los casilleros adyacentes son posiciones validas.
-- A partir de cada nuevo camino, se repite el procedimiento anterior recursivamente
-- hasta llegar al largo k con que la funcion es llamada.
-- En cada expansion de caminos, se filtran los caminos fibonacci y el resto se descarta.
-- Finalmente, los caminos que hasta ahora eran listas de posiciones en el tablero,
-- se 'traducen' a listas de integers, y se descartan los repetidos para obtener
-- conjuntos de integers.
secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t k = borraDuplicados (secueFiboKConRepes t k)

-- Borra duplicados de la lista de lista de valores
-- para tener un conjunto (sin repetidos) y no una lista
borraDuplicados :: [[Integer]] -> Conjunto [Integer]
borraDuplicados [] = [] 
borraDuplicados (v:vs) = v:borraDuplicados(borraOcurrencias v vs)

-- Borra las ocurrencias de una lista l en una lista de listas (v:vs)
borraOcurrencias :: [Integer] -> [[Integer]] -> [[Integer]]
borraOcurrencias _ [] = []
borraOcurrencias l (v:vs) | l == v = borraOcurrencias l vs
                          | otherwise = v:borraOcurrencias l vs

-- Crea una secuencia de fibonacci, solo que con repeticiones, que
-- vienen de caminos diferentes
secueFiboKConRepes :: Tablero -> Integer -> [[Integer]]
secueFiboKConRepes t k = numerosDeConjunto t (crearSecuenciasK t k)

-- Convierte un conjunto de caminos a un conjunto de numeros
numerosDeConjunto :: Tablero -> Conjunto Camino -> [[Integer]]
numerosDeConjunto _ [] = []
numerosDeConjunto t (c:cs) = (numerosDeCamino t c):(numerosDeConjunto t cs)

-- Crea caminos de fibonacci de largo k
crearSecuenciasK :: Tablero -> Integer -> Conjunto Camino
crearSecuenciasK _ 0 = []
crearSecuenciasK t 1 = caminosIniciales t
crearSecuenciasK t k = expandCaminos t (crearSecuenciasK t (k-1))

-- Devuelve los caminos 'iniciales' de un solo elemento cada uno,
-- o sea, cada posicion del mapa en un conjunto de caminos de un elemento
caminosIniciales :: Tablero -> Conjunto Camino 
caminosIniciales t = mapaToCami (creaMapa row col 1 1)
                     where row = cantidadFilas t
                           col = cantidadColumnas t

-- Recibe la lista de todas las posiciones del tablero, y las convierte en
-- un conjunto de caminos
mapaToCami :: [Posicion] -> Conjunto Camino 
mapaToCami [] = []
mapaToCami (m:ms) = [m]:(mapaToCami ms)

-- Recibe conjunto de caminos, y devuelve un conjunto con TODOS 
-- los nuevos caminos posibles, que sean fibonacci (checkea en getCaminos)
expandCaminos :: Tablero -> Conjunto Camino -> Conjunto Camino 
expandCaminos _ [] = []
expandCaminos t (cami:cs) = nuevosCami++(expandCaminos t cs)
                            where lastPos = lastPosCami cami
                                  nuevosCami = getCaminos t cami (getAdj t lastPos)

-- Devuelve la ultima posicion de un camino, para luego expandir
lastPosCami :: Camino -> Posicion
lastPosCami (c:[]) = c
lastPosCami (c:cs) = lastPosCami cs

-- Recibe un camino y una lista de nuevos destinos adjacentes,
-- y devuelve la lista de los nuevos caminos posibles, SOLO SI
-- el nuevo camino es un camino de fibonacci
getCaminos :: Tablero -> Camino -> [Posicion] -> Conjunto Camino
getCaminos _ _ [] = [] 
getCaminos t cami (a:adjs) | caminoDeFibonacci t nuevoCami = nuevoCami:(getCaminos t cami adjs)
                           | otherwise = getCaminos t cami adjs
                            where nuevoCami = cami ++ [a]

-- Devuelve los casilleros (validos) adjacentes a una posicion en un mapa
-- solo puede moverse hacia la derecha (x,y+1) y abajo (x+1,y)
getAdj :: Tablero -> Posicion -> [Posicion]
getAdj t (x,y) = checkAdj t [(x+1,y), (x,y+1)]
-- Obs: si pudiera moverse en cualquier direccion:
-- getAdj t (x,y) = checkAdj t [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]

checkAdj :: Tablero -> [Posicion] -> [Posicion]
checkAdj _ [] = []
checkAdj t (p:ps) | posValida t p = p:(checkAdj t ps)
                  | otherwise = checkAdj t ps

-- Crea un mapa de posiciones del tablero de row x col
-- row := cantidadFilas t
-- col := cantidadColumnas t
-- r y c son contadores iniciales que arrancan en 1
-- Ej> creaMapa 2 3 1 1
-- [(1,1),(1,2),(1,3),
--  (2,1),(2,2),(2,3)]
creaMapa :: Integer -> Integer -> Integer -> Integer -> [Posicion]
creaMapa row col r c | c <= col && r <= row = (r, c):nuevaCol
                     | r <= row = nuevaRow
                     | otherwise = []
                     where nuevaRow = creaMapa row col (r+1) 1
                           nuevaCol = creaMapa row col r (c+1)
---------------------------------------------------

-- TESTEO
-- para correr testeos en ghci, usar el comando:
-- runTestTT tests
-- previamente habiendo instalado el package HUnit
-- http://hackage.haskell.org/package/HUnit
{-
o podes comentar
todo el bloque
con {- -}
-}
-- testn = TestCase (assertEqual "mensaje" valorEsperado valorObtenido)
test1 = TestCase (assertEqual "maximo sopa1" 53 (maximo sopa1))
test2 = TestCase (assertEqual "maximo sopa2" 20 (maximo sopa2))
test3 = TestCase (assertEqual "masRepetido sopa1" 7 (masRepetido sopa1))
test4 = TestCase (assertBool "masRepetido sopa2" (((masRepetido sopa2)==(-10)) || (masRepetido sopa2)==(-20)))
test5 = TestCase (assertEqual "numerosDeCamino sopa1 camino1" [13,12,13,32] (numerosDeCamino sopa1 camino1))
test6 = TestCase (assertEqual "numerosDeCamino sopa1 camino2" [7,13,32,14,53] (numerosDeCamino sopa1 camino2))
test7 = TestCase (assertEqual "numerosDeCamino sopa2 camino1" [-20,-20,10,20] (numerosDeCamino sopa2 camino1))
test8 = TestCase (assertEqual "numerosDeCamino sopa2 camino2" [0,10,20,0,-10] (numerosDeCamino sopa2 camino2))
test9 = TestCase (assertEqual "caminoSinRepetidos sopa1 camino1" False (caminoSinRepetidos sopa1 camino1))
test10 = TestCase (assertEqual "caminoSinRepetidos sopa1 camino2" True (caminoSinRepetidos sopa1 camino2))
test11 = TestCase (assertEqual "caminoDeFibonacci sopa1 camino3" False (caminoDeFibonacci sopa1 camino3))
test12 = TestCase (assertEqual "caminoDeFibonacci sopa3 camino3" True (caminoDeFibonacci sopa3 camino3))
test13 = TestCase (assertEqual "mayorSecuenciaDeFibonacci sopa1" [7,13,20,33,53] (mayorSecuenciaDeFibonacci sopa1))
test14 = TestCase (assertEqual "mayorSecuenciaDeFibonacci sopa2" [-20,10,-10,0,-10] (mayorSecuenciaDeFibonacci sopa2))
test15 = TestCase (assertBool "mayorSecuenciaDeFibonacci sopa3" ((((mayorSecuenciaDeFibonacci sopa3)==[10,5,15]) || ((mayorSecuenciaDeFibonacci sopa3)==[5,7,12]))))
test16 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa1 1" [[12],[19],[13],[32],[6],[22],[20],[14],[7],[33],[53],[16],[27],[2],[8],[18]] (secuenciasDeFibonacciDeLongitudK sopa1 1) )
test17 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa1 3" [[7,13,20],[13,20,33],[20,33,53]] (secuenciasDeFibonacciDeLongitudK sopa1 3) )
test18 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa1 4" [[7,13,20,33],[13,20,33,53]] (secuenciasDeFibonacciDeLongitudK sopa1 4) )
test19 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa1 5" [[7,13,20,33,53]] (secuenciasDeFibonacciDeLongitudK sopa1 5) )
test20 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa1 6" [] (secuenciasDeFibonacciDeLongitudK sopa1 6))
test21 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa2 1" [[-20],[0],[10],[20],[-10]] (secuenciasDeFibonacciDeLongitudK sopa2 1))
test22 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa2 2" [[-20,0],[-20,10],[-20,-20],[-20,20],[0,10],[10,-10],[20,0],[-10,10],[-10,-10],[-10,20],[-10,0],[0,-10],[10,20],[20,-10]] (secuenciasDeFibonacciDeLongitudK sopa2 2) )
test23 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa2 3" [[-20,10,-10],[-20,20,0],[0,-10,-10],[10,-10,0],[-10,0,-10]] (secuenciasDeFibonacciDeLongitudK sopa2 3) )
test24 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa2 4" [[-20,10,-10,0],[10,-10,0,-10]] (secuenciasDeFibonacciDeLongitudK sopa2 4) )
test25 = TestCase (assertEqual "secuenciasDeFibonacciDeLongitudK sopa2 5" [[-20,10,-10,0,-10]] (secuenciasDeFibonacciDeLongitudK sopa2 5))
--test = TestCase (assertEqual "" () )

tests = TestList [ test1,  test2,  test3,  test4,  test5,
                   test6,  test7,  test8,  test9,  test10,
                   test11, test12, test13, test14, test15,
                   test16, test17, test18, test19, test20, 
                   test21, test22, test23, test24, test25]