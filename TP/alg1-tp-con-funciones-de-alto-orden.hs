-- Probando algunas funciones del tp con funciones de alto orden

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
-- Devuelve el numero mas grande de un tablero dado
maximo :: Tablero -> Integer
maximo (t:[]) = foldr max (-9999) t
maximo (t:ts) = max (foldr max (-9999) t) (maximo ts)

-- Devuelve el numero que mas veces aparece en un tablero dado.
-- Si hay empate, devuelve cualquiera de ellos.
-- masRepetido :: Tablero -> Integer

--
numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino t cs = map (valor t) cs