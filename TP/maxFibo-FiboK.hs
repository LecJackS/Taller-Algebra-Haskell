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