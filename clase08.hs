-- convierte a en [a], por convencion es conjunto
type Set a = [a]

-- Escibir una funcion que reciba una lista y devuelva
-- una lista solo con los multiplos de 7
mul7 :: [Integer] -> [Integer]
mul7 [] = []
mul7 (x:xs) | mod x 7 == 0 = x:mul7(xs)
            | otherwise = mul7(xs) 

--
vacio :: Set Integer
vacio = []

--

agregar :: Integer -> Set Integer -> Set Integer
agregar n xs | elem n xs = xs
             | otherwise = n:xs


--

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True 
incluido (a:as) bs | elem a bs = incluido as bs
                   | otherwise = False

---

iguales :: Set Integer -> Set Integer -> Bool
--iguales [] [] = True -- no hace falta
iguales as bs = (incluido as bs) && (incluido bs as)

--- dado un numero n y un conjunto de conjuntos cls
-- agrega a n en cada subconjunto de cls .
agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos n [] = []
agregarATodos n (x:xs) = (agregar n x):(agregarATodos n xs) 

--- partes
partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes(n-1) ++ agregarATodos n (partes (n-1))

--partes 0 = [[]]
--partes 1 = [[], [1]]
--partes 2 = [[], [1], [2], [1,2]]

--podria haberlo arrancado desde uno, que era lo que pedia el ejercicio
partes' :: Integer -> Set (Set Integer)
partes' 1 = [[],[1]]
partes' n = partes'(n-1) ++ agregarATodos n (partes' (n-1))

-- producto cartesiano
pCarte :: Set Integer -> Set Integer -> Set (Integer, Integer)
pCarte [] _ = []
pCarte (a:as) bs = (carteAux a bs) ++ (pCarte as bs)

carteAux :: Integer -> Set Integer -> Set (Integer, Integer)
carteAux a [] = []
carteAux a (b:bs) = (a,b):carteAux a bs


-- VARIACIONES con repeticion
-- dado un conjunto c y una longitud l genere todas las posibles
-- listas de longitud l a partir de elementos de c .
-- Ejemplo> variaciones [4, 7] 3
-- [[4, 4, 4], [4, 4, 7],
--  [4, 7, 4], [4, 7, 7],

--  [7, 4, 4], [7, 4, 7],
--  [7, 7, 4], [7, 7, 7]]
{- 
    > variaciones [4,7] 3

    [[]]
    
    [[4],[7]]
    
    [[4,4],[4,7],
     [7,4],[7,7]]

    [[4,4,4],[4,7,4], [7,4,4],[7,7,4]
     [4,4,7],[4,7,7], [7,4,7],[7,7,7]]]
-}
-- agrega n a todos los subconjuntos
addNATodos :: Integer -> Set (Set Integer) -> Set [Integer]
addNATodos n [] = []
addNATodos n (c:cs) = (n:c):(addNATodos n cs) 

-- agrega cada elemento n de un conjunto a cada subconjunto de un conjunto cs
addTodosATodos :: Set Integer -> Set (Set Integer) -> Set [Integer]
addTodosATodos [] _ = []
addTodosATodos (n:ns) cs = addNATodos n cs ++ (addTodosATodos ns cs)

-- agrega los elementos de cs a todos los elementos anteriores, l veces
variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones _ 0 = [[]]
variaciones cs l = (addTodosATodos cs (variaciones cs (l-1) ))


-- PERMUTACIONES:

-- permutaciones 1:
-- [[1]]
-- insertarEn [] 1 1 > [1]

--- permutaciones 2:
-- [[1,2], [2,1]]
-- insertarEn [1] 2 1 > [2,1]
-- insertarEn [1] 2 2 > [1,2]

--- permutaciones 3
-- [[1, 2, 3], [1, 3, 2], [2, 1, 3],
--  [2, 3, 1], [3, 1, 2], [3, 2, 1]]
--insertarEn [1,2] 3 1  --> [3, 1 , 2]
--insertarEn [1,2] 3 2  --> [1, 3 , 2]
--insertarEn [1,2] 3 3  --> [1, 2 , 3]
-- lo mismo con [2,1]


-- inserta un n en la posicion i de la lista (l:ls)
insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn [] n _ = [n]
insertarEn (l:ls) n i | i == 1 = n:(l:ls)
                      | otherwise = l:(insertarEn ls n (i-1))
-- insertar en TODAS las posiciones de una lista (si i == len(ls))
insEnTodPos :: [Integer] -> Integer -> Integer -> [[Integer]]
insEnTodPos _ _ 0 = []
insEnTodPos ls n i = (insertarEn ls n i):(insEnTodPos ls n (i-1))

-- insertar en todas las posiciones de todas las listas
insEnTodPosTodLis :: [[Integer]] -> Integer -> [[Integer]]
insEnTodPosTodLis [] _ = []
insEnTodPosTodLis (p:ps) n = (insEnTodPos p n n)++(insEnTodPosTodLis ps n)

permu :: Integer -> [[Integer]]
permu 1 = [[1]]
permu n = insEnTodPosTodLis (permu (n-1)) n

----
-- MAS EJERCICIOS
-- Todas las formas de ubicar n bolitas numeradas en k cajas 

-- 1er bolita k opciones, 2da k opciones, 3ra ... , n-esima k opciones
-- k.k.k.k. ... .k (n veces) = k^n (esto se va a descontrolaaar)

-- Inserta bolita n en caja i de conjunto de cajas (c:cs)
-- n -->> [[],[n],[]]
insEnCajas :: [Set Integer] -> Integer -> Integer -> [Set Integer]
insEnCajas [] _ _ = []
insEnCajas (c:cs) n i | i == 1 = (n:c):cs
                      | otherwise = c:(insEnCajas cs n (i-1))
-- Devuelve TODAS las formas ('universos') de meter una bolita n en cajas k_i
-- [[],[],[]] n -->> [ [[n],[],[]] , [[],[n],[]] , [[],[],[n]] ]
bolitaEnCajas :: [Set Integer] -> Integer -> Integer -> [[Set Integer]]
bolitaEnCajas _ _ 0 = []
bolitaEnCajas cs n i = (insEnCajas cs n i):(bolitaEnCajas cs n (i-1))
-- por cada 'universo' de cajas, devuelve un nuevo universo con bolita n de
-- TODAS las formas posibles de meterla en TODAS las cajas de TODOS los universos (D:!)
-- Consejo: Leer por diagonales y columnas (al estar ordenadas como abajo):
-- Ej: porCadaUniverso [[[],[],[1]], [[],[1],[]], [[1],[],[]]] 2 3
-- [ [[],[],[2,1]], [[],[2],[1]],  [[2],[],[1]],
--   [[],[1],[2]],  [[],[2,1],[]], [[2],[1],[]],
--   [[1],[],[2]],  [[1],[2],[]],  [[2,1],[],[]] ]
porCadaUniverso :: [[Set Integer]] -> Integer -> Integer -> [[Set Integer]]
porCadaUniverso [] _ _ = []
porCadaUniverso (u:us) n k = (bolitaEnCajas u n k)++(porCadaUniverso us n k)
-- Mete todas las bolitas en todos los universos de cajas calculados previamente
-- porCadaBolita [ [[],[],[]] ] 2 3
-- [ [[],[],[1,2]], [[],[1],[2]],  [[1],[],[2]],
--   [[],[2],[1]],  [[],[1,2],[]], [[1],[2],[]],
--   [[2],[],[1]],  [[2],[1],[]],  [[1,2],[],[]] ]
porCadaBolita :: [[Set Integer]] -> Integer -> Integer -> [[Set Integer]]
porCadaBolita cs 0 _ = cs
porCadaBolita cs n k = (porCadaBolita (porCadaUniverso cs n k) (n-1) k)
-- No hace nada, solo tiene nombre bonito y llama a porCadaBolita
-- con un universo de k cajas vacias
-- bolitas 1 3:
-- [ [[1],[], []]
--   [[], [1],[]]
--   [[], [], [1]] ]
bolitas :: Integer -> Integer -> [[Set Integer]]
bolitas 0 k = uniDeCajas k
bolitas n k = porCadaBolita (uniDeCajas k) n k

-- Llama a cajas k y la mete en una lista, a lo que llamo 'universo'
--  [[],[],[],[]] -->> [ [[],[],[],[]] ]
uniDeCajas :: Integer -> [[Set Integer]]
uniDeCajas k = [cajas k]
-- Devuelve una lista de k conjuntos (cajas)
-- cajas 4 -->> [[],[],[],[]]
cajas :: Integer -> [Set Integer]
cajas 1 = [[]]
cajas k = [[]]++(cajas (k-1))
-----
-- FIN DE BOLITAS (:D!)

