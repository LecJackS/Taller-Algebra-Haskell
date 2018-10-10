-- Pattern Matching + Recursion
-- 2 a
yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False

--b
oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True

--c Recordar el implica
-- Tabla de verdad del "implica"
-- True  > True  > True
-- True  > False > True
-- False > True  > False
-- False > False > True
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--d
sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

--e
algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (a, b, c) = a*b*c == 0

--d
productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, y1) (x2, y2) = (x1*x2) + (y1*y2) 


-- Ejercicios (mas recursion)

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | otherwise = n == (menorDivisor n)

checkPrimos :: Integer -> Integer -> Bool
checkPrimos n k | n == 1 = False
                | (esPrimo n) && (esPrimo k) = True
                | otherwise = checkPrimos (n-1) (k+1)

--checkPrimos 1 k = (esPrimo k)
--checkPrimos n k = (esPrimo n) && (esPrimo k)

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos 1 = False
esSumaDeDosPrimos n = checkPrimos (n-1) 1

-- 2 -- todo numero par > 2 es suma de dos primos
-- checkea si todos los p pares hasta n son suma de dos primos
pruebaGoldHasta :: Integer -> Bool
pruebaGoldHasta 3 = False   -- solo por si ingresan un impar o 3
pruebaGoldHasta 4 = True    -- primer par > 2
pruebaGoldHasta n = esSumaDeDosPrimos n && pruebaGoldHasta (n-2)

-- 3
-- suma de digitos version desprolija. 
auxSuma :: Integer -> Integer -> Integer
auxSuma n k | n < k = n
            | otherwise = unidad + auxSuma (div (n-unidad) 10) k --trunco unidad de n
            where unidad = mod n k

sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
              | otherwise = auxSuma n 10

-- limpiando el codigo
-- suma de digitos de n
sumaDigitos2 :: Integer -> Integer
sumaDigitos2 n | n < 10 = n
               | otherwise = unidad + sumaDigitos2 nTrunco
               where unidad  = mod n 10
                     nTrunco = div (n-unidad) 10 --trunco unidad de n

-- 4
-- todos digitos iguales
-- compara unidad de n previo con la unidad del n truncado
-- o sea, compara unidad de n con su decena, si son iguales,
-- compara decena con centena, y asi hasta llegar al ultimo digito o
-- hasta encontrar dos digitos diferentes
auxTodosDigitosIguales :: Integer -> Integer -> Bool
auxTodosDigitosIguales n uPre | n == 0 = True
                              | unidad == uPre = auxTodosDigitosIguales nTrunco unidad 
                              | otherwise = False
                               where unidad = mod n 10 -- unidad de n
                                     nTrunco = div (n-unidad) 10 --trunco unidad de n

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = auxTodosDigitosIguales n unidad
                      where unidad  = mod n 10

-- algo que me hace ruido es que al llamar la funcion por primera vez,
-- compara la unidad de n con si misma, lo cual siempre es verdadero,
-- pero no se me ocurrio una forma linda de saltear este primer paso

-- 5
-- sucesion a_[n+1] se define como:
--     a_[n] / 2     : si a_[n] es par
--     3*a_[n] + 1   : si a_[n] es impar

-- si a_1 = 13, obtengo:
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
-- 10 terminos

-- voy a hacer un contador de terminos a partir de un a_n, aunque
-- no estoy seguro que el ejercicio pida

-- Primero la conjetura a secas que siempre devuelve un 1
conjeturaDeCollatz :: Integer -> Integer
conjeturaDeCollatz an | an == 1 = 1
                      | anEsPar   = conjeturaDeCollatz (div an 2)
                      | otherwise = conjeturaDeCollatz (3*an + 1)
                      where anEsPar = mod an 2 == 0

-- ahora implemento el contador de terminos, que suma 1 en cada llamado recursivo
contaColl :: Integer -> Integer
contaColl an | an == 1 = 1
             | anEsPar   = 1 + contaColl (div an 2)
             | otherwise = 1 + contaColl (3*an + 1)
             where anEsPar = mod an 2 == 0

-- y ahora hago una funcion que pruebe todos los valores desde an = 10.000 hasta 2
-- y devuelva el de la secuencia más larga (an=1 se reserva para terminar
-- la secuencia, sino nunca termina)

checkLenColl :: Integer -> Integer -> Integer
checkLenColl an max | an == 1 = max
                    | contaColl an > contaColl max = checkLenColl (n-1) an
                    | otherwise = checkLenColl (an-1) max

checkLenCollHasta an = checkLenColl an an

-- compara cantidad de terminos de an con cantidad de terminos maxima hasta el
-- momento (max). Si an tiene mas terminos que el maximo guardad, este an es el
-- nuevo maximo, y se hace una llamada recursiva para an-1 y el maximo.
-- finalmente devuelvo el maximo obtenido a traves de todos los llamados
-- recursivos.

-- an = 6171 es el valor que mas terminos tiene, con 262 terminos.
-- puede ser que existan otros valores con 262 terminos, en cuyo caso, son
-- ignorados por la funcion.