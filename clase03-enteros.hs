-- Ejercicios de numeros enteros.
-- Dar el tipo y luego implementar las siguientes funciones:
--
-- 1 unidades : dado un entero, devuelve el digito de las unidades del numero
--             el digito menos significativo).
signo :: Integer -> Integer
signo x | x >= 0 = 1
        | otherwise = (-1)
unidades :: Integer -> Integer
unidades x = (signo x) * (mod (abs x) 10)
-- variante linda, usando la funcion rem
unidades2 x = rem x 10 
-- 2 sumaUnidades3 : dados 3 enteros, devuelve la suma de los digitos de las
--                  unidades de los 3 numeros.
sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 a b c = unidades a + unidades b + unidades c
-- 3 todosImpares:  dados 3 n ́umeros enteros determina si son todos impares.
todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c = ((mod a 2) == 1) && ((mod b 2) == 1) && ((mod c 2) == 1)
-- 4 alMenosUnImpar: dados 3 n ́umeros enteros determina si al menos uno de ellos
--                  es impar.
alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c = ((mod a 2) == 1) || ((mod b 2) == 1) || ((mod c 2) == 1)
-- otra variante:
alMenosUnImpar2 :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar2 a b c | mod a 2 == 1 = True
                      | mod b 2 == 1 = True
                      | mod c 2 == 1 = True
                      | otherwise = False
-- 5 alMenosDosImpares: dados 3 numeros enteros determina si al menos dos de
--                     ellos son impares.
alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c = (mod a 2) + (mod b 2) + (mod c 2) >= 2
-- 6 alMenosDosPares: dados 3 numeros enteros determina si al menos dos de ellos
--                    son pares.
-- Aclaracion: si entre tres numeros al menos dos NO son IMPARES, entonces al 
--             menos dos son PARES)
alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares a b c = not (alMenosDosImpares a b c)
-- 7 alMenosUnMultiploDe: dados 3 numeros enteros determina si alguno de los
--                        primeros dos es multiplo del tercero
-- Aclaracion: si 'a' o 'b' son cero, se produciria un error, pero en esta clase
--             suponemos que los datos ingresados siempre seran validos, o sea,
--             siempre estaran dentro del dominio de la funcion.
alMenosUnMultiploDe :: Integer -> Integer -> Integer -> Bool
alMenosUnMultiploDe a b c = ((rem c a) == 0) || ((rem c b) == 0)