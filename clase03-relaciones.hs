-- Ejercicios de relaciones
-- 8 - Dados dos enteros a, b implementar funciones:
--     (r1, r2 y r3) :: Integer -> Integer -> Bool
--     que determinen si a ∼ b donde:
-- 8.1 - a ∼ b tienen la misma paridad.
r1 :: Integer -> Integer -> Bool
r1 a b = mod a 2 == mod b 2
-- 8.2 - a ∼ b si 2a + 3b es divisible por 5.
r2 :: Integer -> Integer -> Bool
r2 a b = (rem (2*a + 3*b) 5) == 0
-- 8.3 - a ∼ b si los digitos de las unidades de a, b y a*b son todos distintos.
uni :: Integer -> Integer
uni x = rem x 10
-- Aclaracion: si los digitos de las unidades son distintos, los digitos del
--             producto de los mismos tambien lo son, por lo que no hace falta
--             agregar (uni (a*b)) en la funcion
r3 :: Integer -> Integer -> Bool
r3 a b = (uni a) /= (uni b)
rela :: Integer -> Integer -> Bool
rela a b = (r1 a b) || (r2 a b) || (r3 a b)
-- variante:
rela2 :: Integer -> Integer -> Bool
rela2 a b | r1 a b = True
          | r2 a b = True
          | r3 a b = True
          | otherwise = False
-- 9. Se define en R la relacion de equivalencia asociaeda a la particion:
--      R = (-inf, 3) U [3, +inf)
--    Determinar el tipo e implementar una funcion que dados dos numeros
--    x, y pertenecientes a R determine si x ∼ y
rela3 :: Float -> Float -> Bool
rela3 x y | x < 3  && y < 3  = True -- (-inf, 3) 
          | x >= 3 && y >= 3 = True -- [3, +inf)
          | otherwise = False

-- 10. Repetir el ejercicio anterior para la particion:
--       R = (-inf, 3) U [3, 7) U [7, +inf)
-- rela7 chequea si x, y existen en (-inf, 7) U [7, +inf) (wait for it)
rela7 :: Float -> Float -> Bool
rela7 x y | x < 7  && y < 7  = True -- (-inf, 7) 
          | x >= 7 && y >= 7 = True -- [7, +inf)
          | otherwise = False

-- si x,y estan en (-inf, 3) o [3, +inf) Y TAMBIEN en (-inf, 7) o [7, +inf)
-- entonces estan en (-inf, 3) o [3, 7) o [7, +inf)
rela10 :: Float -> Float -> Bool
rela10 x y = (rela3 x y) && (rela7 x y)

-- tambien se podria haber hecho reescribiendolo como en el punto 9, pero
-- supuse que la idea del ejercicio era reutilizar codigo y aprovechar
-- propiedades de las particiones.

-- 11. Dados (a, b) y (p, q), determinar el tipo e implementar funciones
--     que determinen si (a, b) esta relacionado con (p, q) cuando:
-- 11.1: (a, b) = k*(p, q), con a,b,p,q,k Reales, sin el cero
--'(a, b) == k*(p, q)' es lo mismo que
-- 'a == k*p' y 'b == k*q'?
-- 'a/p == k' y 'b/q == k'?
-- 'a/p == b/q'?
rela111 :: (Float, Float) -> (Float, Float) -> Bool
rela111 ab pq = ((fst ab) / (fst pq)) == ((snd ab) / (snd pq))
-- 11.2: lo mismo que 11.1, pero con a,b,p,q Enteros, y k Real, sin el cero
-- '(a, b) == k*(p, q)' es lo mismo que
-- 'a/p == b/q', pero como son Integers y la division no esta definida,
-- 'pasamos' multiplicando los denominadores y obtenemos
-- 'a*q == b*p'
rela112 :: (Float, Float) -> (Float, Float) -> Bool
rela112 ab pq = ((fst ab) * (snd pq)) == ((snd ab) * (fst pq))
--11.3: Opcional: lo mismo que 11.2 pero incluyen el (0,0):
rela113 :: (Float, Float) -> (Float, Float) -> Bool
rela113 ab pq | (fst pq == 0) || (snd pq == 0) = False
              | otherwise = rela112 ab pq