--esPar :: Integer -> Integer
--esPar x | x > 0 = esPar (x-2)
--        | x == 0 = 1
--        | otherwise = 0

--esMul3 :: Integer -> Integer
--esMul3 x | x > 0 = esPar (x-3)
--         | x == 0 = 1
--         | otherwise = 0

-- 1. fibonacci
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)


-- 2.1:
-- a_1 = 2;
-- a_(n+1) = 2*n*a_n + 2**(n+1)*n!
fact :: Integer -> Integer
fact n | n == 0 = 1
       | otherwise = n * (fact (n-1))

a1 :: Integer -> Integer
a1 n | n == 1 = 2
     | otherwise = (2 * (n-1) * (a1 (n-1))) + ((2 ^ n) * fact (n-1))

-- 2.2:
a2 :: Integer -> Integer
a2 n | n == 1 = 1
     | n == 2 = 2
     | otherwise = ((n-2) * (a2 (n-1))) + (2 * (n-1) * (a2 (n-2)))

-- 2.3:
a3 :: Integer -> Integer
a3 n | n == 1 = (-3)
     | n == 2 = 6
     | (mod n 2) == 1 = (-(a3 (n-1)) - 3)
     | otherwise = a3 (n-1) + (2 * (a3 (n-2)) + 9)

-- recursion con distintos tipos
-- f :: Integer -> Bool
-- f n | n == 1 = True
--     | n > 1  = not (f (n-1))
% n d = mod n d