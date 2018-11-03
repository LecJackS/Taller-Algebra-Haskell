type Complejo = (Float, Float)

suma :: Complejo -> Complejo -> Complejo
suma (a,bi) (c,di) = (a+c, bi+di)

-- producto (a,ai) (b,bi) = (a*c + a*di + bi*c + bi*di)
producto :: Complejo -> Complejo -> Complejo
producto (a,bi) (c,di) = (a*c - b*d, a*di + bi*c)
                       where b = bi
                             d = di

re :: Complejo -> Float
re (a,bi) = a

im :: Complejo -> Float
im (a,bi) = bi

modulo :: Complejo -> Float
modulo (a,bi) = sqrt (a^2 + bi^2)

-- 360° = 2pi
argumento :: Complejo -> Float
argumento (a,bi) | a > 0 = valorPrin
                 | a < 0 && bi >= 0 = pi + valorPrin
                 | a < 0 && bi < 0 = -pi + valorPrin
                 | a == 0 && bi > 0 =  pi/2
                 | a == 0 && bi < 0 = -pi/2
                 | otherwise = 0
                   where toDegrees = 360 / (2*pi)
                         valorPrin = toDegrees * atan (bi/a)

-------
-- Mas ejercicios
conjugado :: Complejo -> Complejo
conjugado (a,bi) = (a,-bi)

inverso :: Complejo -> Complejo
inverso (a, bi) = (a/(a^2+bi^2), -bi/(a^2+bi^2))

cociente :: Complejo -> Complejo -> Complejo
cociente (a,bi) (c,di) = ( (ac+bd)/l2cd , (bc-ad)/l2cd )
                        where ac = a*c; bd = bi*di
                              bc = bi*c; ad = a*di
                              l2cd = (c^2+di^2)

potencia :: Complejo -> Integer -> Complejo
potencia (a, bi) 1 = (a, bi)
potencia (a, bi) n = producto (a, bi) (potencia (a, bi) (n-1))

---
-- Agregar al codigo:
numerador = (1, sqrt 3) :: Complejo
denominador = (1, -1) :: Complejo

a = (-1, sqrt 3) :: Complejo