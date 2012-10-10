module S2_3_4 where

t1 = (3, True)

t2 = (14.8, 'd', 34)

t3 = ((True, "hello"), False, (112, 16.821))

later (h1, m1, s1) (h2, m2, s2)
    | h1 < h2   = False
    | h1 > h2   = True
    | m1 < m2   = False
    | m1 > m2   = True
    | otherwise = s1 > s2

distance (x1, y1) (x2, y2)
    = sqrt (dx * dx + dy * dy)
      where
      dx = x2 - x1
      dy = y2 - y1
			       
roots (a, b, c) = (r1, r2)
    where
    r1         = (-b + r) / f
    r2         = (-b - r) / f
    f          = 2 * a
    r | d >= 0 = sqrt d
      | d < 0  = error "imaginary roots"
    d          = b * b - 4 * a * c

{----- Examples of evaluations and results
? :set +t
? t1
(3, True) :: (Int,Bool)
? t2
(14.8, 'd', 34) :: (Double,Char,Int)
? t3
((True, "hello"), False, (112, 16.821)) :: ((Bool,[Char]),Bool,(Int,Double))
? :set -t
? later (12,34,56) (4,56,34)
True
? distance (1,0) (5,5)
6.40312
? roots (1,1,1)
(
Program error: imaginary roots

? roots (1,2,1)
(-1.0, -1.0)
? roots (1,1,-2)
(1.0, -2.0)
-----}
