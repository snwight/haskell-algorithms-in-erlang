module S2_2 where

e = 2.717281828
area r  = pi * r * r
stirling n = (n/e)**n * sqrt(2*pi*n)

volume r = 4.0 / 3.0 * pi * (cube r)
    where
    cube x = x * x * x

volume' r = let
	       cube x = x * x * x
	    in
	       4.0 / 3.0 * pi * (cube r)

{----- examples of evaluations and results
? area 10
314.159
? stirling 10
3.61197e+06
? product [1..10]
3628800
? volume 10
4188.79
? volume' 10
4188.79
-----}
