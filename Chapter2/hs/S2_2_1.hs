module S2_2_1 where

fact n = if (n == 0)
	 then 1
	 else n * fact (n-1)

fact' n | n == 0 = 1
	| otherwise = n * fact (n - 1)

fact'' 0 = 1
fact'' n = n * fact (n - 1)

checkVal x y | x > y  = 1
	     | x < y  = -1
	     | x == y = 0


{-----  examples of evaluations and results
? fact 10
3628800
? fact' 10
3628800
? fact'' 10
3628800
? checkVal 10 20
-1
? checkVal 10 10
0
? checkVal 20 10
1
-----}
