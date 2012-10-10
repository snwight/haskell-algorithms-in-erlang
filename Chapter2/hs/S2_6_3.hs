module S2_6_3 where

cond (x, y, z) = if x then y else z
apply (f, x) = f x

{----- Examples of evaluations and results

? cond (True, 23, 25)
23
?  apply (id, 23)
23
? apply (fst, (1234, 122.3))
1234

-----}
