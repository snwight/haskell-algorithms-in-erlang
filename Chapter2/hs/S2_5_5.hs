module S2_5_5 where

double x = x* 2
square x = x * x
bump x   = x + 1

applyall [] x = x
applyall (f:fs) x = f (applyall fs x)

{----- Examples of evaluations and results

? applyall [bump, square, double] 3
37

-----}
