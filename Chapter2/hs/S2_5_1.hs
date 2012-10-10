module S2_5_1 where

makeTuple x = (0, x)

tupleList []     = []
tupleList (x:xs) = (makeTuple x):(tupleList xs)

double x = x + x

doubleList []     = []
doubleList (x:xs) = (double x):(doubleList xs)

tupleList' xs = map makeTuple xs

doubleList' xs = map double xs

one _ = 1

length' xs = sum (map one xs)

{----- Examples of evaluations and results

? tupleList' [1, 2, 3, 4]
[(0, 1), (0, 2), (0, 3), (0, 4)]
? tupleList [1, 2, 3, 4]
[(0, 1), (0, 2), (0, 3), (0, 4)]
? doubleList [1, 2, 3, 4]
[2, 4, 6, 8]
? tupleList' [1, 2, 3, 4]
[(0, 1), (0, 2), (0, 3), (0, 4)]
? doubleList' [1, 2, 3, 4]
[2, 4, 6, 8]
? length' [1, 2, 3, 4, 5]
5

-----}
