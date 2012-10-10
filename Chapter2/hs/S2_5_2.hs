module S2_5_2 where

tupleList xs = map (\x -> (0, x)) xs

doubleList xs = map (\x -> x + x) xs

doubleList' xs = map ((*)2) xs

{----- Examples of evaluations and results

? tupleList [1, 2, 3, 4, 5]
[(0, 1), (0, 2), (0, 3), (0, 4), (0, 5)]
? doubleList [1, 2, 3, 4, 5]
[2, 4, 6, 8, 10]
? doubleList' [1, 2, 3, 4, 5]
[2, 4, 6, 8, 10]

-----}
