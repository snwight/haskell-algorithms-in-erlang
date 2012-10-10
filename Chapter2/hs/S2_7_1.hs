module S2_7_1 where

import Array

a' = array (1, 4) [(3, 'c'), (2, 'a'), (1, 'f'), (4, 'e')]
f n = array (0, n) [(i, i*i) | i <- [0..n]]
m = array ((1, 1), (2, 3)) [((i, j), (i*j)) | i <- [1..2], j <- [1..3]]

a' :: Array Int Char
f  :: Int -> Array Int Int
m  :: Array (Int, Int) Int

fibs n = a
    where
    a = array (1,n) ([(1, 1), (2, 1)] ++
                     [(i, a!(i-1) + a!(i-2)) | i <- [3..n]])
	
a'' = listArray (1,4) "face"

histogram bnds vs = accumArray (+) 0 bnds [(i, 1) | i <- vs]
	  
{----- Examples of evaluations and results

? f 3
array (0, 3) [(0, 0), (1, 1), (2, 4), (3, 9)]
? m
array ((1, 1), (2, 3)) [((1, 1), 1), ((1, 2), 2), ((1, 3), 3), ((2, 1), 2), ((2, 2), 4), ((2, 3), 6)]
? fibs 3
array (1, 3) [(1, 1), (2, 1), (3, 2)]
? a''
array (1, 4) [(1, 'f'), (2, 'a'), (3, 'c'), (4, 'e')]
? histogram (0,9) [3,1,4,1,5,9,2]
array (0, 9) [(0, 0), (1, 2), (2, 1), (3, 1), (4, 1), (5, 1), (6, 0), (7, 0), (8, 0), (9, 1)]

-----}
