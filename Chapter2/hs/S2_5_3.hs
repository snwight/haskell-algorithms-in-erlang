module S2_5_3 where

import Prelude hiding (concat)

concat xs = foldr (++) [] xs

revOnto' xs ys = (reverse ys) ++ xs

consOnto xs y = y:xs
revOnto l1 l2 = foldl consOnto l1 l2

sumll xss = foldl (foldl (+)) 0 xss

listDiff xs1 xs2
    = foldl del xs1 xs2
      where del [] _                  = []
            del (x:xs)  y | x == y    = xs
                          | otherwise = x : (del xs y)

{----- Examples of evaluations and results

? concat [[1, 2, 3, 4], [5, 6, 7, 8]]
[1, 2, 3, 4, 5, 6, 7, 8]
? revOnto' [1, 2, 3] [4, 5, 6]
[6, 5, 4, 1, 2, 3]
? revOnto [1, 2, 3] [4, 5, 6]
[6, 5, 4, 1, 2, 3]
? sumll [[1, 2, 3], [2, 3, 4]]
15
? listDiff [1, 2, 3, 4, 5] [1, 3, 2]
[4, 5]

-----}
