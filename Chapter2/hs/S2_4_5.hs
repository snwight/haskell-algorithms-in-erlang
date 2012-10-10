module S2_4_5 where

import Prelude hiding (concat)
import List -- necessary for the definition of (\\)

concat []	    = []
concat ([]:ys)	    = concat ys
concat ((x:xs):ys) = x:concat (xs:ys)

removeDups []	                 = []
removeDups [x]	                 = [x]
removeDups  (x:y:ys) | x == y    = removeDups (y:ys)
		     | otherwise = x:removeDups (y:ys)

rem34 (p:q:r:s:xs) | r == s    = p:q:xs
		   | otherwise = p:q:r:s:xs

rem34' lab@(p:q:r:s:xs) | r == s    = p:q:xs
			| otherwise = lab

perms [] = [[]]
perms xs = [x:p | x <- xs, p <- perms (removeFirst x xs)]
    where removeFirst x []                 = []
          removeFirst x (y:ys) | x == y    = ys
                               | otherwise = y : removeFirst x ys

{----- examples of evaluations and results

? concat [[1, 2, 3, 4], [5, 6, 7], [8, 9]]
[1, 2, 3, 4, 5, 6, 7, 8, 9]
? removeDups [1, 2, 3, 3, 1, 2, 4, 5, 5]
[1, 2, 3, 1, 2, 4, 5]
? rem34 [1, 2, 2, 2, 2, 2, 2, 2]
[1, 2, 2, 2, 2, 2]
? rem34' [1, 2, 2, 2, 2, 2, 2, 2]
[1, 2, 2, 2, 2, 2]
? perms [1..3]
[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

-----}
