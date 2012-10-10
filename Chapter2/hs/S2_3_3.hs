module S2_3_3 where

isB :: Char -> Bool
isB c = (c == 'B') || (c == 'b')

{----- examples of evaluations and results
? isB 'b'
True
? isB 'c'
False
-----}
