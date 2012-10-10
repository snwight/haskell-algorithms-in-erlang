module S2_6_4 where

data IntList = Nil
             | Cons Int IntList
    deriving Show

lengthIntList  Nil = 0
lengthIntList (Cons _ xs) = 1 + lengthIntList xs

data List a = Cons' a (List a)
	     | Nil'
    deriving Show

lengthList  Nil' = 0
lengthList (Cons' _ xs) = 1 + lengthList xs

or' [] = False
or' (True: _) = True
or' (_:xs) = or' xs

numDigits [] = 0
numDigits (c:cs) = (if (c >= '0') && (c <= '9') then 1 else 0)
                   + numDigits cs

{----- Examples of evaluations and results

? lengthIntList (Cons 3 (Cons 1 (Cons 4 Nil)))
3
? lengthList (Cons' 3 (Cons' 1 (Cons' 4 Nil')))
3
? lengthList (Cons' 'a' (Cons' 'c' (Cons' 'd' Nil')))
3
? or [True, 3>4]
True
? or [False, 3>4]
False
? or [False, 3<4]
True
? numDigits ['1', 'a', '5', 'c']
2

-----}
