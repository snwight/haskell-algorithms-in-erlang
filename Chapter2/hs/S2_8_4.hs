module S2_8_4 where

data (Ord a , Ord b) => UsDefType3 a b = UsConst3 (a, b)

instance (Ord a, Ord b) => Eq (UsDefType3 a b)
    where
    UsConst3 (x, x') == UsConst3 (y,y') = (x==y)&&(x'==y')

instance (Ord a, Ord b) => Ord (UsDefType3 a b)
    where
    UsConst3 (x,x') <= UsConst3 (y,y') = if (x==y)
                                       then x'<= y'
                                       else x <= y

{----- Examples of evaluations and results

? UsConst3 (4, 'x') <= UsConst3 (5, 'x')
True
? UsConst3 (4, 'x') <= UsConst3 (4, 'x')
True
? UsConst3 (4, 'x') <= UsConst3 (4, 'w')
False

-----}
