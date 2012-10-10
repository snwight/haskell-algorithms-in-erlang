module S2_6_1 where

data CoordType = Coord Float Float
    deriving Show

xElt (Coord x _) = x
yElt (Coord _ y) = y

firstQuad [] = True
firstQuad ((Coord x y):cs) = (x >= 0) && (y >= 0) && (firstQuad cs)

{----- Examples of evaluations and results

? xElt (Coord 14.0 12.0)
14.0
? yElt (Coord 14.0 12.0)
12.0
? firstQuad [Coord 14.0 12.0, Coord 15.0 (-3.0)]
False
? firstQuad [Coord 14.0 12.0, Coord 15.0 3.0]
True

-----}
