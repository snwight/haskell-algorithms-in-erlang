module S2_6_2 where

import S2_6_1

data Shape = Rectangle CoordType CoordType
	   | Circle CoordType Float
	   | Triangle CoordType CoordType CoordType
    deriving Show
		
area (Rectangle corner1 corner2)
    = abs(xElt corner1 - xElt corner2)*
      abs(yElt corner1 - yElt corner2)
area (Circle _ radius)
    = pi*radius*radius
area (Triangle vert1 vert2 vert3)
    = sqrt (h*(h-a)*(h-b)*(h-c))
      where
      h = (a+b+c)/2.0
      a = dist vert1 vert2
      b = dist vert1 vert3
      c = dist vert2 vert3
      dist (Coord x1 y1) (Coord x2 y2)
	  = sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))


{----- Examples of evaluations and results

? area (Rectangle (Coord 13.0 27.3) (Coord 4.9 12.1))
123.12
? area (Circle (Coord 64.0 37.0) 7.5)
176.715
? area (Triangle (Coord 0.0 0.0) (Coord 45.3 6.0) (Coord 12.8 17.0))
346.65

-----}
