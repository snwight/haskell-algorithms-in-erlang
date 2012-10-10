%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module 2_6_2 where
% import S2_6_1
-module(s2_6_2).
-export([area/1, area/2, area/3]).
-import(math,[sqrt/1, pi/0]).

% data Shape = Rectangle CoordType CoordType
% 	   | Circle CoordType Float
% 	   | Triangle CoordType CoordType CoordType
%     deriving Show
%% nb: actually erlang doesn't... just forget it
%% I proceed here to just implement these functions using just one
%% constructed "type" rather than annoying records, with all their cruft
-record(coord, {x, y}).

% area (Circle _ radius) = pi*radius*radius
area(Radius) -> pi() * Radius * Radius.

% area (Rectangle corner1 corner2)
%     = abs(xElt corner1 - xElt corner2)* 
%       abs(yElt corner1 - yElt corner2)
area(#coord{x=X1, y=Y1}, #coord{x=X2, y=Y2}) -> abs(X1 - X2) * abs(Y1 - Y2).

% area (Triangle vert1 vert2 vert3)
%     = sqrt (h*(h-a)*(h-b)*(h-c))
%       where
%       h = (a+b+c)/2.0
%       a = dist vert1 vert2
%       b = dist vert1 vert3
%       c = dist vert2 vert3
%       dist (Coord x1 y1) (Coord x2 y2)
% 	  = sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
area(V1=#coord{}, V2=#coord{}, V3=#coord{}) ->
    Dist = fun(#coord{x=X1, y=Y1}, #coord{x=X2, y=Y2}) ->
		   sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2))
	   end,
    A = Dist(V1, V2), B = Dist(V1, V3), C = Dist(V2, V3),
    H = (A+B+C)/2.0,
    sqrt(H*(H-A)*(H-B)*(H-C)).

% {----- Examples of evaluations and results
% ? area(#coord{x=13.0, y=27.3}, #coord{x=4.9, y=12.1}.
% 123.12
% ? area(7.5).
% 176.715
% ? area(#coord{x=0.0, y=0.0}, #coord{x=45.3, y=6.0}, #coord{x=12.8, y=17.0}).
% 346.65
% -----}
