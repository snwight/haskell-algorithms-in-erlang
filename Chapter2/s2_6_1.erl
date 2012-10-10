%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_6_1 where
-module(s2_6_1).
-export([xElt/1, yElt/1, firstQuad/1]).

% data CoordType = Coord Float Float
%     deriving Show
%% nb: erlang doesn't... oh forget it
-record(coord, {x, y}).

% xElt (Coord x _) = x
% yElt (Coord _ y) = y
xElt(#coord{x=X}) -> X.
yElt(#coord{y=Y}) -> Y.

% firstQuad [] = True
% firstQuad ((Coord x y):cs) = (x >= 0) && (y >= 0) && (firstQuad cs)
firstQuad([]) -> true;
firstQuad([#coord{x=X, y=Y}|CS]) ->
    if X >= 0, Y >= 0 -> firstQuad(CS);
       X < 0; Y < 0 -> false
    end.

% {----- Examples of evaluations and results
% ? xElt(#coord{x=14.0, y=12.0}).
% 14.0
% ? yElt(#coord{x=14.0, y=12.0).
% 12.0
% ? firstQuad([#coord{x=14.0, y=12.0}, #coord{x=15.0, y=-3.0}]).
% false
% ? firstQuad([#coord{x=14.0, y=12.0}, #coord{x=15.0, y=3.0}]).
% true
% -----}
