%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_5_2 where
-module(s2_5_2).
-export([tupleList/1, doubleListA/1, doubleListB/1]).

% tupleList xs = map (\x -> (0, x)) xs
tupleList(XS) -> lists:map(fun(X) -> {0, X} end, XS).

% doubleList xs = map (\x -> x + x) xs
doubleListA(XS) -> lists:map(fun(X) -> X + X end, XS).

% doubleList' xs = map ((*)2) xs
doubleListB(XS) -> lists:map(fun(X) -> X * 2 end, XS).

% {----- Examples of evaluations and results
% ? tupleList([1, 2, 3, 4, 5]).
% [{0,1},{0,2},{0,3},{0,4},{0,5}]
% ? doubleListA([1, 2, 3, 4, 5]).
% [2, 4, 6, 8, 10]
% ? doubleListB([1, 2, 3, 4, 5]).
% [2, 4, 6, 8, 10]
% -----}


