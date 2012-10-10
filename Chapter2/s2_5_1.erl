%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S2_5_1 where
-module(s2_5_1).
-export([makeTuple/1, tupleListA/1, tupleListB/1, double/1]).
-export([doubleListA/1, doubleListB/1, one/1, lengthA/1]).
-import(lists, [map/2]).

% makeTuple x = (0, x)
makeTuple(X) -> {0, X}.

% tupleList []     = []
% tupleList (x:xs) = (makeTuple x):(tupleList xs)
tupleListA([]) -> [];
tupleListA([X|XS]) -> [makeTuple(X)|tupleListA(XS)].
 
% double x = x + x
double(X) -> X+X.

% doubleList []     = []
% doubleList (x:xs) = (double x):(doubleList xs)
doubleListA([]) -> [];
doubleListA([X|XS]) -> [double(X)|doubleListA(XS)].

% tupleList' xs = map makeTuple xs
tupleListB(XS) -> map(fun makeTuple/1, XS).

% doubleList' xs = map double xs
doubleListB(XS) -> map(fun double/1, XS).

% one _ = 1
one(_) -> 1.

% length' xs = sum (map one xs)
sum([]) -> 0;
sum([X|XS]) -> X + sum(XS).
lengthA(XS) -> sum(map(fun one/1, XS)).

% {----- Examples of evaluations and results
% ? tupleListA([1, 2, 3, 4]).
% [(0, 1), (0, 2), (0, 3), (0, 4)]
% ? tupleListB([1, 2, 3, 4]).
% [(0, 1), (0, 2), (0, 3), (0, 4)]
% ? doubleListA([1, 2, 3, 4]).
% [2, 4, 6, 8]
% ? doubleListB([1, 2, 3, 4]).
% [2, 4, 6, 8]
% ? lengthA([1, 2, 3, 4, 5]).
% 5
% -----}
