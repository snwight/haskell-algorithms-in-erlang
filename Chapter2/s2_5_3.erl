%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_5_3 where
% import Prelude hiding (concat)
-module(s2_5_3).
-export([concat/1, revOntoA/2, revOntoB/2, consOnto/2, sumll/1, listDiff/2]).

% concat xs = lists:foldr (++) [] xs
concat(XS) -> lists:foldr(fun(H, T) -> H++T end, [], XS).

% revOnto' xs ys = (lists:reverse ys) ++ xs
revOntoA(XS, YS) -> lists:reverse(YS)++XS.

% consOnto xs y = y:xs
consOnto(XS, Y) -> [Y|XS].

% revOnto l1 l2 = lists:foldl consOnto l1 l2
revOntoB(L1, L2) -> lists:foldl(fun(Y, XS) -> consOnto(XS, Y) end, L1, L2).

% sumll xss = lists:foldl(lists:foldl (+)) 0 xss
sumll(XSS) -> 
    lists:foldl(
      fun(A,B) -> lists:foldl(
		    fun(C,D)-> C+D end, B, A) end, 0, XSS).

% listDiff xs1 xs2
%     = foldl del xs1 xs2
%       where del [] _                  = []
%             del (x:xs)  y | x == y    = xs
%                           | otherwise = x : (del xs y)
del(_, []) -> [];
del(Y, [X|XS]) ->
    if X =:= Y -> XS;
       X =/= Y -> [X|del(Y, XS)]
    end.
listDiff(XS1, XS2) -> lists:foldl(fun del/2, XS1, XS2).

% {----- Examples of evaluations and results
% ? concat([[1, 2, 3, 4], [5, 6, 7, 8]]).
% [1, 2, 3, 4, 5, 6, 7, 8]
% ? revOntoA([1, 2, 3], [4, 5, 6]).
% [6, 5, 4, 1, 2, 3]
% ? revOntoB([1, 2, 3], [4, 5, 6]).
% [6, 5, 4, 1, 2, 3]
% ? sumll([[1, 2, 3], [2, 3, 4]]).
% 15
% ? listDiff([1, 2, 3, 4, 5], [1, 3, 2]).
% [4, 5]
% -----}

