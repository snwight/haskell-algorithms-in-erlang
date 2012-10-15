%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S6_3_4 where
% import List
-module(s6_3_4).
-import(lists, [sublist/2, nthtail/2]).
-export([msort/1, msort1/1, msort2/1, merge/2, mergepairs/1, ms/1, ex/0]).

% -- Merge Sort

% -- naive implementation
% msort :: (Ord a) => [a] -> [a]
% msort [] = []
% msort [x] = [x]
% msort l = merge (msort l1)(msort l2)
%            where l1 = (take k l )
%                  l2 = (drop k l)
%                  k  = (length l) `div` 2 
% msort :: (Ord a) => [a] -> [a]
msort([]) -> [];
msort([X]) -> [X];
msort(L) ->
    K = length(L) div 2,
    L1 = sublist(L, K),
    L2 = nthtail(K, L),
    merge(msort(L1), msort(L2)).

% merge :: (Ord a) => [a] -> [a] -> [a]
% merge [] b = b
% merge a [] = a
% merge a@(x:xs) b@(y:ys) | (x<=y)    = x : (merge xs b)
%                         | otherwise = y : (merge a ys)
merge([], B) -> B;
merge(A, []) -> A;
merge([X|XS], [Y|YS]) when X =< Y -> [X|merge(XS, [Y|YS])];
merge([X|XS], [Y|YS]) when X > Y -> [Y|merge([X|XS], YS)].

% -- improved implementation
% msort' :: (Ord a) => [a] -> [a]
% msort' [] = []
% msort' [x] = [x]
% msort' l = merge (msort' l1) (msort' l2)
%            where l1 = (take k l )
%                  l2 = (drop k l)
%                  k  = (length l) `div` 2 
% -- improved implementation
msort1([]) -> [];
msort1([X]) -> [X];
msort1(L) ->
    K  = length(L) div 2, 
    L1 = sublist(L, K),
    L2 = nthtail(K, L),
    merge(msort1(L1), msort1(L2)).

% -- more efficient implementation

% split :: (Ord a) => [a] -> [[a]]
% split []     = []
% split (x:xs) = [x] : split xs
split([]) -> [];
split([X|XS]) -> [[X]|split(XS)].

% mergepairs :: (Ord a) => [[a]] -> [[a]]
% mergepairs []           = []
% mergepairs x@[l]        = x
% mergepairs (l1:l2:rest) = (merge l1 l2) : (mergepairs rest)
mergepairs([]) -> [];
mergepairs([L]) -> L;
mergepairs([L1|[L2|Rest]]) -> [merge(L1, L2)|mergepairs(Rest)].

% msort'' :: (Ord a) => [a] -> [a]
% msort'' l = ms (split l)
%          where ms [r] = r
%                ms l   = ms (mergepairs l)
ms([R]) -> R;
ms(L) -> ms(mergepairs(L)).
msort2(L) -> ms(split(L)).

% ex = [3,1,4,1,5,9,2,8]
ex() -> [3,1,4,1,5,9,2,8].

% {-  Examples of evaluations and results 
%     (with number of reductions given by hugs +s)
% ? msort(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (638 reductions, 1022 cells)
% ? msort1(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (627 reductions, 994 cells)
% ? msort2(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (199 reductions, 380 cells)
% -}
