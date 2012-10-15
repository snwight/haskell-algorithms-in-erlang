%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S6_3_2 where
-module(s6_3_2).
-export([insert/2, isort/1, insert1/2, isort1/1, isort2/1, ex/0]).

% -- Insertion Sort

% -- naive implementation

% insert :: (Ord a) => a -> [a] -> [a]
% insert x xs = takewhile (<= x) xs ++ [x] ++ dropwhile (<=x) xs
insert(X, XS) -> 
    lists:takewhile(fun(E)-> E =< X end, XS) ++	[X] ++ 
	lists:dropwhile(fun(E)-> E =< X end, XS).

% isort        :: (Ord a) => [a] -> [a]
% isort []     = []
% isort (x:xs) = insert x (isort xs) 
isort([]) -> [];
isort([X|XS]) -> insert(X, isort(XS)).

% -- more efficient implementation

% isort' xs = foldr insert [] xs 
isort1(XS) -> lists:foldr(fun insert/2, [], XS). 

% insert' key []                   = [key]
% insert' key l@(x:xs) | key <=x   = key : l
%                      | otherwise = x : (insert key xs)
insert1(Key, []) -> [Key];
insert1(Key, [X|XS]) when Key =< X -> [Key|[X|XS]];
insert1(Key, [X|XS]) -> [X|insert(Key, XS)].

% isort'' xs = foldr insert' [] xs
isort2(XS) -> lists:foldr(fun insert1/2, [], XS).

% ex = [3,1,4,1,5,9,2,8]
ex() -> [3,1,4,1,5,9,2,8].

% {-  Examples of evaluations and results 
%     (with number of reductions given by hugs +s)
% ? isort(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (351 reductions, 591 cells)
% ? isort1(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (343 reductions, 562 cells)
% ? isort2(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (343 reductions, 562 cells)
% -}
