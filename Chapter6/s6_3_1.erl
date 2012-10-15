%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S6_3 where
% -- Selection Sort
% import List
-module(s6_3_1).
-import(list,[]).
-export([ssort/1, ssort1/1, split/3, ex/0]).

% -- naive implementation

% ssort :: (Ord a) => [a] -> [a]
% ssort [] = []
% ssort xs = m : ssort (delete m xs)
%            where m = minimum xs    
ssort([]) -> [];
ssort(XS) -> M = lists:min(XS), [M|ssort(lists:delete(M, XS))].

% -- more efficient implementation

% split :: (Ord a) => [a] -> a -> [a] -> [a]
% split [] m r = m : (ssort r)
% split (x:xs) m r = if x < m
%                    then split xs x (m:r)
%                    else split xs m (x:r)
split([], M, R) -> [M|ssort(R)];
split([X|XS], M, R) when X < M -> split(XS, X, [M|R]);
split([X|XS], M, R) when X >= M -> split(XS, M, [X|R]).

% ssort' [] = []
% ssort' (x:xs) = split xs x []
ssort1([]) -> [];
ssort1([X|XS]) -> split(XS, X, []).

% ex = [3,1,4,1,5,9,2,8]
ex() -> [3,1,4,1,5,9,2,8].

% {-  Examples of evaluations and results 
%     (with number of reductions given by hugs +s)
% ? ssort(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (358 reductions, 584 cells)
% ? ssort1(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (333 reductions, 572 cells)
% -}
