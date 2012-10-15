%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S6_3_3 where
-module(s6_3_3).
-export([qsort/1, qsort1/2, qsort2/1, qs/2, split/5, ex/0]).

% -- QuickSort

% -- naive implementation
% qsort :: (Ord a) => [a] -> [a]
% qsort [] = []
% qsort (pivot:rest) = qsort lower ++ [pivot] ++ qsort upper
%     where lower = [ x | x <- rest, x<= pivot]
%           upper = [ x | x <- rest, x > pivot] 
qsort([]) -> [];
qsort([Pivot|Rest]) ->
    Lower = [ X || X <- Rest, X =< Pivot],
    Upper = [ X || X <- Rest, X > Pivot],
    qsort(Lower) ++ [Pivot] ++ qsort(Upper).

% -- improved implementation
% qsort' :: (Ord a) => [a] -> [a] -> [a]
% qsort' [] s = s
% qsort' (pivot:rest) s = qsort' lower (pivot : (qsort' upper s))
%     where lower = [ x | x <- rest, x<= pivot]
%           upper = [ x | x <- rest, x > pivot] 
qsort1([], S) -> S;
qsort1([Pivot|Rest], S)->
    Lower = [X || X <- Rest, X =< Pivot],
    Upper = [X || X <- Rest, X > Pivot],
    qsort1(Lower, [Pivot|qsort1(Upper, S)]).

% -- more efficient implementation
% --   note this is different from the book version
% --   it is the correct and more efficient version 
% qsort'' l = qs l []
%     where qs []  s          = s
%           qs [x] s          = (x:s)
%           qs (pivot:rest) s = split pivot rest [] [] s
%           split pivot [] lower upper s     
%             = qs lower (pivot : (qs upper s))
%           split pivot (x:xs) lower upper s 
%             = if x < pivot
%               then split pivot xs (x:lower) upper s
%               else split pivot xs lower (x:upper) s
qs([],  S) -> S;
qs([X], S) -> [X|S];
qs([Pivot|Rest], S) -> split(Pivot, Rest, [], [], S).
split(Pivot, [], Lower, Upper, S) -> qs(Lower, [Pivot|qs(Upper, S)]);
split(Pivot, [X|XS], Lower, Upper, S) ->
    if X < Pivot -> split(Pivot, XS, [X|Lower], Upper, S);
       true -> split(Pivot, XS, Lower, [X|Upper], S)
    end.
qsort2(L) -> qs(L, []).

% ex = [3,1,4,1,5,9,2,8]
ex() -> [3,1,4,1,5,9,2,8].

% {-  Examples of evaluations and results 
%     (with number of reductions given by hugs +s)
% ? qsort ex
% [1, 1, 2, 3, 4, 5, 8, 9]
% (380 reductions, 679 cells)
% ? qsort' ex []
% [1, 1, 2, 3, 4, 5, 8, 9]
% (351 reductions, 624 cells)
% ? qsort'' ex
% [1, 1, 2, 3, 4, 5, 8, 9]
% (160 reductions, 389 cells)
% -}
