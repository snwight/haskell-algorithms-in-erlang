%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_4_5 where
% import Prelude hiding (concat)
% import List -- necessary for the definition of (\\)
-module(s2_4_5).
-export([concat/1, removeDups/1, rem34A/1, rem34B/1, perms/1]).

% concat []	    = []
% concat ([]:ys)	    = concat ys
% concat ((x:xs):ys) = x:concat (xs:ys)
concat([]) -> [];
concat([[]|YS]) -> concat(YS);
concat([[X|XS]|YS]) -> [X|concat([XS|YS])].

% removeDups []	                 = []
% removeDups [x]	                 = [x]
% removeDups  (x:y:ys) | x == y    = removeDups (y:ys)
% 		     | otherwise = x:removeDups (y:ys)
removeDups([]) -> [];
removeDups([X]) -> [X];
removeDups([X|[Y|YS]]) ->
    if X =:= Y -> removeDups([Y|YS]);
       X =/= Y -> [X|removeDups([Y|YS])]
    end.

% rem34 (p:q:r:s:xs) | r == s    = p:q:xs
% 		   | otherwise = p:q:r:s:xs
rem34A([P|[Q|[R|[S|XS]]]]) ->
    if  R =:= S -> [P|[Q|XS]];
	R =/= S -> [P|[Q|[R|[S|XS]]]]
    end.

% rem34' lab@(p:q:r:s:xs) | r == s    = p:q:xs
% 			| otherwise = lab
rem34B(Pqrsxs) ->
    % highly debatable pedagogically or pragmatically  
    [P|[Q|[R|[S|XS]]]] = Pqrsxs,
    if R =:= S -> [P|[Q|XS]];
       R =/= S -> Pqrsxs
    end.

% perms [] = [[]]
% perms xs = [x:p | x <- xs, p <- perms (removeFirst x xs)]
%     where removeFirst x []                 = []
%           removeFirst x (y:ys) | x == y    = ys
%                                | otherwise = y : removeFirst x ys
%% nb: no LET construct in erlang, and we can't recurse in fun() so:
removeFirst(_, []) -> [];
removeFirst(X, [Y|YS]) ->
    if X =:= Y -> YS;
       X =/= Y -> [Y | removeFirst(X, YS)]
    end.
perms([]) -> [[]];
perms(XS) -> [[X|P] || X <- XS, P <- perms(removeFirst(X, XS))].

% {----- examples of evaluations and results
% ? concat([[1, 2, 3, 4], [5, 6, 7], [8, 9]]).
% [1, 2, 3, 4, 5, 6, 7, 8, 9]
% ? removeDups([1, 2, 3, 3, 1, 2, 4, 5, 5]).
% [1, 2, 3, 1, 2, 4, 5]
% ? rem34A([1, 2, 2, 2, 2, 2, 2, 2]).
% [1, 2, 2, 2, 2, 2]
% ? rem34B([1, 2, 2, 2, 2, 2, 2, 2]).
% [1, 2, 2, 2, 2, 2]
% ? perms(lists:seq(1,3)).
% [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
% -----}
