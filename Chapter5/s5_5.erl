%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S5_5 where
%% import Set
-module(s5_5).
-export([s1/0, s2/0, v/0, t1/0, t2/0, t3/0]).

% s = foldr addSet emptySet [2,5,1,3,7,5,3,2,1,9,0]
% v = delSet 1 s, inSet 8 s, inSet 0 s)
% s' = foldr addSet emptySet [2,5,3,3,2,5,1]
s1() -> lists:foldr(fun set:addSet/2, set:emptySet(), [2,5,1,3,7,5,3,2,1,9,0]).
v() -> {set:delSet(1, s5_5:s1()), 
	set:inSet(8, s5_5:s1()), 
	set:inSet(0, s5_5:s1())}.
s2() -> lists:foldr(fun set:addSet/2, set:emptySet(), [2,5,3,3,2,5,1]).
t1() -> set:delSet(3, set:addSet(3, set:emptySet())).
t2() -> set:setEmpty(s2()).
t3() -> set:setEmpty(set:delSet(3, set:addSet(3, set:emptySet()))).

% {- examples of evaluations and results
% ? s1().
% {0,1,2,3,5,7,9}
% ? s2().
% {1,2,3,5}
% ? v().
% ({0,2,3,5,7,9}, False, True)
% ? t1().
% {}
% ? t2().
% False
% ? t3().
% True
% -}
