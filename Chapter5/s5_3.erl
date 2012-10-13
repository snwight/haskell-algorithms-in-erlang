%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S5_3 where
% import Queue
-module(s5_3).
-export([test/0]).

% test = foldr enqueue emptyQueue [1..10]
test() ->
    queue:showQ(lists:foldr(fun queue:enqueue/2, 
			    queue:emptyQueue(), lists:seq(1,10))).

% {-  examples of evaluations and results
% ? test
% Q [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
% -}
