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
-import(queue, 
	[emptyQueue/0, queueEmpty/1, enqueue/2, dequeue/1, front/1, showQ/1]).
-export([test/0]).

% test = foldr enqueue emptyQueue [1..10]
test() ->
    showQ(lists:foldr(fun queue:enqueue/2, emptyQueue(), lists:seq(1,10))).

% {-  examples of evaluations and results
% ? test
% Q [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
% -}
