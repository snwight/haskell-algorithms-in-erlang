%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S5_4 where
% import PQueue
-module(s5_4).
-import(pqueue, [enPQ/2, emptyPQ/0, frontPQ/1, dePQ/1]).
-export([pq/0, v/0]).

% pq = foldr enPQ emptyPQ [3,1,7,2,9]
pq () -> lists:foldr(fun pqueue:enPQ/2, emptyPQ(), [3,1,7,2,9]).

%% v = (frontPQ pq, dePQ pq)
v() -> {frontPQ(pq()), dePQ(pq())}.

%% {-  examples of evaluations and results

%% ? pq
%% PQ (HP 1 2 (HP 2 2 (HP 9 1 EmptyHP EmptyHP) (HP 7 1 EmptyHP EmptyHP)) (HP 3 1 EmptyHP EmptyHP))
%% ? v
%% (1, PQ (HP 2 2 (HP 9 1 EmptyHP EmptyHP) (HP 3 1 (HP 7 1 EmptyHP EmptyHP) EmptyHP)))

%% -}
