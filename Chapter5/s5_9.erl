%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S5_9 where
% import AVLTree
-module(s5_9).
-export([t/0]).

% {- examples of calls and results
% ? foldr addAVL emptyAVL [7,6..1]
t() -> lists:foldr(fun avltree:addAVL/2, avltree:emptyAVL(), lists:seq(7,1, -2)).
% BTNode 4 (BTNode 2 (BTNode 1 BTEmpty BTEmpty) (BTNode 3 BTEmpty BTEmpty)) (BTNode 6 (BTNode 5 BTEmpty BTEmpty) (BTNode 7 BTEmpty BTEmpty))
%% -}
