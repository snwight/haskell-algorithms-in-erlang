%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S5_7 where

%% import BinTree

%% f5_6 = buildTree (reverse [5,2,6,4,8,3,9])

%% {- examples of evaluations and results 

%% ? f5_6
%% BTNode 5 (BTNode 2 BTEmpty (BTNode 4 (BTNode 3 BTEmpty BTEmpty) BTEmpty)) (BTNode 6 BTEmpty (BTNode 8 BTEmpty (BTNode 9 BTEmpty BTEmpty)))
%% ? inTree 9 f5_6
%% True
%% ? delTree 5 f5_6
%% BTNode 6 (BTNode 2 BTEmpty (BTNode 4 (BTNode 3 BTEmpty BTEmpty) BTEmpty)) (BTNode 8 BTEmpty (BTNode 9 BTEmpty BTEmpty))
%% ? addTree 1 f5_6
%% BTNode 5 (BTNode 2 (BTNode 1 BTEmpty BTEmpty) (BTNode 4 (BTNode 3 BTEmpty BTEmpty) BTEmpty)) (BTNode 6 BTEmpty (BTNode 8 BTEmpty (BTNode 9 BTEmpty BTEmpty)))

%% -}
