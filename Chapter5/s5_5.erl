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

%% s = foldr addSet emptySet [2,5,1,3,7,5,3,2,1,9,0]
%% v = (delSet 1 s, inSet 8 s, inSet 0 s)
%% s' = foldr addSet emptySet [2,5,3,3,2,5,1]

%% {- examples of evaluations and results

%% ? s
%% {0,1,2,3,5,7,9}
%% ? s'
%% {1,2,3,5}
%% ? v
%% ({0,2,3,5,7,9}, False, True)
%% ? delSet 3 (addSet 3 emptySet)
%% {}
%% ? setEmpty s'
%% False
%% ? setEmpty (delSet 3 (addSet 3 emptySet))
%% True

%% -}
