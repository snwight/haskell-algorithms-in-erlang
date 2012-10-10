%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_2_1 where
-module(s2_2_1).
-export([factA/1, factB/1, factC/1, checkVal/2]).

% fact n = if (n == 0)
% 	 then 1
% 	 else n * fact (n-1)
factA(N) ->
    if N =:= 0 -> 1;
       N =/= 0 -> N * factA(N-1)
    end.

% fact' n | n == 0 = 1
% 	| otherwise = n * fact (n - 1)
factB(N) when N =:= 0 -> 1;
factB(N) -> N * factB(N-1). 

% fact'' 0 = 1
% fact'' n = n * fact (n - 1)
factC(0) -> 1; 
factC(N) -> N * factC(N-1).

% checkVal x y | x > y  = 1
% 	     | x < y  = -1
% 	     | x == y = 0
checkVal(X,Y) when X > Y -> 1;
checkVal(X,Y) when X < Y -> -1;
checkVal(X,Y) when X =:= Y -> 0.

%% {-----  examples of evaluations and results
%% ? factA(10).
%% 3628800
%% ? factB(10).
%% 3628800
%% ? factC(10).
%% 3628800
%% ? checkVal(10, 20).
%% -1
%% ? checkVal(10 ,10).
%% 0
%% ? checkVal(20, 10).
%% 1
%% -----}

