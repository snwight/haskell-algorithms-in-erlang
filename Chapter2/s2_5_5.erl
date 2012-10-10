%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_5_5 where
-module(s2_5_5).
-export([double/1, square/1, bump/1, applyall/2]).

% double x = x* 2
double(X) -> X*2.

% square x = x * x
square(X) -> X*X.

% bump x = x + 1
bump(X) -> X+1.

% applyall [] x = x
% applyall (f:fs) x = f (applyall fs x)
applyall([], X) -> X;
applyall([F|FS], X) -> F(applyall(FS, X)).

% {----- Examples of evaluations and results
% ? applyall([fun s2_5_5:bump/1, fun s2_5_5:square/1, fun s2_5_5:double/1], 3).
% 37
% -----}
