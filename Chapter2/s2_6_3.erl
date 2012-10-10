%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_6_3 where
-module(s2_6_3).
-export([condA/3, apply/2, id/1]).

% cond (x, y, z) = if x then y else z
condA(true, Y, _) -> Y;
condA(_, _, Z) -> Z.

% apply (f, x) = f x
apply(F, X) -> F(X).

id(X) -> X.

% {----- Examples of evaluations and results
% ? condA(true, 23, 25).
% 23
% ? apply(id, 23).
% 23
% ? apply(fun (Y) -> lists:nth(1, Y) end, [1234, 122.3]).
% 1234
% -----}
