%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_2 where
-module(s2_2).
-export([e/0, area/1, stirling/1, volumeA/1, volumeB/1, product/1]).
-import(math, [pi/0, pow/2, sqrt/1]).

% e = 2.717281828
e() -> 2.718281828.

% area r  = pi * r * r
area(R) -> pi() * R * R.

% stirling n = (n/e)**n * sqrt(2*pi*n)
stirling(N) -> pow(N/e(), N) * sqrt(2*pi() * N).

% volume r = 4.0 / 3.0 * pi * (cube r)
%     where
%     cube x = x * x * x
%% no WHERE...IN construct in erlang, so this is not really instructional:
volumeA(R) -> 4.0 / 3.0 * pi() * pow(R, 3).

% volume' r = let
% 	       cube x = x * x * x
% 	    in
% 	       4.0 / 3.0 * pi * (cube r)
volumeB(R) ->
    Cube = fun(X) -> X*X*X end,
    4.0 / 3.0 * pi() * Cube(R).

% product [] = 1
% product x:xs = x * product xs
product([]) -> 1;
product([X|XS]) -> X * product(XS).
 
%% {----- examples of evaluations and results
%% ? area(10).
%% 314.159
%% ? stirling(10).
%% 3.61197e+06
%% ? product(lists:seq([1,10])).
%% 3628800
%% ? volumeA(10).
%% 4188.79
%% ? volumeB(10).
%% 4188.79
%% -------}
