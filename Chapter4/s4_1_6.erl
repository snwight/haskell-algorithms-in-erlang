%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S4_1_6 where
-module(s4_1_6).
-export([average/1, averageA/1, averageB/1]).

sum([]) -> 0;
sum([X|XS]) -> X + sum(XS).

% average xs = sum xs / fromInt (length xs)
average(XS) -> sum(XS) / length(XS).

% average' xs = s / fromInt n
%     where (s,n) = av xs
%           av []     = (0,0)
%           av (x:xs) = (x+s,n+1)
%               where (s,n) = av xs
av([]) -> {0,0};
av([X|XS]) -> {S,N} = av(XS), {X+S, N+1}.
averageA(XS) -> {S,N} = av(XS), S/N.

% average'' xs = av' xs 0 0
%     where av' []     s n = s / fromInt n
%           av' (x:xs) s n = av' xs (x+s) (n+1)
avA([], S, N) -> S / N;
avA([X|XS], S, N) -> avA(XS, X+S, N+1).
averageB(XS) ->	avA(XS, 0, 0).
