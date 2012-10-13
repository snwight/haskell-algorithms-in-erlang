%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module s5_2 where
% import Stack
-module(s5_2).
-export([s1/0]).

% s1 = push 1 (push 2 (push 3 emptyStack))
s1() -> stack:push(1, stack:push(2, stack:push(3, stack:emptyStack()))).

% {- examples of evaluations and results
%    with both implementations
% ? s1().
% 1|2|3|-
% ? stack:push(4, s1()).
% 4|1|2|3|-
% ? stack:pop(s1()).
% 2|3|-
% ? stack:top(s1()).
% 1
% ? stack:stackEmpty(s1()).
% False
% ? stack:stackEmpty(emptyStack()).
% True
% ? stack:push("hello", (stack:push("world", stack:emptyStack()).
% "hello"|"world"|-
% ----}

