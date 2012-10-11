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
-import(stack, [emptyStack/0, stackEmpty/1, push/2, pop/1, top/1]).
-export([s1/0]).

% s1 = push 1 (push 2 (push 3 emptyStack))
s1() -> push(1, push(2, push(3, emptyStack()))).

% {- examples of evaluations and results
%    with both implementations
% ? s1
% 1|2|3|-
% ? push 4 s1
% 4|1|2|3|-
% ? pop s1
% 2|3|-
% ? top s1
% 1
% ? stackEmpty s1
% False
% ? stackEmpty emptyStack
% True
% ? push "hello" (push "world" emptyStack)
% "hello"|"world"|-
% ----}

