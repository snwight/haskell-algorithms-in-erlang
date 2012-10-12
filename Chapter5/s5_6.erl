%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S5_6 where
%% import Table
-module(s5_6).
-import(table,[newTable/1, findTable/2, updTable/2]).
-import(table,[newTableL/1, findTableL/2, updTableL/2]).
-export([f/1, t/0, t1/0, vtb/0]).
-export([tL/0, t1L/0, vtbL/0]).

% f x | (x<3) = x
%     | otherwise = (3-x)
f(X) when X < 3 -> X;
f(X) when X >= 3 -> 3 - X.

%% nb: FUNCTION implementation tests
% t = newTable [(i,f i) | i<-[1..6] ] 
t() -> newTable([{I, f(I)} || I <- lists:seq(1,6) ]).

% t1 = newTable [ (4,89) , (1,90) , (2, 67) ]
t1() -> newTable( [ {4,89}, {1,90}, {2, 67} ] ).

% vtb = (findTable t 5, findTable (updTable (2,1) (updTable (3,4) t)) 3) 
vtb() -> {findTable(t(), 5), 
	  findTable(updTable({2,1}, updTable({3,4}, t())), 3)}. 

%% nb LIST implementation tests
tL() -> newTableL([{I, f(I)} || I <- lists:seq(1,6) ]).
t1L() -> newTableL( [ {4,89}, {1,90}, {2, 67} ] ).
vtbL() -> [findTableL(tL(), 5),
	  findTableL(updTableL({2,1}, updTableL({3,4}, tL())), 3)].

% {- examples of evaluations and results 
% ----- Function implementation
% ? t().
% <<A Table>>
% ? t1().
% <<A Table>>
% ? vtb().
% (-2, 4)
% ------ List implementation
% ? tL().
% Tbl [(1, 1), (2, 2), (3, 0), (4, -1), (5, -2), (6, -3)]
% ? t1L().
% Tbl [(4, 89), (1, 90), (2, 67)]
% ? vtbL().
%%?????????? WHYYYYYYYY????
% (-2, 2)
%%?????????? WHYYYYYYYY????

%% NB: ignored ------- array implementation
%% ? t
%% Tbl (array (1, 6) [(1, 1), (2, 2), (3, 0), (4, -1), (5, -2), (6, -3)])
%% ? t1
%% Tbl (array (1, 4) [(1, 90), (2, 67), (3, 
%% Program error: {_undefined_array_element}
%% ? vtb
%% (-2, 4)
% -}
