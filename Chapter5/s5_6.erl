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
%% f x | (x<3) = x
%%     | otherwise = (3-x)

%% t = newTable [(i,f i) | i<-[1..6] ] 

%% t1 = newTable [ (4,89) , (1,90) , (2, 67) ]

%% vtb = (findTable t 5, findTable (updTable (2,1) (updTable (3,4) t)) 3) 

%% {- examples of evaluations and results 

%% ----- Function implementation
%% ? t
%% <<A Table>>
%% ? t1
%% <<A Table>>
%% ? vtb
%% (-2, 4)

%% ------ List implementation
%% ? t
%% Tbl [(1, 1), (2, 2), (3, 0), (4, -1), (5, -2), (6, -3)]
%% ? t1
%% Tbl [(4, 89), (1, 90), (2, 67)]
%% ? vtb
%% (-2, 4)
%% ------- array implementation
%% ? t
%% Tbl (array (1, 6) [(1, 1), (2, 2), (3, 0), (4, -1), (5, -2), (6, -3)])
%% ? t1
%% Tbl (array (1, 4) [(1, 90), (2, 67), (3, 
%% Program error: {_undefined_array_element}

%% ? vtb
%% (-2, 4)
%% -}
