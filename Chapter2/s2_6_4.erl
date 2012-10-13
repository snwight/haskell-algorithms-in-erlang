%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_6_4 where
-module(s2_6_4).
-export([lengthIntList/1, lengthList/1, or1/1, numDigits/1]).

% data IntList = Nil
%              | Cons Int IntList
%     deriving Show

% lengthIntList  Nil = 0
% lengthIntList (Cons _ xs) = 1 + lengthIntList xs
lengthIntList([]) -> 0;
lengthIntList([_|XS]) -> 1 + lengthIntList(XS).

% data List a = Cons' a (List a)
% 	     | Nil'
%     deriving Show

% lengthList  Nil' = 0
% lengthList (Cons' _ xs) = 1 + lengthList xs
lengthList([]) -> 0;
lengthList([_|XS]) -> 1 + lengthList(XS).

% or' [] = False
% or' (True: _) = True
% or' (_:xs) = or' xs
or1([]) -> false;
or1([true|_]) -> true;
or1([_|XS]) -> or1(XS).

% numDigits [] = 0
% numDigits (c:cs) = (if (c >= '0') && (c <= '9') then 1 else 0)
%                    + numDigits cs
numDigits([]) -> 0;
numDigits([C|CS]) -> 
    if C >= '0', C =< '9' -> 1 + numDigits(CS);
       true -> numDigits(CS)
    end.

% {----- Examples of evaluations and results
% ? lengthIntList (Cons 3 (Cons 1 (Cons 4 Nil)))
% 3
% ? lengthList (Cons' 3 (Cons' 1 (Cons' 4 Nil')))
% 3
% ? lengthList (Cons' 'a' (Cons' 'c' (Cons' 'd' Nil')))
% 3
% ? or [True, 3>4]
% True
% ? or [False, 3>4]
% False
% ? or [False, 3<4]
% True
% ? numDigits ['1', 'a', '5', 'c']
% 2
% -----}
