%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S2_3_3 where
-module(s2_3_3).
-export([isB/1]).

% isB :: Char -> Bool
% isB c = (c == 'B') || (c == 'b')
%% nb: again, actually demonstrating guard patterns, not type system!
isB(B) when B =:= 'B' orelse B =:= 'b' -> true;
isB(_) -> false.
