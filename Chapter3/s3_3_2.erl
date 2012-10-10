%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S3_3_2 where
-module(s3_3_2).
-export([average/1, lengthA/1, sumA/1, averageA/1]).

sum([]) -> 0;
sum([X|XS]) -> X + sum(XS).

% average xs = sum xs / fromInt(length xs)
average(XS) -> sum(XS) / length(XS).

% length' xs = lengthTR xs 0
%     where lengthTR [] r     = r
%           lengthTR (x:xs) r = strict (lengthTR xs) (r+1)
lengthTR([], R) -> R;
lengthTR([_|XS], R) -> lengthTR(XS, R+1).
lengthA(XS) -> lengthTR(XS, 0).

% sum' xs = sumTR xs 0
%     where sumTR [] r     = r
%           sumTR (x:xs) r = strict (sumTR xs) (r+x)
sumTR([], R) -> R;
sumTR([X|XS], R) -> sumTR(XS, R+X).
sumA(XS) -> sumTR(XS, 0).

% average' xs = sum' xs / fromInt(length' xs)
averageA(XS) -> sumA(XS) / lengthA(XS).
