%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_3_4 where
-module(s2_3_4).
-export([later/2, distance/2, roots/3]).
-export([t1/0,t2/0,t3/0]).
-import(math, [sqrt/1, pow/2]).

% t1 = (3, True)
% t2 = (14.8, 'd', 34)
% t3 = ((True, "hello"), False, (112, 16.821))
%% functional constants... 
t1() -> { 3, true }.
t2() -> { 14.8, d, 34 }.
t3() -> { {true, hello}, false, {112, 16.821} }.

% later (h1, m1, s1) (h2, m2, s2)
%     | h1 < h2   = False
%     | h1 > h2   = True
%     | m1 < m2   = False
%     | m1 > m2   = True
%     | otherwise = s1 > s2
later({H1, M1, S1}, {H2, M2, S2}) ->
    if H1 < H2 -> false;
       H1 > H2 -> true;
       M1 < M2 -> false;
       M1 > M2 -> true;
       true -> S1 > S2
    end.

% distance (x1, y1) (x2, y2)
%     = sqrt (dx * dx + dy * dy)
%       where
%       dx = x2 - x1
%       dy = y2 - y1
distance({X1, Y1}, {X2, Y2}) ->
    DX = X2 - X1,
    DY = Y2 - Y1,
    sqrt(DX * DX + DY * DY).

% roots (a, b, c) = (r1, r2)
%     where
%     r1         = (-b + r) / f
%     r2         = (-b - r) / f
%     f          = 2 * a
%     r | d >= 0 = sqrt d
%       | d < 0  = error "imaginary roots"
%     d          = b * b - 4 * a * c
roots(A, B, C) ->
    D = B * B - 4 * A * C,
    if D < 0 -> erlang:error("imaginary roots!!");
       D >= 0 -> {(-B + sqrt(D)) / 2 * A, (-B - sqrt(D)) / 2 * A}
    end.

% {----- Examples of evaluations and results
% ? :set +t
% ? t1
% (3, True) :: (Int,Bool)
% ? t2
% (14.8, 'd', 34) :: (Double,Char,Int)
% ? t3
% ((True, "hello"), False, (112, 16.821)) :: ((Bool,[Char]),Bool,(Int,Double))
% ? :set -t
% ? later (12,34,56) (4,56,34)
% True
% ? distance (1,0) (5,5)
% 6.40312
% ? roots (1,1,1)
% (
% Program error: imaginary roots
% ? roots (1,2,1)
% (-1.0, -1.0)
% ? roots (1,1,-2)
% (1.0, -2.0)
% -----}
