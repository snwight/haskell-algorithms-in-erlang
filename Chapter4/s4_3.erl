%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% nb: PUNTED - array module is a square peg for round hole. Later!
%%

%module S4_3 where

% import Array

% row :: (Ix a, Ix b) => a -> Array (a,b) c -> Array b c
% row i m = ixmap (l', u') (\j->(i,j)) m
%     where ((l,l'),(u,u'))= bounds m

% col :: (Ix a, Ix b) => a -> Array (b,a) c -> Array b c
% col j m = ixmap (l, u) (\i->(i,j)) m
%     where ((l,l'),(u,u'))= bounds m

% inner :: (Ix a, Num b) => Array a b -> Array a b -> b
% inner v w = sum [v!i * w!i | i<-indices v]

% matMult :: (Num a, Ix a, Enum a, Num b, Ix b, Num c, Num d, Ix d, Enum d)
%            => Array (a,b) c -> Array (b,d) c -> Array (a,d) c
% matMult a b = array ((1,1),(m,n))
%               [((i,j), inner (row i a) (col j b)) | i<-[1..m],j<-[1..n]]
%     where ((1,1),(m,p)) = bounds a
%           ((1,1),(p',n))= bounds b 

% -- examples of arrays for testing
% squares n = array (1,n) [(i,i*i)|i<-[1..n]]
% m = array ((1,1),(2,3)) [((i, j), (i*j)) | i <- [1..2], j <- [1..3]]
% m'= array ((1,1),(3,2)) [((j, i), (i*j)) | i <- [1..2], j <- [1..3]]
% {-  Examples of evaluations and results
% ? squares 5
% array (1, 5) [(1, 1), (2, 4), (3, 9), (4, 16), (5, 25)]
% ? m
% array ((1, 1), (2, 3)) [((1, 1), 1), ((1, 2), 2), ((1, 3), 3), ((2, 1), 2), ((2, 2), 4), ((2, 3), 6)]
% ? map (\x->10*x) (squares 5)
% array (1, 5) [(1, 10), (2, 40), (3, 90), (4, 160), (5, 250)]
% ? row 2 m
% array (1, 3) [(1, 2), (2, 4), (3, 6)]
% ? col 2 m
% array (1, 2) [(1, 2), (2, 4)]
% ? inner (squares 3) (squares 3)
% 98
% ? matMult m m'
% array ((1, 1), (2, 2)) [((1, 1), 14), ((1, 2), 28), ((2, 1), 28), ((2, 2), 56)]
% -}
