%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S5_7 where
%% import BinTree
-module(s5_7).
-import(lists, [reverse/1, foldr/3]).
-import(bintree, [emptyTree/0, inTree/2, addTree/2, delTree/2]).
-import(bintree, [buildTree/1, buildTree1/1, inorder/1]).
-export([f/0, f2/0]).

%% f5_6 = buildTree (reverse [5,2,6,4,8,3,9])
f() -> buildTree(reverse([5,2,6,4,8,3,9])).

% fig5_6 = foldr addTree emptyTree (reverse [5,2,4,3,8,6,7,10,9,11])
f2() -> foldr(fun bintree:addTree/2, emptyTree(), 
	      reverse([5,2,4,3,8,6,7,10,9,11])).

% {- examples of evaluations and results 
% ? f().
% BTNode 5 (BTNode 2 BTEmpty (BTNode 4 (BTNode 3 BTEmpty BTEmpty) BTEmpty)) (BTNode 6 BTEmpty (BTNode 8 BTEmpty (BTNode 9 BTEmpty BTEmpty)))
% ? bintree:inTree(9, s5_7:f()).
% True
% ? bintree:delTree(5, s5_7:f()).
% BTNode 6 (BTNode 2 BTEmpty (BTNode 4 (BTNode 3 BTEmpty BTEmpty) BTEmpty)) (BTNode 8 BTEmpty (BTNode 9 BTEmpty BTEmpty))
% ? bintree:addTree(1, s5_7:f()).
% BTNode 5 (BTNode 2 (BTNode 1 BTEmpty BTEmpty) (BTNode 4 (BTNode 3 BTEmpty BTEmpty) BTEmpty)) (BTNode 6 BTEmpty (BTNode 8 BTEmpty (BTNode 9 BTEmpty BTEmpty)))
% -}
