%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module S5_8 where
%% import Heap
%% import PQueue
-module(s5_8).

%% {- examples of calls and results
%% S5_8> heap:insHeap(3, heap:emptyHeap()).
%% HP 3 1 EmptyHP EmptyHP
%% S5_8> heap:insHeap(1, heap:insHeap(3, heap:emptyHeap())). 
%% HP 1 1 (HP 3 1 EmptyHP EmptyHP) EmptyHP
%% S5_8> insHeap 4 $$
%% HP 1 2 (HP 3 1 EmptyHP EmptyHP) (HP 4 1 EmptyHP EmptyHP)
%% S5_8> insHeap 1 $$
%% HP 1 1 (HP 1 2 (HP 3 1 EmptyHP EmptyHP) (HP 4 1 EmptyHP EmptyHP)) EmptyHP
%% S5_8> insHeap 5 $$
%% HP 1 2 (HP 1 2 (HP 3 1 EmptyHP EmptyHP) (HP 4 1 EmptyHP EmptyHP)) (HP 5 1 EmptyHP EmptyHP)
%% S5_8> insHeap 9 $$
%% HP 1 2 (HP 1 2 (HP 3 1 EmptyHP EmptyHP) (HP 4 1 EmptyHP EmptyHP)) (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP)
%% S5_8> insHeap 2 $$
%% HP 1 2 (HP 1 2 (HP 3 1 EmptyHP EmptyHP) (HP 4 1 EmptyHP EmptyHP)) (HP 2 1 (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP) EmptyHP)
%% S5_8> insHeap 6 $$
%% HP 1 3 (HP 1 2 (HP 3 1 EmptyHP EmptyHP) (HP 4 1 EmptyHP EmptyHP)) (HP 2 2 (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP) (HP 6 1 EmptyHP EmptyHP))
%% S5_8> delHeap $$
%% HP 1 2 (HP 2 2 (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP) (HP 4 1 (HP 6 1 EmptyHP EmptyHP) EmptyHP)) (HP 3 1 EmptyHP EmptyHP)
%% S5_8> delHeap $$
%% HP 2 2 (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP) (HP 3 1 (HP 4 1 (HP 6 1 EmptyHP EmptyHP) EmptyHP) EmptyHP)
%% S5_8> delHeap $$
%% HP 3 2 (HP 4 1 (HP 6 1 EmptyHP EmptyHP) EmptyHP) (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP)
%% S5_8> delHeap $$
%% HP 4 2 (HP 6 1 EmptyHP EmptyHP) (HP 5 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP)
%% S5_8> delHeap $$
%% HP 5 2 (HP 9 1 EmptyHP EmptyHP) (HP 6 1 EmptyHP EmptyHP)
%% S5_8> delHeap $$
%% HP 6 1 (HP 9 1 EmptyHP EmptyHP) EmptyHP
%% S5_8> delHeap $$
%% HP 9 1 EmptyHP EmptyHP
%% S5_8> delHeap $$
%% EmptyHP
%% -}
