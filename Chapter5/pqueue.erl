%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module PQueue(PQueue,emptyPQ,pqEmpty,enPQ,dePQ,frontPQ) where
% import Heap
-module(pqueue).
-export([emptyPQ/0, pqEmpty/1, enPQ/2, dePQ/1, frontPQ/1]).
-import(heap, [emptyHeap/0, heapEmpty/1, findHeap/1, insHeap/2, delHeap/1]).

%% emptyPQ :: (Ord a) => PQueue a 
%% pqEmpty :: (Ord a) => PQueue a -> Bool 
%% enPQ    :: (Ord a) => a -> PQueue a -> PQueue a 
%% dePQ    :: (Ord a) => PQueue a -> PQueue a
%% frontPQ :: (Ord a) => PQueue a -> a

%% {-- List implementation --

%% newtype PQueue a      = PQ[a]
%%    deriving Show

%% emptyPQ            =  PQ []

%% pqEmpty (PQ []) = True
%% pqEmpty _       = False

%% enPQ x (PQ q) = PQ (insert x q)
%%     where insert x []                   = [x]
%%           insert x r@(e:r') | x < e     = x:r
%%                             | otherwise = e:insert x r'

%% dePQ (PQ [])     = error "dePQ:empty priority queue"
%% dePQ (PQ (x:xs)) = PQ xs

%% frontPQ (PQ [])    = error "frontPQ:empty priority queue"
%% frontPQ (PQ(x:xs)) = x

%% -- end of List implementation --}


% {-- Heap implementation --}
%% nb: interesting... removal of types makes this a degenerate enterprise...

% newtype PQueue a = PQ (Heap a)
%     deriving Show

% emptyPQ = PQ emptyHeap
emptyPQ() -> emptyHeap().

% pqEmpty (PQ h) = heapEmpty h
pqEmpty(H) -> heapEmpty(H).

% enPQ v (PQ h) = PQ (insHeap v h)
enPQ(V, H) -> insHeap(V, H).

% frontPQ (PQ h) = findHeap h
frontPQ(H) -> findHeap(H).

% dePQ (PQ h) = PQ (delHeap h)
dePQ(H) -> delHeap(H).

% {-- end of Heap implementation --}
