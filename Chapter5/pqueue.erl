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


%% {-- Heap implementation --}

%% newtype PQueue a = PQ (Heap a)
%%     deriving Show

%% emptyPQ = PQ emptyHeap

%% pqEmpty (PQ h) = heapEmpty h

%% enPQ v (PQ h) = PQ (insHeap v h)

%% frontPQ (PQ h) = findHeap h

%% dePQ (PQ h) = PQ (delHeap h)

%% {-- end of Heap implementation --}
