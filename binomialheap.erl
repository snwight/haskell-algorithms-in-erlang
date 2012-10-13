%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module Heap(Heap,emptyHeap,heapEmpty,findHeap,insHeap,delHeap)
%%     where 

%% emptyHeap:: (Ord a) => Heap a
%% heapEmpty:: (Ord a) => Heap a -> Bool
%% findHeap :: (Ord a) => Int -> Heap a -> a
%% insHeap  :: (Ord a) => a -> Heap a -> Heap a
%% delHeap  :: (Ord a) => Int -> Heap a -> Heap a

%% -- IMPLEMENTATION with Binomial Heap
%% -- adapted from C. Okasaki Purely Functional Data Structures p 198

%% data Tree a = Node Int a [Tree a] deriving Show
%% data (Ord a) => Heap a = BH [Tree a] deriving Show

%% rank (Node r _ _) = r
%% root (Node _ x _) = x

%% link t1@(Node r x1 c1) t2@(Node _ x2 c2)
%%      | x1 <= x2  = Node (r+1) x1 (t2:c1)
%%      | otherwise = Node (r+1) x2 (t1:c2)

%% insTree t []               = [t]
%% insTree t ts@(t':ts')
%%         | rank t < rank t' = t:ts
%%         | otherwise        = insTree (link t t') ts'

%% mrg ts1 []              = ts1
%% mrg [] ts2              = ts2
%% mrg ts1@(t1:ts1') ts2@(t2:ts2')
%%     | rank t1 < rank t2 = t1:mrg ts1' ts2
%%     | rank t2 < rank t1 = t1:mrg ts1  ts2'
%%     | otherwise         = insTree (link t1 t2) (mrg ts1' ts2')

%% removeMinTree []                 = error "empty Heap"
%% removeMinTree [t]                = (t,[])
%% removeMinTree (t:ts)
%%               | root t < root t' = (t,ts)
%%               | otherwise        = (t',t:ts')
%%               where (t',ts') = removeMinTree ts

%% emptyHeap = BH []

%% heapEmpty (BH ts) = null ts
                    
%% findHeap 1 (BH ts) = root (fst (removeMinTree ts))
%% findHeap _ _       = error "findHeap: not looking for first"

%% insHeap x (BH ts) = BH (insTree (Node 0 x []) ts)

%% delHeap 1 (BH ts) = BH (mrg (reverse ts1) ts2)
%%     where (Node _ x ts1, ts2) = removeMinTree ts
%% delHeap _ _            = error "delHeap: not looking for first"


%% {- examples of calls and results

%% Heap> insHeap 3 emptyHeap
%% BH [Node 0 3 []]
%% Heap> insHeap 1 $$
%% BH [Node 1 1 [Node 0 3 []]]
%% Heap> insHeap 4 $$
%% BH [Node 0 4 [], Node 1 1 [Node 0 3 []]]
%% Heap> insHeap 1 $$
%% BH [Node 2 1 [Node 1 1 [Node 0 3 []], Node 0 4 []]]
%% Heap> insHeap 5 $$
%% BH [Node 0 5 [], Node 2 1 [Node 1 1 [Node 0 3 []], Node 0 4 []]]
%% Heap> insHeap 6 $$
%% BH [Node 1 5 [Node 0 6 []], Node 2 1 [Node 1 1 [Node 0 3 []], Node 0 4 []]]
%% Heap> insHeap 9 $$
%% BH [Node 0 9 [], Node 1 5 [Node 0 6 []], Node 2 1 [Node 1 1 [Node 0 3 []], Node 0 4 []]]
%% Heap> insHeap 4 $$
%% BH [Node 3 1 [Node 2 4 [Node 1 5 [Node 0 6 []], Node 0 9 []], Node 1 1 [Node 0 3 []], Node 0 4 []]]
%% Heap> delHeap 1 $$
%% BH [Node 0 4 [], Node 1 1 [Node 0 3 []], Node 2 4 [Node 1 5 [Node 0 6 []], Node 0 9 []]]
%% Heap> delHeap 1 $$
%% BH [Node 1 3 [Node 0 4 []], Node 2 4 [Node 1 5 [Node 0 6 []], Node 0 9 []]]
%% Heap> delHeap 1 $$
%% BH [Node 0 4 [], Node 2 4 [Node 1 5 [Node 0 6 []], Node 0 9 []]]
%% Heap> delHeap 1 $$
%% BH [Node 2 4 [Node 1 5 [Node 0 6 []], Node 0 9 []]]
%% Heap> delHeap 1 $$
%% BH [Node 0 9 [], Node 1 5 [Node 0 6 []]]
%% Heap> delHeap 1 $$
%% BH [Node 1 6 [Node 0 9 []]]
%% Heap> delHeap 1 $$
%% BH [Node 0 9 []]
%% Heap> delHeap 1 $$
%% BH []
%% Heap> foldr insHeap emptyHeap (reverse [1,10,5,15,6])
%% BH [Node 0 6 [], Node 2 1 [Node 1 5 [Node 0 15 []], Node 0 10 []]]
%% Heap> delHeap 1 $$
%% BH [Node 2 5 [Node 1 6 [Node 0 10 []], Node 0 15 []]]
%% -}
