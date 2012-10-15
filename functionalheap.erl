%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module Heap(Heap,emptyHeap,heapEmpty,findHeap,insHeap,delHeap)
%     where 
-module(functionalheap).
-export([emptyHeap/0, heapEmpty/1, findHeap/2, insHeap/2, delHeap/2]).
 
% emptyHeap:: (Ord a) => Heap a
% heapEmpty:: (Ord a) => Heap a -> Bool
% findHeap :: (Ord a) => Int -> Heap a -> a
% insHeap  :: (Ord a) => a -> Heap a -> Heap a
% delHeap  :: (Ord a) => Int -> Heap a -> Heap a

% -- IMPLEMENTATION
% data (Ord a) => BinTree a = EmptyBT
%                           | NodeBT a (BinTree a) (BinTree a)
%     deriving Show

% type Heap a = (Int, BinTree a)

% emptyHeap = (0,EmptyBT)
emptyHeap() -> {0, emptyBT}.

% heapEmpty (n,_) = n==0
heapEmpty({0,_}) -> true;
heapEmpty(_) -> false.

% findHeap i (n,t)
%     | i>0 && i<=n = findTree i t
%     | otherwise   = error "findHeap: element not found in Heap"
findHeap(I, {N,T}) when I > 0, I =< N -> findTree(I, T);
findHeap(_, _) -> error("findHeap: element not found in Heap").

% findTree :: (Ord a) => Int -> BinTree a -> a
% findTree i (NodeBT v lf rt)
%     | i==1      = v
%     | even i    = findTree (i `div` 2) lf
%     | otherwise = findTree (i `div` 2) rt
findTree(I, {V, _, _}) when I =:= 1 -> V;
findTree(I, {_, LF, _T}) when I rem 2 =:= 0 -> findTree(I div 2, LF);
findTree(I, {_, _, RT}) -> findTree(I div 2, RT).

% insHeap v (n,t) = (n+1, insTree v (n+1) t)
insHeap(V, {N,T}) -> {N+1, insTree(V, N+1, T)}.

% insTree :: (Ord a) => a -> Int -> BinTree a -> BinTree a
% insTree v' 1 _ = NodeBT v' EmptyBT EmptyBT
% insTree v' i (NodeBT v lf rt) 
%     | even i    = NodeBT small (insTree big (i `div` 2) lf) rt
%     | otherwise = NodeBT small lf (insTree big (i `div` 2) rt)
%     where (small, big) = if (v<=v') then (v,v') else (v',v)
insTree(V1, 1, _) -> {V1, emptyBT, emptyBT};
insTree(V1, I, {V, LF, RT}) when I rem 2 =:= 0 -> 
    {min(V,V1), insTree(max(V,V1), I div 2, LF), RT};
insTree(V1, I, {V, LF, RT}) -> 
    {min(V,V1), LF, insTree(max(V,V1), I div 2, RT)}.

% delHeap i h@(_,EmptyBT) = error ("delHeap: empty heap")
% delHeap i h@(n,t@(NodeBT v lf rt))
%     | i == n     = (n-1,t')
%     | i>0 && i<n = (n-1,t'')
%     | otherwise   = error ("delHeap: element "++ show i
%                            ++ " not found in heap")
%     where
%     (v',t')   = delTreeLast n t -- get and delete last value
%     (v'',t'') = delTree i v' t' -- delete i th value and replace with v'
delHeap(_, {_, emptyBT}) -> error("delHeap: empty heap");
delHeap(I, {N, {V, LF, RT}}) ->
    {V1, T1} = delTreeLast(N, {V, LF, RT}),         % get and delete last value
    {_, T2} = delTree(I, V1, T1),      % delete i th value and replace with v1
    if I =:= n -> {N-1, T1};
       I > 0, I < N -> {N-1, T2};
       true -> error ("delHeap: element not found in heap")
    end.

% delTreeLast :: (Ord a) => Int -> BinTree a -> (a,BinTree a)
% delTreeLast i t@(NodeBT v lf rt)
%     | i == 1    = (v,EmptyBT)
%     | even i    = let (v',lf') = delTreeLast (i `div` 2) lf
%                       in (v',NodeBT v lf' rt)
%     | otherwise = let (v',rt') = delTreeLast (i `div` 2) rt
%                       in (v',NodeBT v lf rt')
delTreeLast(I, {V, _, _}) when I =:= 1 -> {V, emptyBT};
delTreeLast(I, {V, LF, RT}) when I rem 2 =:= 0 ->
    {V1, LF1} = delTreeLast(I div 2, LF), {V1, {V, LF1, RT}};
delTreeLast(I, {V, LF, RT}) ->
    {V1, RT1} = delTreeLast(I div 2, RT), {V1, {V, LF, RT1}}.

% -- removes ith value and pushdown newv in this subtree
% delTree :: (Ord a) => Int -> a -> BinTree a -> (a,BinTree a)
% delTree i v' t@(NodeBT v lf rt) 
%     | i == 1   = (v,pdown v' t)
%     | even i   = let (v'',lf'') = delTree (i `div` 2) big lf 
%                      in (v'',NodeBT small lf'' rt)
%     | otherwise = let (v'',rt'') = delTree (i `div` 2) big rt
%                       in (v'',NodeBT big lf rt'')
%     where (small,big) = if (v<=v') then (v,v') else (v',v)
delTree(I, V1, {V, LF, RT}) when I =:= 1 -> 
    {V, pdown(V1, {V, LF, RT})};
delTree(I, V1, {V, LF, RT}) when I rem 2 =:= 0 ->
    {V2, LF2} = delTree(I div 2, max(V, V1), LF), 
    {V2, {min(V, V1), LF2, RT}};
delTree(I, V1, {V, LF, RT}) ->
    {V2, RT2} = delTree(I div 2, max(V, V1), RT),
    {V2, {max(V, V1), LF, RT2}}.
	    
% pdown :: (Ord a) => a -> BinTree a -> BinTree a 
% pdown v'  EmptyBT   = EmptyBT
% pdown v'  (NodeBT _ EmptyBT EmptyBT) 
%     = (NodeBT v' EmptyBT EmptyBT)
% pdown v'  (NodeBT _ (NodeBT v lf rt) EmptyBT) 
%     | v < v'     = (NodeBT v (NodeBT v' lf rt) EmptyBT)
%     | otherwise = (NodeBT v' (NodeBT v lf rt) EmptyBT)
% pdown v' (NodeBT _ lf@(NodeBT vlf _ _) rt@(NodeBT vrt _ _)) 
%     | vlf<vrt   = if v' < vlf
%                   then (NodeBT v' lf rt)
%                   else (NodeBT vlf (pdown v'  lf) rt)
%     | otherwise = if v' < vrt
%                   then (NodeBT v' lf rt)
%                   else (NodeBT vrt lf (pdown v' rt))
pdown(_, emptyBT) -> emptyBT;
pdown(V1, {_, emptyBT, emptyBT}) -> {V1, emptyBT, emptyBT};
pdown(V1, {_, {V, LF, RT}, emptyBT}) when V < V1 -> {V, {V1, LF, RT}, emptyBT};
pdown(V1, {_, {V, LF, RT}, emptyBT}) -> {V1, {V, LF, RT}, emptyBT};
pdown(V1, {_, {Vlf, _a, _b}, {Vrt, _c, _d}}) when Vlf < Vrt ->
    if V1 < Vlf -> {V1, {Vlf, _a, _b}, {Vrt, _c, _d}};
       V1 >= Vlf -> {V1, pdown(V1, {Vlf, _a, _b}), {Vrt, _c, _d}}
    end;
pdown(V1, {_, {Vlf, _a, _b}, {Vrt, _c, _d}}) when Vlf >= Vrt ->
    if V1 < Vrt -> {V1, {Vlf, _a, _b}, {Vrt, _c, _d}};
       V1 >= Vrt -> {Vrt, {Vlf, _a, _b}, pdown(V1, {Vrt, _c, _d})}
    end.
