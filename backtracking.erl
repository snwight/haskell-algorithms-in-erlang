%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BACKTRACKING FRAMEWORK
% module Backtracking where
-module(backtracking).
-export([del/2, slashslash/2]).
-export([flatten/1, emptyStack/0, stackEmpty/1, push/2, pop/1, top/1]).
-export([emptyHeap/0, heapEmpty/1, findHeap/2, insHeap/2, delHeap/2, pdown/1]).
-export([emptyPQ/0, pqEmpty/1, enPQ/2, frontPQ/1, dePQ/1]).
-export([searchDfs/3, searchA/3, searchPfs/3, searchB/3, searchPfs2/3, searchC/4]).

% --import "../haskell/ListOps.hs"
%% (\\) xs1 xs2 = foldl del xs1 xs2
%%        where []     `del`  _ = []
%%              (x:xs) `del`  y
%%                  | x == y    = xs
%%                  | otherwise = x : (xs `del` y)
%% nb: interesting... but i'll keep this infix for now
del([], _) -> [];
del([X|XS], Y) when X =:= Y -> XS;
del([X|XS], Y) when X =/= Y -> [X | del(XS, Y)].
slashslash(XS1, XS2) -> lists:foldl(fun del/2, XS1, XS2).

%% flatten = foldr (++) []
flatten(L) -> lists:foldr(fun(Elem, Acc) -> Elem ++ Acc end, [], L).

% ---------------------------------------------------
% --import "../adt/Stack.hs"

% emptyStack :: Stack a
% stackEmpty :: Stack a -> Bool
% push       :: a -> Stack a -> Stack a
% pop        :: Stack a -> Stack a
% top        :: Stack a -> a

% type Stack a  = [a]

% emptyStack    = []
% stackEmpty [] = True
% stackEmpty _  = False
% push x xs     = x:xs
% pop (_:xs)    = xs
% top (x:_)     = x
emptyStack() -> [].
stackEmpty([]) -> true;
stackEmpty(_) ->  false.
push(x, xs) -> [x|xs].
pop([_|xs]) -> xs.
top([x|_]) -> x.

% ----------------------------------------------------

% --import Heap
% emptyHeap:: (Heap a)
% heapEmpty:: (Heap a) -> Bool
% findHeap :: (Ord a) => Int -> (Heap a) -> a
% insHeap  :: (Ord a) => (Int,a) -> (Heap a) -> (Heap a)
% delHeap  :: (Ord a) => Int -> (Heap a) -> (a,(Heap a))
% pdown    :: (Ord a) => (a , (Heap a)) -> (Heap a)

% -- IMPLEMENTATION

% data (Ord a) => Heap a = Node a (Heap a) (Heap a) | Empty
%     deriving Show
% emptyHeap       = Empty
emptyHeap() -> empty.

% heapEmpty Empty = True
% heapEmpty _     = False
heapEmpty(empty) -> true;
heapEmpty(_) -> false.

% findHeap n (Node v lf rt) 
%      | (n==1)             = v
%      | ((n `mod` 2) == 0) = findHeap (n `div` 2) lf
%      | otherwise          = findHeap (n `div` 2) rt
findHeap(N, {V, _, _}) when N =:= 1 -> V;
findHeap(N, {_, LF, _}) when N rem 2 =:= 0 -> findHeap(N div 2, LF);
findHeap(N, {_, _, RT}) -> findHeap(N div 2, RT).

% insHeap (n,k) Empty 
%                 = (Node  k Empty Empty)
insHeap({_, K}, empty) -> {K, empty, empty};

% insHeap (n,k) (Node v lf rt) 
%      | v < k    = if ((n `mod` 2) == 0)
%                   then Node v (insHeap ((n `div` 2),k) lf) rt 
%                   else Node v lf (insHeap ((n `div` 2),k) rt)   
%      | otherwise= if ((n `mod` 2) == 0)
%                   then Node k (insHeap ((n `div` 2),v) lf) rt 
%                   else Node k lf (insHeap ((n `div` 2),v) rt) 
insHeap({N, K}, {V, LF, RT}) when V < K ->
    if N rem 2 =:= 0 -> 
	    {V, {insHeap(N div 2, K), LF}, RT};
       true -> 
	    {V, LF, {insHeap(N div 2, K), RT}}
    end;
insHeap({N, K}, {V, LF, RT}) ->
    if N rem 2 =:= 0 -> 
	    {K, {insHeap(N div 2, V), LF}, RT};
       true -> 
	    {K, LF, {insHeap(N div 2, V), RT}}
    end.

% delHeap 1  (Node v Empty Empty) 
%                           = (v,Empty)
% delHeap k  (Node v lf rt) 
%        | (k `rem` 2 == 0) = let (v',rest) = (delHeap (k `div` 2) lf)
%                             in
%                               (v', (Node v rest  rt))
%        | otherwise        = let (v',rest) = (delHeap (k `div` 2) rt)
%                             in 
%                               (v', (Node v lf rest))
delHeap(1, {V, empty, empty}) -> {V, empty};
delHeap(K, {V, LF, RT}) when K rem 2 =:= 0 ->
    {V1, Rest} = delHeap(K div 2, LF), {V1, {V, Rest, RT}};
delHeap(K, {V, LF, RT}) ->
    {V1, Rest} = delHeap(K div 2, RT), {V1, {V, LF, Rest}}.

% pdown (v , Empty)     = Empty
% pdown (v , (Node _ Empty Empty)) 
%                       = (Node v Empty Empty)
% pdown (v , (Node _ (Node a lf rt) Empty)) 
%           | a < v     = (Node a (Node v lf rt) Empty)
%           | otherwise = (Node v (Node a lf rt) Empty)
% pdown (v , (Node _ n1@(Node a _ _) n2@(Node b _ _))) 
%           | a<b       = if v < a
%                         then (Node v n1 n2)
%                         else (Node a (pdown (v , n1)) n2)
%           | otherwise = if v < b
%                         then (Node v n1 n2)
%                         else (Node b n1 (pdown (v , n2) ))
pdown({_, empty}) -> empty;
pdown({V, {_, empty, empty}}) -> {V, empty, empty};
pdown({V, {_, {A, LF, RT}, empty}}) ->
    if A < V -> {A, {V, LF, RT}, empty};
       A >= V -> {V, {A, LF, RT}, empty}
    end;
pdown({V, {_, {A, _a, _b}, {B, _c, _d}}}) when A < B ->
    if V < A -> {V, {A, _a, _b}, {B, _c, _d}};
       V >= A -> {A, pdown({V, {A, _a, _b}}), {B, _c, _d}}
    end;
pdown({V, {_, {A, _a, _b}, {B, _c, _d}}}) when A >= B ->
    if V < B -> {V, {A, _a, _b}, {B, _c, _d}};
       V >= B -> {B, {A, _a, _b}, pdown({V, {B, _c, _d}})}
    end.

% ------------------------------------------------------------------------
% --import "../adt/Priqueue.hs"
% -- INTERFACE

% emptyPQ :: PQueue a 
% pqEmpty :: PQueue a -> Bool 
% enPQ    :: (Ord a) => a -> PQueue a -> PQueue a 
% dePQ    :: (Ord a) => PQueue a -> PQueue a
% frontPQ :: (Ord a) => PQueue a -> a

% -- HEAP IMPLEMENTATION
% -- include Heap.hs when loading

% type PQueue a     = (Int,Heap a)

% emptyPQ           = (0,emptyHeap)
emptyPQ() -> {0, emptyHeap()}.

% pqEmpty (_,t) 
%     | heapEmpty t = True
%     | otherwise   = False
pqEmpty({_, T}) -> heap:heapEmpty(T).

% enPQ k (s,t)      = (s+1,insHeap((s+1),k) t)
enPQ(K, {S, T}) -> {S + 1, insHeap({S + 1}, K), T}.

% frontPQ (_,t)     = findHeap 1 t
frontPQ({_, T}) -> findHeap(1, T).

% dePQ (s,t)        = (s-1,pdown (k,t'))
%                     where 
%                       (k,t') = delHeap s t  
dePQ({S, T}) -> 
    {K, T1} = delHeap(S, T),
    {S - 1, pdown({K, T1})}.

% -- BACKTRACKING

% -- DIFFS : 1) Use implicit graph (fct SUCC INSTEAD OF G)
% --         2) add goal function
% --         3) no path accumulation
% --         4) Assumes acyclic graph

% searchDfs             :: (Eq node) => (node -> [node]) -> (node -> Bool) 
%                           -> node -> [node]
% searchDfs succ goal x = (search' (push x emptyStack) )
%  where
%    search' s  
%     | (stackEmpty s)   = [] 
%     | goal (top s)     = (top s):(search' (pop s))
%     | otherwise        = let x = top s
%                        in search' (foldr push (pop s) (succ x))
searchDfs(Succ, Goal, X) -> searchA(Succ, Goal, push(X, emptyStack())).
searchA(Succ, Goal, S) -> 
    case stackEmpty(S) of 
	true -> [];
	false -> 
	    case Goal(top(S)) of 
		true -> [top(S)|searchA(Succ, Goal, pop(S))];
		false -> searchA(Succ, Goal, lists:foldr(fun heap:push/2, 
							 pop(S), 
							 Succ(top(S))))
	    end
    end.

% ----------------------------------------------------------------------
% ---------------------------------------------------------------------
% ------------------------------------------------------------------
% -- PRIORITY-FIRST FRAMEWORK

% searchPfs             :: (Ord node) => (node -> [node]) -> (node -> Bool) 
%                           -> node -> [node]
% searchPfs succ goal x = (search' (enPQ x emptyPQ) )
%  where
%    search' q  
%     | (pqEmpty q)      = [] 
%     | goal (frontPQ q) = (frontPQ q):(search' (dePQ q))
%     | otherwise        = let x = frontPQ q
%                          in search' (foldr enPQ (dePQ q) (succ x))
searchPfs(Succ, Goal, X) -> searchB(Succ, Goal, enPQ(X, emptyPQ())).
searchB(Succ, Goal, Q) ->
    case pqEmpty(Q) of
	true -> [];
	false -> 
	    case Goal(frontPQ(Q)) of
		true -> [frontPQ(Q)|searchB(Succ, Goal, dePQ(Q))];
		false -> searchB(Succ, Goal, lists:foldr(fun enPQ/2, 
							 dePQ(Q), 
							 Succ(frontPQ(Q))))
	    end
    end.

% --------------------------------------------------------------------
% --Also counts how many nodes examined

% -- searchPfs'             :: (Ord node) => (node -> [node]) -> (node -> Bool) 
% --                           -> node -> [(node,Int)]
% -- searchPfs' succ goal x = (search' (enPQ x emptyPQ) 0)
% --  where
% --    search' q  c
% --     | (pqEmpty q)      = []
% --     | goal (frontPQ q) = ((frontPQ q),c+1):(search' (dePQ q)(c+1))
% --     | otherwise        = let x = frontPQ q
% --                          in search' (foldr enPQ (dePQ q) (succ x)) (c+1)
searchPfs2(Succ, Goal, X) ->  searchC(Succ, Goal, enPQ(X, emptyPQ()), 0).
searchC(Succ, Goal, Q, C) ->
    case pqEmpty(Q) of
	true -> [];
	false -> 
	    case Goal(frontPQ(Q)) of
		true -> [{frontPQ(Q), C+1}|searchC(Succ, Goal, dePQ(Q), C+1)];
		false -> searchC(Succ, Goal, 
				 lists:foldr(fun enPQ/2, dePQ(Q), Succ(frontPQ(Q))),
				 C+1)
	    end
    end.

