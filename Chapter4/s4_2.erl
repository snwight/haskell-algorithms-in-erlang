%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S4_2 where
-module(s4_2).
-export([tcomp/1, tdouble/1, tsum/1, tcompA/1, depth/1, count/1]).
-export([count_depth/1, count_depthA/1, perc/2, comp/1, compA/2, compB/1]).
-export([inorder/1, inorderA/1, inorderB/2, btA/0, flipT/1, tinsert/2]).
-export([btSz/0, tinsertSz/2, bt/0]).

% data BinTree a = Empty | NodeBT a (BinTree a) (BinTree a)
%     deriving Show

% -- 4.2.2 composing tree operations

% tcomp :: BinTree Int -> Int
% tcomp t = (tsum . tdouble) t
tcomp(T) -> tsum(tdouble(T)).

% tdouble       :: BinTree Int -> BinTree Int
% tdouble Empty = Empty
% tdouble (NodeBT v lf rt) = NodeBT (2*v) (tdouble lf) (tdouble rt)
tdouble(empty) -> empty;
tdouble({V, LF, RT}) -> {2*V,  tdouble(LF), tdouble(RT)}.

% tsum       :: BinTree Int -> Int
% tsum Empty = 0
% tsum (NodeBT v lf rt) = v + (tsum lf) + (tsum rt)
tsum(empty) -> 0;
tsum({V, LF, RT}) -> V + tsum(LF) + tsum(RT).

% tcomp' :: BinTree Int -> Int
% tcomp' Empty = 0
% tcomp' (NodeBT v lf rt) = (2*v)+(tcomp' lf)+(tcomp' rt)
tcompA(empty) -> 0;
tcompA({V, LF, RT}) -> 2*V + tcompA(LF) + tcomp(RT).

% -- 4.2.3 Reducing the number of passes

% -- counting and finding depth
% depth                  :: BinTree a -> Int
% depth Empty            = 0
% depth (NodeBT _ lf rt) = 1 + max (depth lf) (depth rt)
depth(empty) -> 0;
depth({_, LF, RT}) -> 1 + max(depth(LF), depth(RT)).

% count                  :: BinTree a -> Int
% count Empty            = 1
% count (NodeBT _ lf rt) = count lf + count rt
count(empty) -> 1;
count({_, LF, RT}) -> count(LF) + count(RT).

% count_depth t = (count t, depth t)
count_depth(T) -> {count(T), depth(T)}.

% -- counting and finding depth in one pass
% count_depth' Empty = (1,0)
% count_depth' (NodeBT v lf rt) = (c1 + c2, 1 + (max d1 d2))
%     where (c1,d1) = count_depth' lf
%           (c2,d2) = count_depth' rt
count_depthA(empty) -> {1,0};
count_depthA({_, LF, RT}) ->
    {C1,D1} = count_depthA(LF),
    {C2,D2} = count_depthA(RT),
    {C1 + C2, 1 + max(D1,D2)}.

% perc x Empty = Empty
% perc x (NodeBT v lf rt) = NodeBT (fromInt v/ fromInt x)
%                                  (perc x lf)
%                                  (perc x rt)
perc(_, empty) -> empty;
perc(X, {V, LF, RT}) -> {V/X, perc(X, LF), perc(X, RT)}.

% comp t = perc (tsum t) t
comp(T) -> perc(tsum(T), T).

% -- in one pass
% comp'' t = t'
%     where (t', x) = comp' x t
%%
%% nb: leaving this as a warning to those who come after
%%
compB(T) -> {T1, X} = compA(X, T), T1.

% comp' x Empty = (Empty,0)
% comp' x (NodeBT v lf rt) = (NodeBT (fromInt v / fromInt x) p1 p2,
%                             v + s1 + s2)
%     where (p1,s1) = comp' x lf
%           (p2,s2) = comp' x rt
compA(_, empty) -> {empty,0};
compA(X, {V, LF, RT}) -> 
    {P1,S1} = compA(X, LF),
    {P2,S2} = compA(X, RT),
    {{V/X, P1, P2}, {V + S1 + S2}}.

% -- 4.2.4 Removing appends revisited

% inorder Empty = []
% inorder (NodeBT a lf rt) = inorder lf ++ [a] ++ inorder rt 
inorder(empty) -> [];
inorder({A, LF, RT}) -> inorder(LF) ++ [A] ++ inorder(RT). 

% inorder' t = inorder'' t []
%     where inorder'' Empty z = z
%           inorder'' (NodeBT a lf rt) z = inorder'' lf (a:(inorder'' rt z))
inorderA(T) -> inorderB(T, []).
inorderB(empty, Z) -> Z;
inorderB({A, LF, RT}, Z) -> inorderB(LF, [A|inorderB(RT, Z)]).

% -- 4.2.5 Copying in trees
%% or not

% data BinTree'' a = Leaf'' a | Node'' (BinTree'' a) (BinTree'' a)
%     deriving Show

% bt' = Node'' (Node'' (Leaf'' 1) (Leaf'' 2))
%              (Node'' (Leaf'' 3) (Leaf'' 4))
btA() ->{{1, 2}, {3, 4}}.

% flipT             :: BinTree'' a -> BinTree'' a
% flipT (Node'' a b) = Node'' (flipT b) (flipT a)
% --flipT (Leaf'' a)   = Leaf'' a
% flipT x@(Leaf'' a) = x
%% no explicit memoiziation in Erlang so...
flipT({A, B}) -> {flipT(B), flipT(A)};
flipT(A) -> A.

% -- 4.2.6 Storing additional information in the tree

% tinsert v Empty                = NodeBT v Empty Empty
% tinsert v (NodeBT w lf rt)
%     | (count lf) <= (count rt) = NodeBT w (tinsert v lf) rt
%     | otherwise                = NodeBT w lf (tinsert v rt)
tinsert(V, empty) -> {V, empty, empty};
tinsert(V, {W, LF, RT}) ->
    case count(LF) =< count(RT) of
	true -> {W, tinsert(V, LF), RT};
	false -> {W, LF, tinsert(V, RT)}
    end.

% data BinTreeSz a = EmptySz | NodeBTSz (Int,Int) a (BinTreeSz a) (BinTreeSz a)
%     deriving Show

%btSz = (NodeBTSz (3,2) 5 
%	 (NodeBTSz (1,1) 8 (NodeBTSz (0,0) 3 EmptySz EmptySz)
%			   (NodeBTSz (0,0) 1 EmptySz EmptySz))
%	 (NodeBTSz (0,1) 6 EmptySz (NodeBTSz (0,0) 4 EmptySz EmptySz)))
btSz() -> {{3,2}, 5, 
	   {{1,1}, 8, {{0,0}, 3, emptySz, emptySz},
	    {{0,0}, 1, emptySz, emptySz}},
	   {{0,1}, 6, emptySz, {{0,0}, 4, emptySz, emptySz}}}.

% tinsertSz :: a -> BinTreeSz a -> BinTreeSz a
% tinsertSz v EmptySz
%     = NodeBTSz (0,0) v EmptySz EmptySz
% tinsertSz v (NodeBTSz (s1,s2) w lf rt)
%     | s1 <= s2 = NodeBTSz (s1+1, s2) w (tinsertSz v lf) rt
%     | otherwise = NodeBTSz (s1,s2+1) w lf (tinsertSz v rt)
tinsertSz(V, emptySz) -> 
    {{0,0}, V, emptySz, emptySz};
tinsertSz(V, {{S1,S2}, W, LF, RT}) when S1 =< S2 ->
    {{S1+1, S2}, W, LF, tinsertSz(V, RT)};
tinsertSz(V, {{S1,S2}, W, LF, RT}) when S1 > S2 ->
    {{S1,S2+1}, W, LF, tinsertSz(V, RT)}.

%bt = (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty))
%               (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
bt() -> {5, {8, {3, empty, empty}, {1, empty, empty}},
	      {6, empty, {4, empty, empty}}}.

% {- Examples of evaluations and results
% ? flipT(s4_2:btA()).
% Node'' (Node'' (Leaf'' 4) (Leaf'' 3)) (Node'' (Leaf'' 2) (Leaf'' 1))
% ? depth(s4_2:bt()).
% 3
% ? count(s4_2:bt()).
% 7
% ? count_depth(s4_2:bt()).
% (7,3)
% ? count_depth(s4_2:bt()).
% (7,3)
% ? tsum(s4_2:bt).
% 27
% ? perc(27, s4_2:bt()).
% NodeBT 0.185185 (NodeBT 0.296296 (NodeBT 0.111111 Empty Empty) (NodeBT 0.037037 Empty Empty)) (NodeBT 0.222222 Empty (NodeBT 0.148148 Empty Empty))
% ? comp(s4_2:bt()).
% NodeBT 0.185185 (NodeBT 0.296296 (NodeBT 0.111111 Empty Empty) (NodeBT 0.037037 Empty Empty)) (NodeBT 0.222222 Empty (NodeBT 0.148148 Empty Empty))
% ? compB(s4_2:bt()).
% NodeBT 0.185185 (NodeBT 0.296296 (NodeBT 0.111111 Empty Empty) (NodeBT 0.037037 Empty Empty)) (NodeBT 0.222222 Empty (NodeBT 0.148148 Empty Empty))
% ? tdouble(s4_2:bt()).
% NodeBT 10 (NodeBT 16 (NodeBT 6 Empty Empty) (NodeBT 2 Empty Empty)) (NodeBT 12 Empty (NodeBT 8 Empty Empty))
% ? tcomp(s4_2:bt()).
% 54
% ? tcompA(s4_2:bt()).
% 54
% ? inorder(s4_2:bt()).
% [3, 8, 1, 5, 6, 4]
% ? inorderA(s4_2:bt()).
% [3, 8, 1, 5, 6, 4]
% ? s4_2:bt().
% NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty))
% ? tinsert(10, s4_2:bt()).
% NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 (NodeBT 10 Empty Empty) (NodeBT 4 Empty Empty))
% ? tinsertSz(3,s4_2:btSz()).
% NodeBTSz (3,3) 5 (NodeBTSz (1,1) 8 (NodeBTSz (0,0) 3 EmptySz EmptySz) (NodeBTSz (0,0) 1 EmptySz EmptySz)) (NodeBTSz (1,1) 6 (NodeBTSz (0,0) 3 EmptySz EmptySz) (NodeBTSz (0,0) 4 EmptySz EmptySz))
% -}
