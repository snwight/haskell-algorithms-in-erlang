%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module BinTree (BinTree,emptyTree,inTree,addTree,
%                         delTree, buildTree,inorder) where
-module(bintree).
-export([emptyTree/0, inTree/2, addTree/2, delTree/2]).
-export([buildTree/1, buildTree1/1, inorder/1]).

% inTree    :: (Ord a,Show a) => a -> BinTree a -> Bool
% addTree   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
% delTree   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
% buildTree :: (Ord a,Show a) => [a] -> BinTree a
% inorder   :: (Ord a,Show a) => BinTree a -> [a]

% data (Ord a) => BinTree a = EmptyBT
%                           | NodeBT a (BinTree a) (BinTree a)
%     deriving Show

% emptyTree = EmptyBT
emptyTree() -> emptyBT.

% inTree v' EmptyBT                  = False
% inTree v' (NodeBT v lf rt) | v==v' = True  
%                            | v'<v  = inTree v' lf
%                            | v'>v  = inTree v' rt
inTree(_, emptyBT) -> false;
inTree(V1, {V, _, _}) when V =:= V1 -> true;
inTree(V1, {V, LF, _}) when V1 < V -> inTree(V1, LF);
inTree(V1, {V, _, RT}) when  V1 > V -> inTree(V1, RT).

% addTree v' EmptyBT                      = NodeBT v' EmptyBT EmptyBT
% addTree v' (NodeBT v lf rt) | v'==v     = NodeBT v lf rt
%                             | v' < v    = NodeBT v (addTree v' lf) rt
%                             | otherwise = NodeBT v lf (addTree v' rt)
addTree(V1, emptyBT) -> {V1, emptyBT, emptyBT};
addTree(V1, {V, LF, RT}) when V1 =:= V -> {V, LF, RT};
addTree(V1, {V, LF, RT}) when V1 < V -> {V, addTree(V1, LF), RT};
addTree(V1, {V, LF, RT}) -> {V, LF, addTree(V1, RT)}.

% buildTree lf = foldr addTree EmptyBT lf
buildTree(LF) -> lists:foldr(fun addTree/2, emptyBT, LF).

% buildTree' [] = EmptyBT
% buildTree' lf = NodeBT x (buildTree' l1) (buildTree' l2)
%     where l1     = lists:sublist n lf
%           (x:l2) = drop n lf 
%           n = (length lf) `div` 2
buildTree1([]) -> emptyBT;
buildTree1(LF) -> 
    N = length(LF) div 2,
    L1 = lists:sublist(LF, N),
    [X|L2] = lists:nthtail(LF, N),
    {X, buildTree1(L1), buildTree(L2)}.

% -- value not found
% delTree v' EmptyBT                       = EmptyBT 
delTree(v1, emptyBT) -> eEmptyBT;

% -- one descendant
% delTree v' (NodeBT v lf EmptyBT) | v'==v = lf 
% delTree v' (NodeBT v EmptyBT rt) | v'==v = rt
% -- two descendants
% delTree v' (NodeBT v lf rt)
%     | v'<v  = NodeBT v (delTree v' lf) rt 
%     | v'>v  = NodeBT v lf (delTree v' rt)  
%     | v'==v = let k = minTree rt 
%                   in NodeBT k lf (delTree k rt)
delTree(V1, {V, LF, emptyBT}) when V1 =:= V -> LF;
delTree(V1, {V, emptyBT, RT}) when V1 =:= V -> RT;
delTree(V1, {V, LF, RT}) when V1 < V -> {V, delTree(V1, LF), RT}; 
delTree(V1, {V, LF, RT}) when V1 > V -> {V, LF, delTree(V1, RT)};
delTree(V1, {V, LF, RT}) when V1 =:= V ->
    K = minTree(RT), {K, LF, delTree(K, RT)}.

% minTree (NodeBT v EmptyBT _) = v
% minTree (NodeBT _ lf _)      = minTree lf 
% inorder EmptyBT = []
% inorder (NodeBT v lf rt) = inorder lf ++ [v] ++ inorder rt
minTree({V, emptyBT, _}) -> V;
minTree({ _, LF, _}) -> minTree(LF).
inorder(emptyBT) -> [];
inorder({V, LF, RT}) -> inorder(LF) ++ [V] ++ inorder(RT).
