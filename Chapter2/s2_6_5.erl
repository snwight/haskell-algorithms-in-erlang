%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S2_6_5 where
-module(s2_6_5).
-export([maximum/1, depthA/1, depthB/1, countEmpty/1, tsumA/1, tsumB/1]).
-export([leaf/1,preorder/1, inorder/1, postorder/1]).

%% nb: ignoring these data types in favor of anonymous tuples - for now
% data Tree a = Node a [Tree a]
%     deriving Show
% data BinTree a  = Empty  | NodeBT a (BinTree a) (BinTree a)
%     deriving Show
% data BinTree' a = Leaf a | NodeBT' a (BinTree' a) (BinTree' a)
%     deriving Show
%% 
%% the closest thing to a data type for this exercise! 
leaf([]) -> empty;
leaf(X) -> {X, empty, empty}.

% maximum [x]= x
% maximum (x:y:ys) | x > y = maximum (x:ys)
%                  | otherwise = maximum (y:ys)
maximum([X]) -> X;
maximum([X|[Y|YS]]) -> 
    if X > Y -> maximum([X|YS]);
       true -> maximum([Y|YS])
    end.

% depth                :: Tree a -> Int
% depth (Node _ [])    = 1
% depth (Node _ succs) = 1 + maximum (map depth succs)
depthA({_, []}) -> 1;
depthA({_, Succs}) -> 1 + maximum(lists:map(fun depthA/1, Succs)).
    
% depth'                  :: BinTree a -> Int
% depth' Empty            = 0
% depth' (NodeBT _ lf rt) = 1 + max (depth' lf) (depth' rt)
depthB(empty) -> 0;
depthB({ _, LF, RT}) -> 1 + max(depthB(LF), depthB(RT)).

% countEmpty                  :: BinTree a -> Int
% countEmpty Empty            = 1
% countEmpty (NodeBT _ lf rt) = countEmpty lf + countEmpty rt
countEmpty(empty) -> 1;
countEmpty({_, LF, RT}) -> countEmpty(LF) + countEmpty(RT).

% tsum                  :: (Num a) => BinTree  a -> a
% tsum Empty            = 0
% tsum (NodeBT a lf rt) = a + (tsum lf) + (tsum rt)
tsumA(empty) -> 0;
tsumA({A, LF, RT}) -> A + tsumA(LF) + tsumA(RT).

% tsum'                   :: (Num a) => BinTree'  a -> a
% tsum' (Leaf v)          = v
% tsum' (NodeBT' v lf rt) = v + (tsum' lf) + (tsum' rt)
tsumB({leaf, V}) -> V;
tsumB({V, LF, RT}) -> V + tsumB(LF) + tsumB(RT).
 
% preorder                  :: BinTree a -> [a]
% preorder Empty            = []
% preorder (NodeBT a lf rt) = [a] ++ preorder lf ++ preorder rt
preorder(empty) -> [];
preorder({A, LF, RT}) -> [A] ++ preorder(LF) ++ preorder(RT).

% inorder                  :: BinTree a -> [a]
% inorder Empty            = []
% inorder (NodeBT a lf rt) = inorder lf ++ [a] ++ inorder rt
inorder(empty) -> [];
inorder({A, LF, RT}) -> inorder(LF) ++ [A] ++ inorder(RT).

% postorder                  :: BinTree a -> [a]
% postorder Empty            = []
% postorder (NodeBT a lf rt) = postorder lf ++ postorder rt ++ [a]
postorder(empty) -> [];
postorder({A, LF, RT}) -> postorder(LF) ++ postorder(RT) ++ [A].

% {----- Examples of evaluations and results
% ? depthB({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, empty, {4, empty, empty}}}).
% 3
% ? countEmpty({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, empty, {4, empty, empty}}}).
% 7
% ? tsumA({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, empty, {4, empty empty}}}).
% 27
% ?  tsumA({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, {2, empty, empty}, {4, empty, empty}}}).
% 29
% ? tsumB({5, {8, {leaf,3}, {leaf,1}}, {6, {leaf,2}, {leaf,4}}}).
% 29
% ? preorder({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, empty, {4, empty, empty}}}).
% [5, 8, 3, 1, 6, 4]
% ? inorder({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, empty, {4, empty, empty}}}).
% [3, 8, 1, 5, 6, 4]
% ? postorder({5, {8, {3, empty, empty}, {1, empty, empty}}, {6, empty, {4, empty, empty}}}).
% [3, 1, 8, 4, 6, 5]
% -----}
