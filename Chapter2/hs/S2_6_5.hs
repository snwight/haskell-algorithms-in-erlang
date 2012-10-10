module S2_6_5 where

data Tree a = Node a [Tree a]
    deriving Show

data BinTree a  = Empty  | NodeBT a (BinTree a) (BinTree a)
    deriving Show

data BinTree' a = Leaf a | NodeBT' a (BinTree' a) (BinTree' a)
    deriving Show

depth                :: Tree a -> Int
depth (Node _ [])    = 1
depth (Node _ succs) = 1 + maximum (map depth succs)

depth'                  :: BinTree a -> Int
depth' Empty            = 0
depth' (NodeBT _ lf rt) = 1 + max (depth' lf) (depth' rt)

countEmpty                  :: BinTree a -> Int
countEmpty Empty            = 1
countEmpty (NodeBT _ lf rt) = countEmpty lf + countEmpty rt

tsum                  :: (Num a) => BinTree  a -> a
tsum Empty            = 0
tsum (NodeBT a lf rt) = a + (tsum lf) + (tsum rt)

tsum'                   :: (Num a) => BinTree'  a -> a
tsum' (Leaf v)          = v
tsum' (NodeBT' v lf rt) = v + (tsum' lf) + (tsum' rt)
 
preorder                  :: BinTree a -> [a]
preorder Empty            = []
preorder (NodeBT a lf rt) = [a] ++ preorder lf ++ preorder rt

inorder                  :: BinTree a -> [a]
inorder Empty            = []
inorder (NodeBT a lf rt) = inorder lf ++ [a] ++ inorder rt

postorder                  :: BinTree a -> [a]
postorder Empty            = []
postorder (NodeBT a lf rt) = postorder lf ++ postorder rt ++ [a]


{----- Examples of evaluations and results

? depth' (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
3
? count (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
7
? tsum (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
27
?  tsum (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 (NodeBT 2 Empty Empty) (NodeBT 4 Empty Empty)))
29
? tsum' (NodeBT' 5 (NodeBT' 8 (Leaf 3) (Leaf 1)) (NodeBT' 6 (Leaf 2) (Leaf 4)))
29
? preorder(NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
[5, 8, 3, 1, 6, 4]
? inorder (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
[3, 8, 1, 5, 6, 4]
? postorder (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
[3, 1, 8, 4, 6, 5]

-----}
