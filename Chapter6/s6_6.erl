%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S6_6 where
% import Array

% -- Radix Sort
% type Key val     = [val]
% type Bucket  val = [Key val]
% type Buckets val = Array val (Bucket val)

% -- First version

% split :: (Ix a) => Int -> (a,a) -> [Key a] -> Buckets a
% split k bnds l  = accumArray f [] bnds [(x!!k , x) | x <- l]
%     where f l key = l ++ [key]

% concatA:: (Ix a)=> Buckets a -> [Key a]
% concatA bckts = concat (elems bckts)

% rsort :: Ix a => Int -> (a,a) -> [Key a] -> [Key a]
% rsort 0     bnds l = l
% rsort (p+1) bnds l = rsort p bnds (concatA (split p bnds l))

% -- Improved version

% split' :: (Ix a) => Int -> (a,a) -> [Key a] -> Buckets a
% split' k bnds l = accumArray f [] bnds [(x!!k,x) | x <- l]
%                    where f l key = key : l

% concatA':: Ix a => Buckets a -> [Key a]
% concatA' bckts = concat (map reverse (elems bckts))

% -- more efficient but more obscure version of concatA' that
% -- combines the effect of concat and (map reverse)
% concatA'' :: Ix a => Buckets a -> [Key a]
% concatA'' = (foldr rev []) . elems
%     where
%     rev []     res = res
%     rev (x:xs) res = rev xs (x:res)

% rsort' 0     bnds l = l
% rsort' (p+1) bnds l = rsort' p bnds (concatA'' (split' p bnds l))


% v = rsort 3 (0,9) [[2,3,2],[2,3,1],[4,2,8],[1,1,1],[2,1,3],
%                    [8,2,1],[7,9,7],[3,9,8],[5,2,1]]

% w = rsort 4 (' ','z') ["fred","sami","paul","joe "]

% v' = rsort' 3 (0,9) [[2,3,2],[2,3,1],[4,2,8],[1,1,1],[2,1,3],
%                    [8,2,1],[7,9,7],[3,9,8],[5,2,1]]

% w' = rsort' 4 (' ','z') ["fred","sami","paul","joe "]


% {-  Examples of evaluations and results 
%     (with number of reductions given by hugs +s)
% ? v
% [[1, 1, 1], [2, 1, 3], [2, 3, 1], [2, 3, 2], [3, 9, 8], [4, 2, 8], [5, 2, 1], [7, 9, 7], [8, 2, 1]]
% (1505 reductions, 2726 cells)
% ? w
% ["fred", "joe ", "paul", "sami"]
% (1916 reductions, 3682 cells)
% ? v'
% [[1, 1, 1], [2, 1, 3], [2, 3, 1], [2, 3, 2], [3, 9, 8], [4, 2, 8], [5, 2, 1], [7, 9, 7], [8, 2, 1]]
% (1459 reductions, 2621 cells)
% ? w'
% ["fred", "joe ", "paul", "sami"]
% (228 reductions, 348 cells)
% -}
