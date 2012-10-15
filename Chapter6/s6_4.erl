%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module S6_4 where
% -- to get access to the imported modules in Hugs do
% -- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts
% import PQueue  
% import BinTree
-module(s6_4).
-export([mkPQ/1, hsort/1, hsort1/1, tsort/1, ex/0]).

% -- Heap Sort

% mkPQ  :: (Ord a) => [a] -> PQueue a
% mkPQ xs  = foldr enPQ emptyPQ xs
mkPQ(XS) -> lists:foldr(fun pqueue:enPQ/2, pqueue:emptyPQ(), XS).
 
% hsort :: (Ord a) => [a] -> [a]
% hsort xs = hsort' (mkPQ xs)
%            where hsort' pq 
%                    | (pqEmpty pq) = []
%                    | otherwise    = (frontPQ pq):(hsort' (dePQ pq))
hsort1(PQ) ->
    case pqueue:pqEmpty(PQ) of
	true -> [];
	false -> [pqueue:frontPQ(PQ)|hsort1(pqueue:dePQ(PQ))]
    end.
hsort(XS) -> hsort1(mkPQ(XS)).

% -- Tree sort

% tsort xs = (inorder . buildTree) xs
tsort(XS) -> bintree:inorder(bintree:buildTree(XS)).

% ex = [3,1,4,1,5,9,2,8]
ex() -> [3,1,4,1,5,9,2,8].

% {-  Examples of evaluations and results 
%     (with number of reductions given by hugs +s)
% ? hsort(ex()).
% [1, 1, 2, 3, 4, 5, 8, 9]
% (437 reductions, 850 cells)
% ? tsort(ex()).
% [1, 2, 3, 4, 5, 8, 9]
% (210 reductions, 432 cells)
% -}
