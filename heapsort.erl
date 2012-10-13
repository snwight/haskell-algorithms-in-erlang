%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module HeapSort where
%% import PQueue
-module(heapsort).
-export([mkPQ/1, hsort/1, ex/0]).

%% mkPQ  :: (Ord a) => [a] -> PQueue a
%% mkPQ xs  = foldr enPQ emptyPQ xs
mkPQ(XS) -> lists:foldr(fun pqueue:enPQ/2, pqueue:emptyPQ(), XS).
 
%% hsort :: (Ord a) => [a] -> [a]
%% hsort xs = hsort' (mkPQ xs)
%%            where hsort' pq 
%%                    | (pqEmpty pq) = []
%%                    | otherwise    = (frontPQ pq):(hsort' (dePQ pq))
hsort1(PQ) ->
    case pqueue:pqEmpty(PQ) of
	true -> [];
	false -> [pqueue:frontPQ(PQ)|hsort1(pqueue:dePQ(PQ))]
    end.
hsort(XS) -> hsort1(mkPQ(XS)).

%% ex = [3,1,4,1,5,9,2,8]
ex() -> [3,1,4,1,5,9,2,8].

%% {-  Examples of evaluations and results 

%% ? hsort(ex()).
%% [1, 1, 2, 3, 4, 5, 8, 9]
%% -}
