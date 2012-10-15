%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -----------------------------------------------------------------------
%% import Backtracking
%% import Array
%% -------------------------------------------------------------------------
%% searchPfs' succ goal x = (search' (enPQ x emptyPQ) 0)
%%  where
%%    search' q  c
%%     | (pqEmpty q)      = []
%%     | goal (frontPQ q) = ((frontPQ q),c+1):(search' (dePQ q)(c+1))
%%     | otherwise        = let x = frontPQ q
%%                          in search' (foldr enPQ (dePQ q) (succ x)) (c+1)

%% -- USING ARRAYS

%% type Position        = (Int,Int)
%% type Board           = Array Int Position
%% data Boards          = BDS [Board] deriving Show

%% g8T :: Board
%% g8T = array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,2)),
%%                    (3,(1,3)),(4,(2,3)),(5,(3,3)),
%%                    (6,(3,2)),(7,(3,1)),(8,(2,1))]

%% s8T :: Board
%% s8T = array (0,8) [(0,(2,2)),(1,(1,2)),(2,(1,1)),
%%                    (3,(3,3)),(4,(2,1)),(5,(3,2)),
%%                    (6,(1,3)),(7,(3,1)),(8,(2,3))]

%% mandist                 :: Position -> Position -> Int
%% mandist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

%% allMoves   :: Board -> [Board]
%% allMoves b = [ b//[(0,b!i),(i,b!0)] 
%%                | i<-[1..8], (mandist (b!0) (b!i) == 1)]

%% succ8Tile     :: Boards -> [Boards]
%% succ8Tile (BDS(n@(b:bs)))  
%%               = filter (notIn bs) [ BDS(b':n) | b' <- allMoves b]
%%   where
%%     notIn bs (BDS(b:_)) 
%%               = not (elem (elems b) (map elems bs))

%% goal8Tile            :: Boards -> Bool
%% goal8Tile (BDS(n:_)) = (elems n == elems g8T)

%% heur1     :: Board  -> Int
%% heur1 l   = sum [ mandist (l!i) (g8T!i) | i<-[0..8]]

%% heur2 :: Board -> Int
%% heur2 l = (heur1 l)  + 3 * (outseq l)

%% outseq   :: Board -> Int
%% outseq b = sum [score (b!i) ((b!(i+1)))|i<-[1..7]]+score (b!8) (b!1)

%% score              :: Position -> Position -> Int
%% score (2,2) _      = 1

%% score (1,3) (2,3)  = 0
%% score (2,3) (3,3)  = 0
%% score (3,3) (3,2)  = 0
%% score (3,2) (3,1)  = 0
%% score (3,1) (2,1)  = 0
%% score (2,1) (1,1)  = 0
%% score (1,1) (1,2)  = 0
%% score (1,2) (1,3)  = 0

%% score _ _          = 2

%% -- Need to define equality of Boards in the priority queue
%% --  before defining order
%% instance Eq Boards
%%   where
%%       ((BDS(l1:_)) == (BDS(l2:_))) = (elems l1 == elems l2)

%% -- 1st heuristic

%% instance Ord Boards
%%   where
%%       ((BDS(l1:_)) < (BDS(l2:_)))  = ((heur1 l1) < (heur1 l2))

%% -- 2nd heuristic
%% -- instance Ord Boards
%% --   where
%% --     ((BDS(l1:_))<(BDS(l2:_))) = (heur2 l1)<(heur2 l2)

%% -------------------------------------------------------------------------------

%% pfs8Tile  :: [[Position]]
%% pfs8Tile  = map elems ls
%%  where
%%    ((BDS ls):_)  
%%           = searchPfs succ8Tile goal8Tile
%%                      (BDS [s8T])

%% stats  = (length ls , c)
%%  where
%%    (((BDS ls),c):_)  
%%           = searchPfs' succ8Tile goal8Tile
%%                      (BDS [s8T])

%% --STATISTICS
%% {-
%% (Heuristic 1)
%% Main> stats
%% (43, 125)

%% (Heuristic 2)
%% Main> stats
%% (19, 26)

%% -}

%% -- 125 nodes are explored with heuristic 1 (length sol=43)
%% -- 26 nodes are explored with heuristic 2 (length sol= 19)
