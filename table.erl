%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module Table(Table,newTable,findTable,updTable) where
-module(table).
-export([newTable/1, findTable/2, updTable/2]).
-export([newTableL/1, findTableL/2, updTableL/2]).

%% {--- general index ---}
%% newTable    :: (Eq b) => [(b,a)] -> Table a b
%% findTable   :: (Eq b) => Table a b -> b -> a
%% updTable    :: (Eq b) => (b,a) -> Table a b -> Table a b

% {-- Function implementation --

% newtype Table a b   = Tbl (b -> a)

% instance Show (Table a b) where
%     showsPrec _ _ str = showString "<<A Table>>" str

% newTable assocs = foldr updTable 
%                         (Tbl (\_ -> error "item not found in table"))
%                         assocs
newTable(Assocs) ->
    lists:foldr(fun table:updTable/2, 
		fun(_) -> error("item not found in table") end,
		Assocs).

% findTable (Tbl f) i   = f i
findTable(F, I) -> F(I).

% updTable (i,x) (Tbl f) = Tbl g
%     where g j | j==i      = x
%               | otherwise = f j
updTable({I,X}, F) -> 
    G = fun(J) ->
		case J =:= I of 
		    true -> X;
		    false -> F(J)
		end
	end,
    G.

% -- end of Function implementation --}

% {-- List implementation --}

% newtype Table a b        = Tbl [(b,a)]
%     deriving Show

% newTable   t          = Tbl t
newTableL(T) -> T.

% findTable (Tbl []) i = error "item not found in table"
% findTable (Tbl ((j,v):r)) i
%      | (i==j)        = v
%      | otherwise     = findTable (Tbl r) i 
findTableL([], _) -> error("item not found in table");
findTableL([{J,V}|_], I) when I =:= J -> V;
findTableL([_|R], I) -> findTableL(R, I).

% updTable e (Tbl [])         = (Tbl [e])
% updTable e'@(i,_) (Tbl (e@(j,_):r))
%      | (i==j)         = Tbl (e':r)
%      | otherwise      = Tbl (e:r')
%      where Tbl r' = updTable e' (Tbl r)
updTableL(E, []) -> [E];
updTableL({I,_a}, [{J,_}|R]) when I =:= J -> [{I,_a}|R];
updTableL({I,_a}, [{J,_b}|R]) -> [{J,_a}|updTableL({I,_b}, R)].

% {-- end of List implementation --}

% {--- end of general index ---}

%% {--- Array implementation ---

%% import Array

%% newTable    :: (Ix b) => [(b,a)] -> Table a b
%% findTable   :: (Ix b) => Table a b -> b -> a
%% updTable    :: (Ix b) => (b,a) -> Table a b -> Table a b

%% newtype Table a b     = Tbl (Array b a)
%%     deriving Show

%% newTable l = Tbl (array (lo,hi) l)
%%     where
%%            indices = map fst l
%%            lo      = minimum indices
%%            hi      = maximum indices

%% findTable (Tbl a) i      = a ! i

%% updTable p@(i,x) (Tbl a) = Tbl (a // [p])

%% --- end of Array implementation ---}
