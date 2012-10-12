%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module AVLTree(AVLTree,emptyAVL,addAVL) where
-module(avltree).
-export([emptyAVL/0, addAVL/2]).

%% data (Ord a,Show a) => AVLTree a = EmptyAVL
%%                                  | NodeAVL a (AVLTree a) (AVLTree a)
%%     deriving Show

%% emptyAVL = EmptyAVL
emptyAVL() -> emptyAVL.

%% rotateLeft,rotateRight :: (Ord a,Show a) => AVLTree a -> AVLTree a
%% rotateLeft EmptyAVL   = EmptyAVL
%% rotateLeft (NodeAVL v (NodeAVL lv lflf lfrt)
%%                        rt)  
%%     = NodeAVL lv lflf
%%                  (NodeAVL v lfrt rt)
rotateLeft(emptyAVL) -> emptyAVL;
rotateLeft ({V, {LV, LFLF, LFRT}, RT}) -> {LV, LFLF, {V, LFRT, RT}}.

%% rotateRight EmptyAVL  = EmptyAVL
%% rotateRight (NodeAVL v lf
%%                        (NodeAVL rv rtlf rtrt)) 
%%     = NodeAVL rv (NodeAVL v lf rtlf)
%%               rtrt
rotateRight(emptyAVL) -> emptyAVL;
rotateRight({V, LF, {RV, RTLF, RTRT}}) -> {RV, {V, LF, RTLF}, RTRT}.

%% dRotateLeftRight , dRotateRightLeft 
%%     :: (Ord a,Show a) => AVLTree a -> AVLTree a
%% dRotateRightLeft (NodeAVL v lf
%%                             (NodeAVL rv (NodeAVL rtlv rtlflf rtlfrt)
%%                                          rtrt))
%%     = NodeAVL rtlv (NodeAVL v lf
%%                               rtlflf)
%%                    (NodeAVL rv rtlfrt rtrt)
%% dRotateLeftRight (NodeAVL v (NodeAVL lv lflf
%%                                         (NodeAVL lfrv lfrtlf
%%                                                       lfrtrt))
%%                              rt)
%%     = NodeAVL lfrv (NodeAVL lv lflf lfrtlf)
%%                    (NodeAVL v lfrtrt rt)
dRotateRightLeft({V, LF, {RV, {RTLV, RTLFLF, RTLFRT}, RTRT}}) ->
    {RTLV, {V, LF, RTLFLF}, {RV, RTLFRT, RTRT}}.
dRotateLeftRight({V, {LV, LFLF, {LFRV, LFRTLF, LFRTRT}}, RT}) ->
    {LFRV, {LV, LFLF, LFRTLF}, {V, LFRTRT, RT}}.

%% height EmptyAVL  = 0
%% height (NodeAVL _ lf rt) = 1 + max (height lf) (height rt)
height(emptyAVL) -> 0;
height({ _, LF, RT}) -> 1 + max(height(LF), height(RT)).

%% addAVL i EmptyAVL= NodeAVL i EmptyAVL EmptyAVL
%% addAVL i (NodeAVL v lf rt) 
%%     | i < v     = let
%%                     newlf@(NodeAVL newlfv _ _)  = addAVL i lf 
%%                   in
%%                    if ((height newlf - height rt) == 2)
%%                    then if i < newlfv
%%                         then rotateLeft (NodeAVL v newlf rt)
%%                         else dRotateLeftRight (NodeAVL v newlf rt)
%%                    else (NodeAVL v newlf rt)
%%     | otherwise = let
%%                     newrt@(NodeAVL newrtv _ _)  = addAVL i rt
%%                   in
%%                    if ((height newrt - height lf) == 2)
%%                    then if i > newrtv
%%                         then rotateRight (NodeAVL v lf newrt)
%%                         else dRotateRightLeft (NodeAVL v lf newrt)
%%                    else (NodeAVL v lf newrt)
addAVL(I, emptyAVL) -> {I, emptyAVL, emptyAVL};
addAVL(I, {V, LF, RT}) when I < V ->
    Newlf = {Newlfv, _a, _b} = addAVL(I, LF), 
    case height(Newlf) - height(RT) =:= 2 of
	true -> 
	    if I < Newlfv ->
		    rotateLeft({V, Newlf, RT});
	       I >= Newlfv -> 
		    dRotateLeftRight({V, Newlf, RT})
	    end;
	false -> {V, Newlf, RT}
    end;
addAVL(I, {V, LF, RT}) when I >= V ->
    Newrt = {Newrtv, _a, _b} = addAVL(I, RT),
    case height(Newrt) - height(LF) =:= 2 of
	true -> 
	    if I > Newrtv ->
		    rotateRight({V, LF, Newrt});
	       I >= Newrtv -> 
		    dRotateRightLeft({V, LF, Newrt})
	    end;
	false -> {V, LF, Newrt}
    end.
