%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module Queue(Queue,emptyQueue,queueEmpty,enqueue,dequeue,front) where
-module(queue).
-export([emptyQueue/0, queueEmpty/1, enqueue/2, dequeue/1, front/1, showQ/1]).

% emptyQueue :: Queue a
% queueEmpty :: Queue a -> Bool
% enqueue    :: a -> Queue a -> Queue a
% dequeue    :: Queue a -> Queue a
% front      :: Queue a -> a

%% % {-  Implementation with a list  -

%% % newtype Queue a   = Q [a]
%% %    deriving Show

%% % emptyQueue     = Q []
%% emptyQueueA() -> [].

%% % queueEmpty (Q [])  = True
%% % queueEmpty (Q _ )  = False
%% queueEmptyA([]) -> true;
%% queueEmptyA(_) -> false.

%% % enqueue x (Q q)    = Q (q ++ [x])
%% enqueueA(X, Q) -> Q ++ [X].

%% % dequeue (Q (_:xs)) = Q xs
%% % dequeue (Q [])     = error "dequeue: empty queue"
%% dequeueA([_|XS]) -> XS;
%% dequeueA([]) -> error("dequeue: empty queue").

%% % front (Q (x:_)) = x
%% % front (Q [])    = error "front: empty queue"
%% frontA([X|_]) -> X;
%% frontA([]) -> error("front: empty queue").

% -  end of list implementation -}

% {- implementation with a tuple of lists -}

% newtype Queue a      = Q ([a],[a])
% instance (Show a) => Show (Queue a) where
%     showsPrec p (Q (front, rear)) str
%         = showString "Q " (showList (front ++ reverse rear) str)
showQ({F,R}) -> F++lists:reverse(R).

% queueEmpty (Q ([],[])) = True
% queueEmpty _           = False
queueEmpty({[],[]}) -> true;
queueEmpty(_) -> false.

% emptyQueue        = Q ([],[])
emptyQueue() -> {[],[]}.

% enqueue x (Q ([],[])) = Q ([x],[])  
% enqueue y (Q (xs,ys)) = Q (xs,y:ys)
enqueue(X, {[],[]}) -> {[X],[]};
enqueue(Y, {XS,YS}) -> {XS,[Y|YS]}.

% dequeue (Q ([],[]))   = error "dequeue:empty queue"
% dequeue (Q ([],ys))   = Q (tail(reverse ys), [])
% dequeue (Q (x:xs,ys)) = Q (xs,ys)
dequeue({[],[]}) -> error("dequeue:empty queue");
dequeue({[],YS}) -> {lists:nthtail(1, lists:reverse(YS)), []};
dequeue({[_|XS],YS}) -> {XS,YS}.

% front (Q ([],[]))   = error "front:empty queue"
% front (Q ([],ys))   = last ys
% front (Q (x:xs,ys)) = x
front({[],[]}) -> error("front:empty queue");
front({[],YS}) -> lists:last(YS);
front({[X|_],_}) -> X.

% {-  end of tuple of lists implementation -}

