%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original Haskell example code from
% Algorithms: A Functional Programming Approach
% Fethi Rabhi & Guy Lapalme, 
% Addison Wesley, 1999, ISBN 0201-59604-0
%
% Erlang translations by Stephen Wight, northwight@gmail.com
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module Stack(Stack,push,pop,top,emptyStack,stackEmpty) where
-module(stack).
-export([emptyStack/0, stackEmpty/1, push/2, pop/1, top/1]).

% {-  list implementation  
% newtype Stack a = Stk [a]

% instance (Show a) => Show (Stack a) where
%     showsPrec p (Stk [])     str = showChar '-' str
%     showsPrec p (Stk (x:xs)) str
%         = shows x (showChar '|' (shows (Stk xs) str))
%% mmmmmmm very nice

%% emptyStack = Stk []
emptyStack() -> [].

%% stackEmpty (Stk []) = True
%% stackEmpty (Stk _ ) = False
stackEmpty([]) -> true;
stackEmpty(_) -> false.

%% push x (Stk xs) = Stk (x:xs)
push(X, XS) -> [X|XS].

%% pop (Stk [])     = error "pop from an empty stack"
%% pop (Stk (_:xs)) = Stk  xs
pop([]) -> error("pop from an empty stack");
pop([_|XS]) -> XS.

%% top (Stk [])    = error "top from an empty stack"
%% top (Stk (x:_)) = x
top([]) -> error("top from an empty stack");
top([X|_]) -> X.

%%    end of list implementation -}

% emptyStack:: Stack a
% stackEmpty:: Stack a -> Bool
% push :: a-> Stack a -> Stack a
% pop :: Stack a -> Stack a
% top :: Stack a -> a
% {-  implementation with a constructor type -}
% data Stack a = EmptyStk
%              | Stk a (Stack a)
% instance (Show a) => Show (Stack a) where
%     showsPrec p  EmptyStk str = showChar '-' str
%     showsPrec p (Stk x s) str = shows x (showChar '|' (shows s str))
%% nb: mmmm, very nice... not gonna happen

% emptyStack = EmptyStk
%%emptyStack() -> emptyStk.

% stackEmpty EmptyStk = True
% stackEmpty _        = False
%%stackEmpty(emptyStk) -> true;
%%stackEmpty(_) -> false.

% push x s = Stk x s
%%push(X, S) -> {X S}.

% pop EmptyStk  = error "pop from an empty stack"
% pop (Stk _ s) = s
%%pop(emptyStk) -> error("pop from an empty stack");
%%pop(_, S) -> S.

% top EmptyStk  = error "top from an empty stack"
% top (Stk x _) =  x
%%top(emptyStk) -> error "top from an empty stack";
%%top(X, _) -> X.

% {-   end of implementation with a constructor type-}
