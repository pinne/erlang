%%% Exercise 4.3 A Database Server with Transactions
-module(my_db_trans).
-author('skers@kth.se').

%% Server
-export([start/0, stop/0, loop/2, loop/3]).
%% Client, helper functions
-export([lock/0, unlock/0, write/2, read/1, match/1, delete/1]).

%% Spawn server loop and register it as my_db.
start() ->
    case whereis(my_db) of
        undefined -> register(my_db, spawn(?MODULE, loop, [free, db:new()]));
        _         -> unregister(my_db), start()
    end,
    ok.

%% Free state
loop(free, DbRef) ->
    receive           
        {From, write, Key, Element} ->
            NewDb = db:write(Key, Element, DbRef),
            From ! {reply, ok},
            loop(free, NewDb);
        {From, delete, Key} ->
            NewDb = db:delete(Key, DbRef),
            From ! {reply, ok},
            loop(free, NewDb);
        {From, read, Key} ->
            Result = db:read(Key, DbRef),
            From ! {reply, Result},
            loop(free, DbRef);
        {From, match, Element} ->
            Result = db:match(Element, DbRef),
            From ! {reply, Result},
            loop(free, DbRef);
        {From, lock} ->
            From ! {reply, ok},
            loop(busy, From, DbRef);
        {From, stop} ->
            From ! {reply, ok};
        {From, Msg} ->
            io:format("~p  ", [Msg]),
            From ! {reply, error},
            loop(free, DbRef)
    end.
%% Busy, receive db commands until unlock.
loop(busy, From, DbRef) ->
    receive
        {From, write, Key, Element} ->
            NewDb = db:write(Key, Element, DbRef),
            From ! {reply, ok},
            loop(busy, From, NewDb);
        {From, delete, Key} ->
            NewDb = db:delete(Key, DbRef),
            From ! {reply, ok},
            loop(busy, From, NewDb);
        {From, read, Key} ->
            Result = db:read(Key, DbRef),
            From ! {reply, Result},
            loop(busy, From, DbRef);
        {From, match, Element} ->
            Result = db:match(Element, DbRef),
            From ! {reply, Result},
            loop(busy, From, DbRef);
        {From, unlock} ->
            From ! {reply, ok},
            loop(free, DbRef);
        {From, stop} ->
            From ! {reply, ok};
        {From, _Arg} ->
            From ! {reply, error_bad_argument},
            loop(busy, From, DbRef)
    end.

%% Client, helper functions
write(Key, Element) ->
    my_db ! {self(), write, Key, Element},
    receive
        {reply, Reply} -> Reply
    end.

read(Key) ->
    my_db ! {self(), read, Key},
    receive
        {reply, Reply} -> Reply
    end.


match(Element) ->
    my_db ! {self(), match, Element},
    receive
        {reply, Reply} -> Reply
    end.

delete(Key) ->
    my_db ! {self(), delete, Key},
    receive
        {reply, Reply} -> Reply
    end.

stop() ->
    my_db ! {self(), stop},
    receive
        {reply, Reply} -> Reply
    end.

%% Start saving db operations
lock() ->
    my_db ! {self(), lock},
    receive
        {reply, Reply} -> Reply
    end.

unlock() ->
    my_db ! {self(), unlock},
    receive
        {reply, Reply} -> Reply
    end.

