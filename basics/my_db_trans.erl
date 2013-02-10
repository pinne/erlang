%% Exercise 4.3 A Database Server with Transactions
-module(my_db_trans).
-author('skers@kth.se').

%% Server
-export([start/0, stop/0, loop/1, loop/2]).
%% Client, helper functions
-export([lock/0, unlock/0, write/2, read/1, match/1, delete/1]).

%% Spawn server loop and register it as my_db.
start() ->
    case whereis(my_db) of
        undefined -> register(my_db, spawn(?MODULE, loop, [db:new()]));
        _Defined  -> unregister(my_db), start()
    end,
    ok.

%% Free state
loop(DbRef) ->
    receive           
        {From, write, Key, Element} ->
            NewDb = db:write(Key, Element, DbRef),
            From ! {reply, ok},
            loop(NewDb);
        {From, delete, Key} ->
            NewDb = db:delete(Key, DbRef),
            From ! {reply, ok},
            loop(NewDb);
        {From, read, Key} ->
            Result = db:read(Key, DbRef),
            From ! {reply, Result},
            loop(DbRef);
        {From, match, Element} ->
            Result = db:match(Element, DbRef),
            From ! {reply, Result},
            loop(DbRef);
        {From, lock} ->
            From ! {reply, ok},
            loop(From, DbRef);
        {From, stop} ->
            From ! {reply, ok};
        {From, Msg} ->
            io:format("~p  ", [Msg]),
            From ! {reply, error},
            loop(DbRef)
    end.
%% Busy, receive db commands until unlock.
loop(From, DbRef) ->
    receive
        {From, write, Key, Element} ->
            NewDb = db:write(Key, Element, DbRef),
            From ! {reply, ok},
            loop(From, NewDb);
        {From, delete, Key} ->
            NewDb = db:delete(Key, DbRef),
            From ! {reply, ok},
            loop(From, NewDb);
        {From, read, Key} ->
            Result = db:read(Key, DbRef),
            From ! {reply, Result},
            loop(From, DbRef);
        {From, match, Element} ->
            Result = db:match(Element, DbRef),
            From ! {reply, Result},
            loop(From, DbRef);
        {From, unlock} ->
            From ! {reply, ok},
            loop(DbRef);
        {From, stop} ->
            From ! {reply, ok};
        {From, _Arg} ->
            From ! {reply, error_bad_argument},
            loop(From, DbRef)
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

