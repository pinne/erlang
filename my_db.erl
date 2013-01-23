%%% Exercise 4.1 A Database Server
-module(my_db).
-author('skers@kth.se').

% Server
-export([start/0, stop/0, loop/1]).
% Client, helper functions
-export([write/2, read/1, match/1, delete/1]).

% Spawn server loop and register it as my_db.
start() ->
    case whereis(my_db) of
        undefined -> register(my_db, spawn(?MODULE, loop, [[]]));
        _         -> unregister(my_db),
                     start()
    end,
    ok.

loop(DbRef) ->
    receive
        {write, Key, Val, From} ->
            NewDb = db:write(Key, Val, DbRef),
            From ! {reply, ok},
            loop(NewDb);
        {delete, Key, From} ->
            NewDb = db:delete(Key, DbRef),
            From ! {reply, ok},
            loop(NewDb);
        {read, Key, From} ->
            Result = db:read(Key, DbRef),
            From ! {ok, Result},
            loop(DbRef);
        {match, Val, From} ->
            Result = db:match(Val, DbRef),
            From ! {reply, Result},
            loop(DbRef);
        stop ->
            ok
    end.

%% Client helper functions, send a message to my_db
write(Key, Val) ->
    my_db ! {write, Key, Val, self()},
    receive
        {reply, Reply} -> Reply
    end.

read(Key) ->
    my_db ! {read, Key, self()},
    receive
        {ok, Reply} -> Reply
    end.

match(Val) ->
    my_db ! {match, Val, self()},
    receive
        {reply, Reply} -> Reply
    end.

delete(Key) ->
    my_db ! {delete, Key, self()},
    receive
        {reply, Reply} -> Reply
    end.

stop() ->
    my_db ! stop,
    unregister(my_db),
    ok.

