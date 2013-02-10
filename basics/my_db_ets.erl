%%% Exercise 4.1 A Database Server
-module(my_db_ets).
-author('skers@kth.se').

% Server
-export([start/0, stop/0, loop/1]).
% Client, helper functions
-export([write/2, read/1, match/1, delete/1]).

% Spawn server loop and register it as my_db.
start() ->
    case whereis(my_db) of
        undefined -> register(my_db, spawn(?MODULE, loop, [db_ets:new()]));
        _         -> unregister(my_db), start()
    end.

loop(DbRef) ->
    receive
        {write, Key, Val, From} ->
            db_ets:write(Key, Val, null),
            From ! {reply, ok},
            loop(DbRef);
        {delete, Key, From} ->
            db_ets:delete(Key, DbRef),
            From ! {reply, ok},
            loop(DbRef);
        {read, Key, From} ->
            Result = db_ets:read(Key, DbRef),
            From ! {ok, Result},
            loop(DbRef);
        {match, Val, From} ->
            Result = db_ets:match(Val, DbRef),
            From ! {reply, Result},
            loop(DbRef);
        stop ->
            db_ets:destroy(),
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

