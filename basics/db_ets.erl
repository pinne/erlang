%%% Exercise 7.2 Database Handling using ETS
-module(db_ets).
-author('skers@kth.se').

-export([new/0, destroy/1, write/3, read/2, match/2, delete/2]).

-record(data, {key, val}).

new() ->
    case ets:info(db_ets) of
        undefined ->
            ets:new(db_ets, [set, public, named_table, {keypos, #data.key}]);
        _Defined ->
            destroy(db_ets), new()
    end.

destroy(Db) ->
    ets:delete(Db).

write(Key, Val, Db) ->
    ets:insert(Db, #data{key = Key, val = Val}),
    Db.

read(Key, Db) ->
    case ets:lookup(Db, Key) of
        [#data{key = Key, val = Val}] -> {ok, Val};
        _NotFound                     -> {error, instance}
    end.

match(Val, Db) ->
    lists:flatten(ets:match(Db, #data{key = '$1', val = Val})).

delete(Key, Db) ->
    ets:delete(Db, Key), Db.

