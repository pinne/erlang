%%% Exercise 8.3 Database Handling using DETS
-module(db_dets).
-author('skers@kth.se').

-export([new/0, destroy/1, write/3, read/2, match/2, delete/2]).

-record(data, {key, val}).

%% Don't know how to fix my code to pass the test, I keep getting these
%% mysterious 'ok' where my <db name> should be
-define(DBFILE, db_dets).

new() -> new(?DBFILE).

new(DbFile) ->
    {_Status, Name} = dets:open_file(DbFile, [{keypos, #data.key}]),
    Name.

destroy(Db) ->
    %dets:delete(Db).
    dets:close(Db).

write(Key, Val, Db) ->
    case dets:insert(?DBFILE, #data{key = Key, val = Val}) of
        Db    -> Db;
        Error -> Error
    end.

read(Key, _Db) ->
    case dets:lookup(?DBFILE, Key) of
        [#data{key = Key, val = Val}] -> {ok, Val};
        _NotFound                     -> {error, instance}
    end.

match(Val, _Db) ->
    lists:flatten(dets:match(?DBFILE, #data{key = '$1', val = Val})).

delete(Key, _Db) ->
    dets:delete(?DBFILE, Key), ?DBFILE.

