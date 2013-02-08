%%% Exercise 6.1 Database Handling Using Records
-module(db_rec).
-author('skers@kth.se').

-export([new/0, destroy/1, write/3, read/2, match/2, delete/2]).
-export([test/0]).

-record(data, {key, val}).

test() ->
    Db0 = write(simon, berlin, []),
    Db1 = write(linda, london, Db0),
    Db2 = write(davve, sthlm, Db1),
    Db3 = write(simon, sthlm, Db2),
    Db4 = write(davve, sthlm, Db3),
    Db4.

new() ->
    [].

destroy(_) ->
    [].

write(Key, Val, Db) ->
    [#data{key = Key, val = Val} | delete(Key, Db)].

read(_, []) ->
    {error, instance};
read(Key, [#data{key = Key, val = Val}|_]) ->
    {ok, Val};
read(Key, [_|T]) ->
    read(Key, T).

match(Key, Db) ->
    match(Key, Db, []).

match(_, [], Results) ->
    Results;
match(Val, [D = #data{}|T], Result) when D#data.val == Val ->
    match(Val, T, [D#data.key | Result]);
match(Val, [_|T], Result) ->
    match(Val, T, Result).

delete(Key, Db) ->
    delete(Key, Db, []).

delete(_, [], NewDb) ->
    NewDb;
delete(Key, [D = #data{}|T], NewDb) when D#data.key == Key ->
    delete(Key, T, NewDb);
delete(Key, [H|T], NewDb) ->
    delete(Key, T, [H|NewDb]).

