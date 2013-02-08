%%% Exercise 2.4 Database Handling Using Lists
-module(db).
-author('skers@kth.se').

-export([new/0, destroy/1, write/3, read/2, match/2, delete/2]).

new() ->
    [].

destroy(_) ->
    [].

write(Key, Val, Db) ->
    write(Key, Val, Db, []).

write(Key, Val, [], NewDb) ->
    NewDb ++ [{Key, Val}];
write(Key, Val, [{Key, _}|T], NewDb) ->
    write(Key, Val, T, NewDb);
write(Key, Val, [{Key1, Val1}|T], NewDb) ->
    write(Key, Val, T, [{Key1, Val1}|NewDb]).

read(_, []) ->
    {error, instance};
read(Key, [{Key, Val}|_]) ->
    {ok, Val};
read(Key, [_|T]) ->
    read(Key, T).

match(Key, Db) ->
    match(Key, Db, []).

match(_, [], Results) ->
    Results;
match(Val, [{Key, Val}|T], Result) ->
    match(Val, T, [Key | Result]);
match(Val, [_|T], Result) ->
    match(Val, T, Result).

delete(Key, Db) ->
    delete(Key, Db, []).

delete(_, [], NewDb) ->
    NewDb;
delete(Key, [{Key, _}|T], NewDb) ->
    delete(Key, T, NewDb);
delete(Key, [H|T], NewDb) ->
    delete(Key, T, [H|NewDb]).

