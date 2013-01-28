%%% Exercise 6.2 Higher Order Functions
-module(higher_order).
-author('skers@kth.se').

-export([ range/1
        , smaller_than/2
        , even_ints/1
        , concat/1
        , sum/1
        ]).
-export([test/0]).

test() ->
    Lists = [[1,2,3], [4,5,6], [7,8,9]],
    concat(Lists).

% Using funs and higher order functions, write a function which prints out the
% integers between 1 and N
range(N) ->
    lists:foreach(fun(Num) -> io:format("~p~n", [Num]) end, lists:seq(1, N)).

% Using funs and higher order functions, write a function which given a list
% of integers and an integer, will return all integers smaller than or equal
% to that integer
smaller_than(X, List) ->
    lists:filter(fun(Num) -> Num =< X end, List).

% Using funs and higher order functions, write a function which prints out
% the even integers between 1 and N
even_ints(N) ->
    lists:filter(fun(Num) -> Num rem 2 == 0 end, lists:seq(1, N)).

% Using funs and higher order functions, write a function which, given a
% list of lists, will concatenate them
concat(Lists) ->
    lists:foldl(fun(L, New) -> New ++ L end, [], Lists).

% Using funs and higher order functions, write a function that given a list
% of integers returns the sum of the integers
sum(List) ->
    lists:foldl(fun(N, Sum) -> N + Sum end, 0, List).

