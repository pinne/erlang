%%% Exercise 7.1 List comprehensions
-module(list_comp).
-author('skers@kth.se').

-export([ div_by_3/0
        , rem_and_sq/1
        , intersection/2
        , disjunction/2
        ]).
-export([test/0]).

test() ->
    div_by_3(),
    rem_and_sq([1, hello, 100, "boo", 9]),
    intersection([1,2,3,4,5], [4,5,6,7,8]),
    disjunction([1,2,3,4,5], [4,5,6,7,8]).


%%% Using List comprehensions, create a set of integers between 1 and 10 which
%%% are divisible by three
div_by_3() ->
    [ X || X <- lists:seq(1,10), X rem 3 == 0 ].

%%% Using list comprehensions remove all non integers from a polymorphic list.
%%% Return the list of integers squared
rem_and_sq(List) ->
    [ X * X || X <- List, is_number(X) ]. 

%%% Using list comprehensions and given two lists, return a new list which is
%%% the intersection of the two lists
intersection(A, B) ->
    [ X || X <- A, Y <- B, X =:= Y ].

%%% Using list comprehensions and given two lists, return a new list which is
%%% the disjunction of the two lists
disjunction(A, B) ->
    (A -- B) ++ (B -- A).

