%%% Exercise 3.2 The Process Ring
-module(p_ring).
-author('skers@kth.se').

-export([start/2, loop/2, print/2]).

%% Start the first process, it has the highest N.
%% It's pid is passed along with every process.
start(N, Msgs) -> start(N, Msgs, []).

%% Start the last process
start(1, Msgs, Pids) ->
    Pid = spawn(?MODULE, loop, [Msgs, self()]),
    communicator(Pid, [Pid|Pids]),
    Pid;
start(N, Msgs, Pids) ->
    Pid = spawn(?MODULE, loop, [Msgs, self()]),
    start(N-1, Msgs, [Pid|Pids]).

%% Tell everyone who their next node is
communicator(Last, [Pid|Rest]) when length(Rest) == 0 ->
    Pid ! {your_next, Last},
    Last;
communicator(Last, [Pid|Rest]) ->
    [H|_] = Rest,
    Pid ! {your_next, H},
    communicator(Last, Rest).

%% Talk to the next
loop(Msgs, NextPid) ->
    receive
        {print, From, Message} ->
            io:format("~w <== ~s == ~w~n", [self(), Message, From]),
            if Msgs > 0 ->
                    NextPid ! {print, self(), Message},
                    loop(Msgs-1, NextPid);
                true ->
                    loop(0, NextPid)
            end;
        {your_next, Node} ->
            io:format("~w == next ==> ~w~n", [self(), Node]),
            loop(Msgs, Node);
        kill ->
            io:format("~w dies.~n", [self()]),
            NextPid ! kill,
            ok
    end.

print(Message, Pid) ->
    Pid ! {print, self(), Message}.

