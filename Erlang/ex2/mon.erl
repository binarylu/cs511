-module(mon).
-export([first_thread/0, second_thread/1]).

first_thread() ->
    {Pid2, _} = spawn_monitor(?MODULE, second_thread, [self()]),
    io:fwrite("second thread is ~p~n", [Pid2]),
    first_thread_loop(Pid2).

first_thread_loop(Pid2) ->
    {ok, [A, B]} = io:fread("enter two integers> ", "~d~d"),
    Pid2 ! {A, B},
    New = receive
        {'DOWN', _, process, _, Reason} ->
            {NewTh, _} = spawn_monitor(?MODULE, second_thread, [self()]),
            io:fwrite("restart second thread(~p); new thread is ~p~n", [Reason, NewTh]),
            NewTh;
        Sum ->
            io:fwrite("~p + ~p = ~p~n", [A, B, Sum]),
            Pid2
    end,
    first_thread_loop(New).

second_thread(Pid1) ->
    receive
        {Num1, Num2} ->
            Sum = Num1 + Num2,
            io:fwrite("thread ~p: ~p + ~p = ~p~n", [self(), Num1, Num2, Sum]),
            if
                Sum == 0 ->
                    exit(sum_is_zero);
                true ->
                    Pid1 ! Sum
            end
    end,
    second_thread(Pid1).
