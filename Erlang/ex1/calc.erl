-module(calc).
-export([thread1/0, thread2/1]).

thread1() ->
    Pid2 = spawn(?MODULE, thread2, [self()]),
    io:fwrite("module: ~p~n", [?MODULE]),
    thread1_loop(Pid2).

thread1_loop(Pid2) ->
    {ok, [A, B]} = io:fread("enter two integers> ", "~d~d"),
    Pid2 ! {A, B},
    receive
        0 ->
            io:fwrite("~p + ~p = 0~n", [A, B]),
            halt();
        Sum ->
            io:fwrite("~p + ~p = ~p~n", [A, B, Sum]),
            thread1_loop(Pid2)
    end.

thread2(Pid1) ->
    receive
        {Num1, Num2} ->
            Sum = Num1 + Num2,
            Pid1 ! Sum;
        _ ->
            io:fwrite("thread2: received unexpected message~n", [])
    end,
    thread2(Pid1).
