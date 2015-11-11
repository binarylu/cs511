-module(test).
-export([start/0, loop/0]).

start() ->
    {ok, [Num1, Num2]} = io:fread("first_thread: enter two integers please> ", "~d~d"),
    NewProcess = spawn(test, loop, []),
    NewProcess ! {self(), 'hello world'}.

loop() ->
    receive
        {Sender, Msg} ->
            io:fwrite("Received ~p from ~s~n", [Msg, Sender]),
            loop()
    end.