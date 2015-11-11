-module(calc).
-expoert([receiver/0]).

receiver() -> rcv_loop().

rcv_loop() ->
    receive
        {Sender, Msg} ->
            %io:fwrite("Received ~p from ~s~n", [Msg, Sender]),
            io:format("abc"),
            rcv_loop()
    end.
