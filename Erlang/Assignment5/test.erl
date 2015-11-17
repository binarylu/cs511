-module(test).
-export([test/0, th/0]).

test() ->
    A = d(0),
    io:fwrite("res: ~p~n", [A]),
    spawn(?MODULE, th, []),
    B = 0,
    case B of
        1 -> io:fwrite("case 1~n");
        _ -> io:fwrite("case other~n")
    end.

d(I) when I == 10 -> [I];
d(I) ->
    [I | d(I + 1)].

th() ->
    io:fwrite("I'm here.~n").
