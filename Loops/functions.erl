-module(functions).
-export([add/1, add/2, add/3, start/0]).

add(X) when X > 3 ->
    io:fwrite("~w~n", [X]).

add(X, Y) ->
    Z = X + Y,
    io:fwrite("~w~n", [Z]).

add(X, Y, Z) ->
    A = X + Y + Z,
    io:fwrite("~w~n", [A]).

start() ->
    add(4),

    add(5, 6),

    add(5, 6, 7),

    Fn = fun() ->
        io:fwrite("Anonymous Function\n")
    end,
    Fn().
