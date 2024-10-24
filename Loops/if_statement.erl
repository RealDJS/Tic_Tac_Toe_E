-module(if_statement).
-export([start/0]).

start() ->
    A = 5,
    B = 6,
    C = 5,
    D = 6,

    if
        A == B -> io:fwrite("True\n");
        true -> io:fwrite("False\n")
    end,

    if
        C == D ->
            io:fwrite("C is equal to D\n");
        C < D ->
            io:fwrite("C is less than D\n");
        true ->
            io:fwrite("False\n")
    end.
