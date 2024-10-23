-module(while).
-export([while/1, while/2, start/0]).

%  While with a list calls on another while function with 2 parameters.
while(L) -> while(L, 0).

% IF the List is empty, this runs.
while([], Acc) ->
    Acc;
% IF there are items in the list, writes the current item Acc and the function calls itself.
while([_ | T], Acc) ->
    io:fwrite("~w~n", [Acc]),
    while(T, Acc + 1).

% Start the function.
start() ->
    X = [1, 2, 3],
    while(X).
