% hello world program

% hyphen gives statements meaning. Statements are delimited by `.`
-module(hello_world).
% Imports the io module.
% -import(io,[fwrite/1]).
% Ensure the start function can be accessed

% Start = function name. Zero parameters
-export([start/0]).
start() ->
    % fwrite("Hello world!\n"),
    X = 40,
    Y = 50,
    X = 80,
    Result = X + Y,
    io:fwrite("~w", [Result]).
