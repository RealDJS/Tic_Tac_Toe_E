%% ============================================================================
%% Erlang Cheat Sheet
%% ============================================================================
%% This document provides a concise overview of Erlang programming fundamentals.
%% Each section includes code examples with comments explaining the concepts.
%% ============================================================================

%% ---------------------------------------------------------------------------
%% Data Types
%% ---------------------------------------------------------------------------
-module(cheat_sheet).

-export([
    dataTypes/0,
    variableExample/0,
    arithmetic_and_logical_operations/0,
    comparison_operators/0,
    conditionals/0
]).

dataTypes() ->
    %% Atoms: Constants whose value is their own name
    Atom = "Hello world",
    %% Numbers: Integers and floats
    Integer = 42,
    Float = 3.14,
    %% Strings: Lists of integers representing ASCII values
    String = "Hello, Erlang!",
    %% Tuples: Fixed-size collections of elements
    Tuple = {ok, "Success", 123},
    %% Lists: Variable-size collections of elements
    List = [1, 2, 3, 4, 5],
    %% Maps (Dictionaries): Key-value pairs
    Map = #{name => "Alice", age => 30},

    {Atom, Integer, Float, String, Tuple, List, Map}.

%% ---------------------------------------------------------------------------
%% Variables
%% ---------------------------------------------------------------------------

%% Variables are immutable and must start with an uppercase letter or underscore
variableExample() ->
    Variable = "I am immutable",
    Another_Var = 100,
    {Variable, Another_Var}.

%% ---------------------------------------------------------------------------
%% Expressions
%% ---------------------------------------------------------------------------

%% Arithmetic and Logical Operations
arithmetic_and_logical_operations() ->
    Sum = 1 + 2,
    Difference = 5 - 3,
    Product = 4 * 2,
    Quotient = 10 / 2,

    %% Logical Operations
    AndResult = true andalso false,
    OrResult = true orelse false,
    NotResult = not true,

    {Sum, Difference, Product, Quotient, AndResult, OrResult, NotResult}.

%% Comparison Operators
comparison_operators() ->
    %% true
    IsEqual = 1 =:= 1,
    %% true
    IsNotEqual = 1 =/= 2,
    %% true
    IsGreater = 3 > 2,
    %% true
    IsLess = 2 < 3,
    {IsEqual, IsNotEqual, IsGreater, IsLess}.

%% ---------------------------------------------------------------------------
%% Conditionals
%% ---------------------------------------------------------------------------

%% if Expression
conditionals() ->
    IfExample =
        if
            1 + 1 == 2 ->
                ok;
            true ->
                error
        end,

    %% case Expression
    CaseExample =
        case {ok, 1} of
            {ok, X} when X > 0 -> X;
            {error, Reason} -> Reason
        end,

    {IfExample, CaseExample}.

%% ---------------------------------------------------------------------------
%% Loops (Recursion)
%% ---------------------------------------------------------------------------

%% Erlang does not have traditional loops; recursion is used instead.

%% Example: Calculating factorial using recursion
factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

%% ---------------------------------------------------------------------------
%% Lists
%% ---------------------------------------------------------------------------

%% Creating Lists
EmptyList = [].
List = [1, 2, 3, 4, 5].

%% Head and Tail

%% Head = 1, Tail = [2,3,4,5]
[Head | Tail] = [1, 2, 3, 4, 5].

%% List Comprehensions
EvenNumbers = [X || X <- [1, 2, 3, 4, 5, 6], X rem 2 =:= 0].

%% ---------------------------------------------------------------------------
%% Dictionaries (Maps)
%% ---------------------------------------------------------------------------

%% Creating Maps
Map = #{name => "Bob", age => 25}.

%% Accessing Values

%% "Bob"
Name = maps:get(name, Map).

%% Adding/Updating Entries
UpdatedMap = Map#{age => 26}.

%% Deleting Entries
ReducedMap = maps:remove(age, Map).

%% ---------------------------------------------------------------------------
%% Input/Output
%% ---------------------------------------------------------------------------

%% Printing to Console
io:format("Hello, ~s!~n", ["World"]).

%% Reading from Console
read_input() ->
    io:format("Enter something: "),
    {ok, Input} = io:get_line(""),
    Input.

%% ---------------------------------------------------------------------------
%% File Operations
%% ---------------------------------------------------------------------------

%% Writing to a File
write_file(FileName, Content) ->
    {ok, File} = file:open(FileName, [write]),
    io:format(File, "~s", [Content]),
    file:close(File).

%% Reading from a File
read_file(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    Binary.

%% ---------------------------------------------------------------------------
%% Guards
%% ---------------------------------------------------------------------------

%% Guards are additional constraints in function clauses or case branches
is_positive(Number) when is_integer(Number), Number > 0 ->
    true;
is_positive(_) ->
    false.

%% ---------------------------------------------------------------------------
%% Pattern Matching in Function Parameters
%% ---------------------------------------------------------------------------

%% Function with Different Clauses Based on Pattern Matching
describe({ok, Value}) ->
    io:format("Success: ~p~n", [Value]);
describe({error, Reason}) ->
    io:format("Error: ~p~n", [Reason]).

%% ---------------------------------------------------------------------------
%% Recursion
%% ---------------------------------------------------------------------------

%% Example: Summing a List Using Recursion
sum_list([]) -> 0;
sum_list([Head | Tail]) -> Head + sum_list(Tail).

%% Tail-Recursive Example: Factorial
factorial(N) -> factorial(N, 1).

factorial(0, Acc) ->
    Acc;
factorial(N, Acc) when N > 0 ->
    factorial(N - 1, N * Acc).

%% ---------------------------------------------------------------------------
%% Lambda Functions and List Operations
%% ---------------------------------------------------------------------------

%% Using lists:map with a Lambda Function
Doubled = lists:map(fun(X) -> X * 2 end, [1, 2, 3, 4]).

%% Using lists:filter with a Lambda Function
Even = lists:filter(fun(X) -> X rem 2 =:= 0 end, [1, 2, 3, 4, 5, 6]).

%% Using lists:foldl with a Lambda Function
Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1, 2, 3, 4, 5]).

%% ---------------------------------------------------------------------------
%% Example Module Combining Concepts
%% ---------------------------------------------------------------------------

%% Define a module named 'cheatsheet'
-module(cheatsheet).
-export([start/0, factorial/1, describe/1, sum_list/1, write_file/2, read_file/1]).

%% Start function to demonstrate various features
start() ->
    %% Data Types and Variables
    Atom = hello,
    Number = 123,
    Str = "Erlang Cheat Sheet",
    Tuple = {ok, "All systems go"},
    List = [1, 2, 3, 4, 5],
    Map = #{key => "value"},

    %% Conditional
    ResultIf =
        if
            Number > 100 ->
                "Greater than 100";
            true ->
                "100 or less"
        end,

    %% Case Expression
    ResultCase =
        case Tuple of
            {ok, Msg} -> Msg;
            {error, Reason} -> Reason
        end,

    %% Recursion Example
    Fact = factorial(5),

    %% Pattern Matching
    describe(Tuple),

    %% List Operations with Lambda
    Doubled = lists:map(fun(X) -> X * 2 end, List),
    Evens = lists:filter(fun(X) -> X rem 2 =:= 0 end, List),
    Total = lists:foldl(fun(X, Acc) -> X + Acc end, 0, List),

    %% I/O Operations
    io:format("Atom: ~p~nNumber: ~p~nString: ~s~n~n", [Atom, Number, Str]),
    io:format("Tuple: ~p~nList: ~p~nMap: ~p~n~n", [Tuple, List, Map]),
    io:format("If Result: ~s~nCase Result: ~s~n~n", [ResultIf, ResultCase]),
    io:format("Factorial of 5: ~p~n~n", [Fact]),
    io:format("Doubled List: ~p~nEvens: ~p~nSum: ~p~n~n", [Doubled, Evens, Total]),

    %% File Operations
    write_file("example.txt", "This is a sample text."),
    FileContent = read_file("example.txt"),
    io:format("File Content: ~s~n", [FileContent]).

%% Factorial Function
factorial(N) ->
    factorial(N, 1).

factorial(0, Acc) ->
    Acc;
factorial(N, Acc) when N > 0 ->
    factorial(N - 1, N * Acc).

%% Describe Function with Pattern Matching
describe({ok, Value}) ->
    io:format("Status: OK, Value: ~p~n", [Value]);
describe({error, Reason}) ->
    io:format("Status: Error, Reason: ~p~n", [Reason]).

%% Sum List Function
sum_list([]) -> 0;
sum_list([Head | Tail]) -> Head + sum_list(Tail).

%% Write to File Function
write_file(FileName, Content) ->
    {ok, File} = file:open(FileName, [write]),
    io:format(File, "~s", [Content]),
    file:close(File).

%% Read from File Function
read_file(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    Binary.
