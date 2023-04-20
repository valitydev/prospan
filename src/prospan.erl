-module(prospan).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main(Args) ->
    erlang:display(Args),
    erlang:display(read_stdin()),
    %% Example plantuml
    Out = [
        "@startuml\n",
        "Alice -> Bob: Authentication Request\n",
        "Bob --> Alice: Authentication Response\n",
        "\n",
        "Alice -> Bob: Another authentication Request\n",
        "Alice <-- Bob: Another authentication Response\n",
        "@enduml\n"
    ],
    write_stdout(Out),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-define(READ_BYTES, 8192).

read_stdin() ->
    read_stdin([], io:get_chars('', ?READ_BYTES)).

read_stdin(Buffer, eof) ->
    lists:revers(Buffer);
read_stdin(Buffer, DataIn) ->
    [DataIn | Buffer].

write_stdout(IoList) ->
    io:format("~s~n", [IoList]).
