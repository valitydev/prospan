-module(prospan).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main(_Args) ->
    Data = jsone:decode(read_stdin(), [{object_format, map}]),

    erlang:display(Data),
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
    iolist_to_binary(read_stdin([], io:get_chars('', ?READ_BYTES))).

read_stdin(Buffer, eof) ->
    lists:reverse(Buffer);
read_stdin(Buffer, DataIn) ->
    read_stdin([DataIn | Buffer], io:get_chars('', ?READ_BYTES)).

write_stdout(IoList) ->
    io:fwrite(standard_io, "~s~n", [IoList]).
