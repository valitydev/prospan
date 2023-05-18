-module(prospan).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main(Args) ->
    Opts = parse_options(Args),
    _ = not prospan_utils:has_option(pipe, Opts) andalso print_usage_information(),
    RawData = prospan_utils:read_stdin(),
    Data = jsone:decode(RawData, [{object_format, map}]),
    Events = prospan_utils:unwrap_messages(Data),
    Out = prospan_sequence:from_events(Events, Opts),
    write_stdout(["@startuml\n", Out, "@enduml\n"]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-define(INPUT_ERROR, 128).

parse_options(Args) ->
    case getopt:parse(get_options_spec(), Args) of
        {ok, {Opts, _RestArgs}} ->
            _ = prospan_utils:has_option(help, Opts) andalso print_usage_information(),
            Opts;
        {error, _} ->
            print_usage_information()
    end.

print_usage_information() ->
    getopt:usage(get_options_spec(), ?MODULE_STRING),
    erlang:halt(?INPUT_ERROR).

get_options_spec() ->
    [
        {help, $h, "help", undefined, "Print this message and exit"},
        {pipe, $p, "pipe", undefined, "Pipe json log from elastic into stdin"},
        {timestamp, $t, "timestamp", undefined, "Mark sequence actions with according timestamps"},
        {metadata, $m, "metadata", undefined, "Add span metadata to sequence actions comments"}
    ].

write_stdout(IoList) ->
    io:fwrite(standard_io, "~s~n", [IoList]).
