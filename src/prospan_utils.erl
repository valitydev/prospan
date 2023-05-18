-module(prospan_utils).

-export([read_stdin/0]).
-export([unwrap_messages/1]).
-export([to_microseconds/1]).
-export([colorize/1]).
-export([has_option/2]).
-export([define_id/3]).
-export([rand_id/1]).

-define(READ_BYTES, 8192).

-spec read_stdin() -> binary().
read_stdin() ->
    iolist_to_binary(read_stdin([], io:get_chars('', ?READ_BYTES))).

read_stdin(Buffer, eof) ->
    lists:reverse(Buffer);
read_stdin(Buffer, DataIn) ->
    read_stdin([DataIn | Buffer], io:get_chars('', ?READ_BYTES)).

-type kibana_map() :: map().
-type log_event() :: map().

%% We expect events to be sorted by '@timestamp' desc
-spec unwrap_messages(kibana_map()) -> list(log_event()).
unwrap_messages(#{<<"hits">> := #{<<"hits">> := EventHits}}) when is_list(EventHits) ->
    [normalize_event(Event) || #{<<"_source">> := Event} <- EventHits, is_interpretable(Event)].

is_interpretable(#{
    <<"trace_id">> := _,
    <<"span_id">> := _,
    <<"@timestamp">> := _,
    <<"@severity">> := _,
    <<"service">> := _
}) ->
    true;
is_interpretable(_Event) ->
    false.

normalize_event(Event) ->
    maps:filter(
        fun
            (<<"stream">>, _V) ->
                false;
            (<<"pid">>, _V) ->
                false;
            (<<"message">>, _V) ->
                false;
            (<<"kubernetes.", _/binary>>, _V) ->
                false;
            (_K, _V) ->
                true
        end,
        Event
    ).

-spec to_microseconds(binary()) -> integer().
to_microseconds(Rfc3339) when is_binary(Rfc3339) ->
    calendar:rfc3339_to_system_time(binary_to_list(Rfc3339), [{unit, microsecond}]).

-spec colorize(binary()) -> binary().
colorize(Binary) when is_binary(Binary) ->
    to_3byte_hex(make_hash(Binary, 0)).

make_hash(<<>>, Hash) ->
    <<Hash:32>>;
make_hash(<<Byte/integer, Rest/binary>>, Hash) ->
    make_hash(Rest, Byte + (Hash bsl 5) - Hash).

to_3byte_hex(<<_:8, R:8, G:8, B:8>>) ->
    <<(byte_to_hex_code(Byte)) || Byte <- [R, G, B]>>.

byte_to_hex_code(Byte) ->
    case integer_to_binary(Byte, 16) of
        Incomplete when byte_size(Incomplete) =:= 1 -> <<"0", Incomplete/binary>>;
        Complete -> Complete
    end.

-spec has_option(atom(), list(atom())) -> boolean().
has_option(Key, Opts) ->
    lists:member(Key, Opts) orelse lists:keyfind(Key, 1, Opts) /= false.

-spec define_id(binary(), #{binary() := binary()}, fun(() -> binary())) -> {binary(), #{binary() := binary()}}.
define_id(ID, IDMapping, Generator) when is_function(Generator, 1) ->
    case maps:get(ID, IDMapping, undefined) of
        undefined ->
            GeneratedID = list_to_binary(Generator(ID)),
            {GeneratedID, maps:put(ID, GeneratedID, IDMapping)};
        ExistingID ->
            {ExistingID, IDMapping}
    end.

-spec rand_id(integer()) -> string().
rand_id(Length) ->
    rand_id(Length, "abcdef1234567890").

rand_id(Length, Chars) ->
    Folder = fun(_I, Acc) ->
        [lists:nth(rand:uniform(length(Chars)), Chars)] ++ Acc
    end,
    lists:foldl(Folder, [], lists:seq(1, Length)).
