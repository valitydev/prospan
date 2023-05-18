-module(prospan_zipkin).

-include("events.hrl").
-include("zipkin.hrl").

-export([collect_spans/2]).

-type map_w_binaries() :: #{binary() => map_w_binaries() | binary()}.
-type event() :: map_w_binaries().
-type opt() :: timestamp.

-spec collect_spans([event()], [opt()]) -> [span()].
collect_spans(Events, Opts) ->
    Spans = #{},
    Map = #{trace_id => #{}, span_id => #{}},
    collect_spans(Spans, Map, Events, [], 10, Opts).

collect_spans(Spans, _Map, [], _PostponedEvents, 0, _Opts) ->
    maps:values(Spans);
collect_spans(Spans, Map, [], PostponedEvents, PassCount, Opts) ->
    %% Try to collect span data from postponed entries
    collect_spans(Spans, Map, PostponedEvents, [], PassCount - 1, Opts);
collect_spans(Spans0, Map0, [Event0 | Events], PostponedEvents, PassCount, Opts) ->
    Event1 = normalize_event(Event0),
    case process_span(Event1, Spans0, Map0) of
        postpone ->
            collect_spans(Spans0, Map0, Events, [Event0 | PostponedEvents], PassCount, Opts);
        {ok, Spans1, Map1} ->
            collect_spans(Spans1, Map1, Events, PostponedEvents, PassCount, Opts)
    end.

%

process_span(?WOODY_CLIENT_CALL(From, Service, Function) = Event, Spans, Map) ->
    case get_span('CLIENT', Event, Spans, Map) of
        undefined ->
            create_span('CLIENT', Event, Spans, Map, fun(Span0) ->
                Span0#{
                    'name' => <<Service/binary, "::", Function/binary, "()">>,
                    'timestamp' => get_timestamp(Event),
                    'localEndpoint' => construct_endpoint(From),
                    'remoteEndpoint' => construct_endpoint(prospan_conf:alias_rpc_service(Service)),
                    'tags' => get_tags(Event, [])
                }
            end);
        #{'id' := _SpanID} = _Span0 ->
            {ok, Spans, Map}
    end;
process_span(?WOODY_CLIENT_RESULT(_From, _Service, _Function) = Event, Spans, Map) ->
    with_span_or_postpone_event('CLIENT', Event, Spans, Map, fun(Span0) ->
        update_span_duration(Span0, Event)
    end);
process_span(
    ?WOODY_SERVER_EVENT_SCOPED(_Scopes, At, Service, <<"call">>, ?EV_INVOKE_SERVICE_HANDLER, Function, _Url) = Event,
    Spans,
    Map
) ->
    case get_span('SERVER', Event, Spans, Map) of
        undefined ->
            create_span('SERVER', Event, Spans, Map, fun(Span0) ->
                Span0#{
                    'name' => <<Service/binary, "::", Function/binary, "()">>,
                    'timestamp' => get_timestamp(Event),
                    'localEndpoint' => construct_endpoint(At),
                    'remoteEndpoint' => undefined,
                    'tags' => get_tags(Event, [])
                }
            end);
        #{'id' := _SpanID} = _Span0 ->
            {ok, Spans, Map}
    end;
process_span(
    ?WOODY_SERVER_EVENT_SCOPED(_Scopes, _At, _Service, <<"call">>, ?EV_SERVICE_HANDLER_RESULT, _Function, _Url) = Event,
    Spans,
    Map
) ->
    with_span_or_postpone_event('SERVER', Event, Spans, Map, fun(Span0) ->
        update_span_duration(Span0, Event)
    end);
process_span(_Event, Spans, Map) ->
    {ok, Spans, Map}.

%

update_span_duration(Span, Event) ->
    Duration = get_timestamp(Event) - maps:get('timestamp', Span),
    UpdateDuration = fun
        (undefined) -> Duration;
        (PrevDuration) when Duration < PrevDuration -> PrevDuration;
        (PrevDuration) when Duration >= PrevDuration -> Duration
    end,
    maps:update_with('duration', UpdateDuration, Span).

get_tags(Event, []) ->
    to_binary_map(maps:merge(get_scope_tags(Event), get_common_tags(Event)));
get_tags(Event, Blacklist) ->
    Filter = fun({K, _V}) ->
        lists:any(fun(E) -> E =:= binary_part(K, 0, byte_size(E)) end, Blacklist)
    end,
    maps:filter(Filter, get_tags(Event, [])).

get_scope_tags(Event) ->
    maps:with(maps:get(<<"scoper">>, Event, []), Event).

get_common_tags(Event) ->
    #{<<"original">> => maps:with([<<"trace_id">>, <<"parent_id">>, <<"span_id">>], Event)}.

to_binary_map(Map) when is_map(Map) ->
    maps:from_list(lists:flatten(collect_tuples(maps:to_list(Map), <<>>, []))).

collect_tuples([], _Prefix, Tuples) ->
    Tuples;
collect_tuples([{K, V} | Rest], Prefix, Tuples) ->
    collect_tuples(Rest, Prefix, [do_collect(K, V, Prefix) | Tuples]).

do_collect(K, true, Prefix) ->
    {<<Prefix/binary, K/binary>>, <<"1">>};
do_collect(K, false, Prefix) ->
    {<<Prefix/binary, K/binary>>, <<"0">>};
do_collect(K, V, Prefix) when is_atom(V) -> {<<Prefix/binary, K/binary>>, atom_to_binary(V)};
do_collect(K, V, Prefix) when is_binary(V) -> {<<Prefix/binary, K/binary>>, V};
do_collect(K, V, Prefix) when is_integer(V) -> {<<Prefix/binary, K/binary>>, integer_to_binary(V)};
do_collect(K, V, Prefix) when is_float(V) -> {<<Prefix/binary, K/binary>>, float_to_binary(V, [{decimals, 3}])};
do_collect(K, V, Prefix) when is_map(V) -> collect_tuples(maps:to_list(V), iolist_to_binary([Prefix, [K, "."]]), []);
do_collect(_K, [], _Prefix) ->
    [];
do_collect(K, V, Prefix0) when is_list(V) ->
    Prefix1 = iolist_to_binary([Prefix0, K]),
    IndexedValues = lists:zip(lists:seq(0, length(V) - 1), V),
    [
        do_collect(iolist_to_binary(["[", integer_to_binary(Index), "]"]), Item, Prefix1)
     || {Index, Item} <- IndexedValues
    ].

% annotate_span(Span, Annotator) when is_function(Annotator) ->
%     maps:update_with('annotations', Annotator, Span).

construct_endpoint(Service) ->
    construct_endpoint(Service, undefined, undefined).

construct_endpoint(Service, IP, Port) ->
    #{
        'serviceName' => Service,
        'ipv4' => IP,
        'ipv6' => undefined,
        'port' => Port
    }.

construct_span(Kind, Event, Map0) ->
    {TraceID, Map1} = alias(trace_id, Kind, Event, Map0),
    {ParentID, Map2} = alias(parent_id, Kind, Event, Map1),
    {SpanID, Map3} = alias(span_id, Kind, Event, Map2),
    Span = #{
        'traceId' => TraceID,
        'id' => SpanID,
        'parentId' => ParentID,
        'name' => undefined,
        'kind' => Kind,
        'timestamp' => undefined,
        'duration' => undefined,
        'debug' => false,
        'shared' => false,
        'localEndpoint' => undefined,
        'remoteEndpoint' => undefined,
        'annotations' => [],
        'tags' => #{}
    },
    {Span, Map3}.

create_span(Kind, Event, Spans0, Map0, WithFunc) when is_function(WithFunc, 1) ->
    {#{'id' := SpanID} = Span0, Map1} = construct_span(Kind, Event, Map0),
    Span1 = WithFunc(Span0),
    {ok, maps:put(SpanID, Span1, Spans0), Map1}.

get_span(Kind, Event, Spans0, Map) ->
    {SpanID, _Map} = alias(span_id, Kind, Event, Map),
    maps:get(SpanID, Spans0, undefined).

with_span_or_postpone_event(Kind, Event, Spans0, Map0, WithFunc) when is_function(WithFunc, 1) ->
    case get_span(Kind, Event, Spans0, Map0) of
        #{'id' := SpanID} = Span -> {ok, maps:put(SpanID, WithFunc(Span), Spans0), Map0};
        undefined -> postpone
    end.

get_timestamp(#{<<"@timestamp">> := Ts}) ->
    prospan_utils:to_microseconds(Ts).

alias(trace_id, _Kind, #{<<"trace_id">> := EventTraceID}, #{trace_id := TraceIDMap0} = Map) ->
    %% Span kind does not affect trace_id translation
    {TraceID, TraceIDMap1} = put_gen_id(EventTraceID, TraceIDMap0, 32),
    {TraceID, maps:put(trace_id, TraceIDMap1, Map)};
alias(parent_id, _Kind, #{<<"parent_id">> := EventParentID}, Map) ->
    %% Parent is always client [kind of a] span
    mk_zipkin_span_id(<<(atom_to_binary('CLIENT'))/binary, EventParentID/binary>>, Map);
alias(span_id, Kind, #{<<"span_id">> := EventSpanID}, Map) ->
    %% Spans for client and server and same "original" span_id must differ
    mk_zipkin_span_id(<<(atom_to_binary(Kind))/binary, EventSpanID/binary>>, Map).

mk_zipkin_span_id(ComplexID, #{span_id := SpanIDMap0} = Map) ->
    {SpanID, SpanIDMap1} = put_gen_id(ComplexID, SpanIDMap0, 16),
    {SpanID, maps:put(span_id, SpanIDMap1, Map)}.

put_gen_id(Key, Map, IdSize) ->
    prospan_utils:define_id(Key, Map, fun
        (undefined) -> undefined;
        (_Else) -> prospan_utils:rand_id(IdSize)
    end).

normalize_event(#{<<"rpc.server">> := #{<<"event">> := EventType} = ClientData} = Event) when is_binary(EventType) ->
    normalize_event(Event#{<<"rpc.server">> => ClientData#{<<"event">> => binary_to_existing_atom(EventType)}});
normalize_event(#{<<"rpc.client">> := #{<<"event">> := EventType} = ClientData} = Event) when is_binary(EventType) ->
    normalize_event(Event#{<<"rpc.client">> => ClientData#{<<"event">> => binary_to_existing_atom(EventType)}});
normalize_event(Event) ->
    Event.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_binary_map_test_() -> [_].
to_binary_map_test_() ->
    [
        ?_assert(
            #{
                <<"function">> => <<"ProcessSignal">>,
                <<"metadata.user-identity.realm">> => <<"external">>,
                <<"metadata.sub.float_value">> => <<"42.000">>,
                <<"metadata.sub.list[0]">> => <<"1">>,
                <<"metadata.sub.list[1]">> => <<"0">>,
                <<"metadata.sub.list[2]">> => <<"test">>,
                <<"count">> => <<"10">>,
                <<"external">> => <<"1">>
            } =:=
                to_binary_map(#{
                    <<"function">> => <<"ProcessSignal">>,
                    <<"metadata">> => #{
                        <<"user-identity.realm">> => <<"external">>,
                        <<"sub">> => #{
                            <<"float_value">> => 42.0,
                            <<"list">> => [1, false, <<"test">>]
                        }
                    },
                    <<"count">> => 10,
                    <<"external">> => true
                })
        )
    ].

-endif.
