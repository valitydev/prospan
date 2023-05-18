-module(prospan_sequence).

-include("events.hrl").

-export([from_events/2]).

-type map_w_binaries() :: #{binary() => map_w_binaries() | binary()}.
-type event() :: map_w_binaries().
-type opt() :: timestamp.

-spec from_events([event()], [opt()]) -> iolist().
from_events(Events0, Opts) ->
    _Types = ensure_woody_event_types_loaded(),
    Events1 = sort_events(Events0, by_span),
    from_events_(Events1, Opts, #{}, []).

sort_events([], _Any) ->
    [];
sort_events(Events, naive) ->
    lists:sort(fun compare_events_asc/2, Events);
sort_events(Events0, by_span) ->
    Events1 = sort_events(Events0, naive),
    {SpansOrder, Spans} = prepare_event_spans(Events1, [], #{}),
    expand_event_spans(SpansOrder, Spans, []).

prepare_event_spans([], Order, Spans) ->
    {lists:reverse(Order), Spans};
prepare_event_spans([#{<<"parent_id">> := ParentID} = Event | Events], Order0, Spans) ->
    {Order1, SpanEvents} =
        case Spans of
            #{ParentID := Span} -> {Order0, [Event | Span]};
            _Else -> {[ParentID | Order0], [Event]}
        end,
    prepare_event_spans(Events, Order1, Spans#{ParentID => SpanEvents}).

expand_event_spans([], _Spans, Expanded) ->
    lists:flatten(lists:reverse(Expanded));
expand_event_spans([ParentID | ParentIDs], Spans0, Expanded) ->
    {ExpandedSpan, Spans1} = do_expand(ParentID, Spans0),
    expand_event_spans(ParentIDs, Spans1, [ExpandedSpan | Expanded]).

do_expand(SpanID, Spans0) ->
    {ExpandedSpan0, Spans1} = expand_span(SpanID, Spans0),
    expand_span_events(ExpandedSpan0, Spans1, []).

expand_span_events([], Spans, Expanded) ->
    {lists:reverse(Expanded), Spans};
expand_span_events([#{<<"span_id">> := SpanID} = Event | Events], Spans0, Expanded) ->
    {ExpandedSpan, Spans1} = do_expand(SpanID, Spans0),
    expand_span_events(Events, Spans1, [wrap_in_span_box(SpanID, [Event | ExpandedSpan]) | Expanded]).

wrap_in_span_box(_SpanID, []) ->
    [];
wrap_in_span_box(_SpanID, Events) ->
    Events.
% wrap_in_span_box(SpanID, Events) ->
%     [{span_start, SpanID}, Events, {span_finish, SpanID}].

expand_span(SpanID, Spans0) ->
    case Spans0 of
        #{SpanID := Span} -> {lists:reverse(Span), maps:remove(SpanID, Spans0)};
        _Else -> {[], Spans0}
    end.

from_events_([], _Opts, _Ctx, Actions) ->
    lists:reverse(Actions);
from_events_([Event | Events], Opts, Ctx0, Actions0) ->
    {ok, Actions1, Ctx1} = interpret_event(normalize_event(Event), Actions0, Ctx0, Opts),
    from_events_(Events, Opts, Ctx1, Actions1).

normalize_event(#{<<"rpc.server">> := #{<<"event">> := EventType} = ClientData} = Event) when is_binary(EventType) ->
    normalize_event(Event#{<<"rpc.server">> => ClientData#{<<"event">> => binary_to_existing_atom(EventType)}});
normalize_event(#{<<"rpc.client">> := #{<<"event">> := EventType} = ClientData} = Event) when is_binary(EventType) ->
    normalize_event(Event#{<<"rpc.client">> => ClientData#{<<"event">> => binary_to_existing_atom(EventType)}});
normalize_event(Event) ->
    Event.

compare_events_asc(#{<<"@timestamp">> := TsA}, #{<<"@timestamp">> := TsB}) ->
    prospan_utils:to_microseconds(TsA) < prospan_utils:to_microseconds(TsB).

interpret_event({span_start, SpanID}, Actions, Ctx, _Opts) ->
    {ok, [["group span=", SpanID, "\n"] | Actions], Ctx};
interpret_event({span_finish, _SpanID}, Actions, Ctx, _Opts) ->
    {ok, [["end\n"] | Actions], Ctx};
interpret_event(?WOODY_CLIENT_CALL(From, Service, _Function) = Event, Actions, Ctx, Opts) ->
    ActionDesc = client_call_msg(Event, Ctx, Opts),
    Client = normalize_service_name(From),
    Server = prospan_conf:alias_rpc_service(Service),
    Action = build_action(Client, arrow(right, get_span_id(Event)), Server, ActionDesc),
    NewAction = [Action, toggle(Client, on, Event), toggle(Server, on, Event)],
    {ok, [NewAction | Actions], Ctx};
interpret_event(?WOODY_CLIENT_RESULT(From, Service, _Function) = Event, Actions, Ctx, Opts) when
    %% Machinegun client calls only WARN or higher, won't match request-response pattern
    Service =/= <<"Processor">>
->
    ActionDesc = client_result_msg(Event, Ctx, Opts),
    Client = normalize_service_name(From),
    Server = prospan_conf:alias_rpc_service(Service),
    Action = build_action(Client, arrow(left_dotted, get_span_id(Event)), Server, ActionDesc),
    {ok, [[Action, toggle(Server, off, Event), toggle(Client, off, Event)] | Actions], Ctx};
interpret_event(?SWAGGER_ENTRY_CALL(Service, Operation, Parameters), Actions, Ctx, _Opts) ->
    Server = normalize_service_name(Service),
    Action = ["?->", quote_id(Server), ": ", stringify_swagger_op(Operation, Parameters), "\n"],
    {ok, [[Action] | Actions], Ctx};
interpret_event(?WOODY_SERVER_MACHINE_INVOKED(At) = Event, Actions, Ctx, _Opts) ->
    Server = normalize_service_name(At),
    Action = machinery_note([Server, prospan_conf:alias_rpc_service(<<"Automaton">>)], Event),
    {ok, [Action | Actions], Ctx};
interpret_event(?WOODY_SERVER_INVOKE(At, Service, Function, Url) = _Event, Actions, Ctx, _Opts) ->
    Server = normalize_service_name(At),
    Action = ["group ", Service, "::", Function, "() @", quote_id(Server), " [", Url, "]\n"],
    {ok, [Action | Actions], Ctx};
interpret_event(?WOODY_SERVER_RESULT(At, Service, Function, Url) = Event, Actions, Ctx, Opts) ->
    NewActions =
        case Actions of
            %% Unstack obsolete grouping and draw an automaton signal arrow for Processor
            [["group ", Service, "::", Function | _] | OldActions] ->
                case Service of
                    <<"Processor">> ->
                        Server = normalize_service_name(At),
                        Action = build_action(
                            Server,
                            arrow(signal_left, get_span_id(Event)),
                            prospan_conf:alias_rpc_service(<<"Automaton">>),
                            processor_signal_msg(Service, Function, Url, Event, Ctx, Opts)
                        ),
                        [Action | OldActions];
                    _Else ->
                        OldActions
                end;
            _Else ->
                [["end", "\n"] | Actions]
        end,
    {ok, NewActions, Ctx};
interpret_event(_Event, Actions, Ctx, _Opts) ->
    {ok, Actions, Ctx}.

processor_signal_msg(Service, Function, Url, Event, Ctx, Opts) ->
    Msg0 = ["<b>", Service, "</b>::<b>", Function, "</b>()\\n", Url],
    case prospan_utils:has_option(timestamp, Opts) of
        true -> Msg0 ++ ["\\n", get_seq_mark(Event, Ctx, Opts), "\n"];
        _Else -> Msg0 ++ ["\n"]
    end.

machinery_note(Services, #{<<"machine">> := MachineCtx}) ->
    [
        "note over ",
        lists:join(", ", lists:map(fun quote_id/1, Services)),
        ": Machine context: ",
        machine_ctx_msg(MachineCtx),
        "\n"
    ];
machinery_note(_Services, _Event) ->
    [].

machine_ctx_msg(#{<<"activity">> := <<"call">>, <<"id">> := ID, <<"namespace">> := NS}) ->
    ["Call to <b>", NS, " ", ID, "</b>"];
machine_ctx_msg(#{<<"activity">> := <<"signal">>, <<"signal">> := Signal, <<"id">> := ID, <<"namespace">> := NS}) ->
    ["Signal <u>", Signal, "</u> to <b>", NS, " ", ID, "</b>"].

format_severity(#{<<"@severity">> := <<"ERROR">> = Severity}) -> do_format_severity(Severity);
format_severity(#{<<"@severity">> := <<"WARN">> = Severity}) -> do_format_severity(Severity);
format_severity(#{<<"@severity">> := _Severity}) -> [].

do_format_severity(Severity) -> ["<b><i>! ", Severity, " !</i></b>"].

stringify_swagger_op(Operation, Parameters) ->
    ["<b>", Operation, "</b>(", lists:map(fun({K, V}) -> [K, ": ", V] end, maps:to_list(Parameters)), ")"].

client_call_msg(?WOODY_CLIENT_CALL(_From, Service, Function) = Event, Ctx, Opts) ->
    Msg0 = [format_severity(Event), "\\n", stringify_call(Service, Function)],
    case prospan_utils:has_option(timestamp, Opts) of
        true -> Msg0 ++ ["\\n", get_seq_mark(Event, Ctx, Opts)];
        _Else -> Msg0
    end.

client_result_msg(?WOODY_CLIENT_RESULT(_From, _Service, _Function) = Event, Ctx, Opts) ->
    Msg0 = [format_severity(Event)],
    case prospan_utils:has_option(timestamp, Opts) of
        true -> Msg0 ++ ["\\n", get_seq_mark(Event, Ctx, Opts)];
        _Else -> Msg0
    end.

arrow(signal_left, Code) -> ["o<<", inline_code_color(Code), "-"];
arrow(right, Code) -> ["-", inline_code_color(Code), ">"];
arrow(left, Code) -> ["<", inline_code_color(Code), "-"];
arrow(right_dotted, Code) -> ["-", inline_code_color(Code), "->"];
arrow(left_dotted, Code) -> ["<-", inline_code_color(Code), "-"].

inline_code_color(<<"">>) -> "";
inline_code_color(Code) -> ["[#", prospan_utils:colorize(Code), "]"].

get_span_id(#{<<"span_id">> := SpanID}) -> SpanID.

get_seq_mark(#{<<"@timestamp">> := Ts, <<"parent_id">> := ParentID, <<"span_id">> := SpanID} = _Event, _Ctx, Opts) ->
    Color = prospan_utils:colorize(SpanID),
    Msg0 = [font(Color, Ts)],
    % [Date, Time] = binary:split(Ts, <<$T>>),
    % Msg0 = [font(Color, Date), "\\n", font(Color, Time)],
    case prospan_utils:has_option(metadata, Opts) of
        true -> Msg0 ++ ["\\nparent=", ParentID, "\\nspan=", SpanID];
        _Else -> Msg0
    end.

font(Color, Content) ->
    ["<font color=#", Color, ">", Content, "</font>"].

toggle(ActorID, on, Event) ->
    Color = colorize_event_span(Event),
    ["activate ", quote_id(ActorID), " #", Color, "\n"];
toggle(ActorID, off, _Event) ->
    ["deactivate ", quote_id(ActorID), "\n"].

colorize_event_span(#{<<"parent_id">> := CorrelationID}) ->
    prospan_utils:colorize(CorrelationID).

build_action(Left, Arrow, Right, <<"">>) ->
    [quote_id(Left), " ", Arrow, " ", quote_id(Right), "\n"];
build_action(Left, Arrow, Right, Comment) ->
    [quote_id(Left), " ", Arrow, " ", quote_id(Right), ": ", Comment, "\n"].

quote_id(ID) when is_binary(ID) ->
    binary:replace(ID, <<$->>, <<$_>>, [global]).

stringify_call(Service, Function) ->
    ["<b>", Service, "</b>::<b>", Function, "</b>()"].

normalize_service_name(ServiceName) ->
    ServiceName.

ensure_woody_event_types_loaded() ->
    %% Dirty hack, don't bonk me
    [
        % 'call service'
        ?EV_CALL_SERVICE,
        % 'service result'
        ?EV_SERVICE_RESULT,

        % 'client begin'
        ?EV_CLIENT_BEGIN,
        % 'client send'
        ?EV_CLIENT_SEND,
        % 'client resolve begin'
        ?EV_CLIENT_RESOLVE_BEGIN,
        % 'client resolve result'
        ?EV_CLIENT_RESOLVE_RESULT,
        % 'client receive'
        ?EV_CLIENT_RECEIVE,
        % 'client end'
        ?EV_CLIENT_END,

        % 'client cache begin'
        ?EV_CLIENT_CACHE_BEGIN,
        % 'client cache hit'
        ?EV_CLIENT_CACHE_HIT,
        % 'client cache miss'
        ?EV_CLIENT_CACHE_MISS,
        % 'client cache update'
        ?EV_CLIENT_CACHE_UPDATE,
        % 'client cache result'
        ?EV_CLIENT_CACHE_RESULT,
        % 'client cache end'
        ?EV_CLIENT_CACHE_END,

        % 'server receive'
        ?EV_SERVER_RECEIVE,
        % 'server send'
        ?EV_SERVER_SEND,

        % 'invoke service handler'
        ?EV_INVOKE_SERVICE_HANDLER,
        % 'service handler result'
        ?EV_SERVICE_HANDLER_RESULT,

        % 'internal error'
        ?EV_INTERNAL_ERROR,
        % 'trace event'
        ?EV_TRACE
    ].
