-include_lib("woody/src/woody_defs.hrl").

-define(WOODY_CLIENT_EVENT(From, Service, Type, EventType, Function), #{
    <<"service">> := From,
    <<"rpc.client">> := #{
        <<"type">> := Type,
        <<"event">> := EventType,
        <<"function">> := Function,
        <<"service">> := Service
    }
}).

-define(WOODY_CLIENT_CALL(From, Service, Function),
    ?WOODY_CLIENT_EVENT(From, Service, <<"call">>, ?EV_CALL_SERVICE, Function)
).

-define(WOODY_CLIENT_RESULT(From, Service, Function),
    ?WOODY_CLIENT_EVENT(From, Service, <<"call">>, ?EV_SERVICE_RESULT, Function)
).

%%

-define(SWAGGER_ENTRY_CALL(Service, Operation, Parameters), #{
    <<"service">> := Service,
    <<"span_id">> := SpanID,
    <<"swagger">> := #{
        <<"request_id">> := SpanID,
        <<"operation_id">> := Operation,
        <<"parameters">> := Parameters
    }
}).

%%

-define(WOODY_SERVER_EVENT_SCOPED(Scopes, At, Service, Type, EventType, Function, Url), #{
    <<"scoper">> := Scopes,
    <<"service">> := At,
    <<"rpc.server">> := #{
        <<"type">> := Type,
        <<"event">> := EventType,
        <<"function">> := Function,
        <<"service">> := Service,
        <<"url">> := Url
    }
}).

-define(WOODY_SERVER_MACHINE_INVOKED(At),
    ?WOODY_SERVER_EVENT_SCOPED(
        [<<"machine">>, <<"rpc.server">>], At, _Service, <<"call">>, ?EV_INVOKE_SERVICE_HANDLER, _Function, _Url
    )
).

-define(WOODY_SERVER_EVENT(At, Service, Type, EventType, Function, Url),
    %% Lone scope "rpc.server" says it is first event upon request landing
    ?WOODY_SERVER_EVENT_SCOPED([<<"rpc.server">>], At, Service, Type, EventType, Function, Url)
).

-define(WOODY_SERVER_INVOKE(At, Service, Function, Url),
    ?WOODY_SERVER_EVENT(At, Service, <<"call">>, ?EV_INVOKE_SERVICE_HANDLER, Function, Url)
).

-define(WOODY_SERVER_RESULT(At, Service, Function, Url),
    ?WOODY_SERVER_EVENT(At, Service, <<"call">>, ?EV_SERVICE_HANDLER_RESULT, Function, Url)
).
