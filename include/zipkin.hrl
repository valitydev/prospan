%% See https://zipkin.io/zipkin-api/zipkin2-api.yaml

%% A span is a single-host view of an operation. A trace is a series of spans
%% (often RPC calls) which nest to form a latency tree. Spans are in the same
%% trace when they share the same trace ID. The parent_id field establishes the
%% position of one span in the tree.
%%
%% The root span is where parent_id is Absent and usually has the longest
%% duration in the trace. However, nested asynchronous work can materialize as
%% child spans whose duration exceed the root span.
%%
%% Spans usually represent remote activity such as RPC calls, or messaging
%% producers and consumers. However, they can also represent in-process
%% activity in any position of the trace. For example, a root span could
%% represent a server receiving an initial client request. A root span could
%% also represent a scheduled job that has no remote context.

%% [a-f0-9]{16,32}
-type trace_id() :: binary().

%% [a-f0-9]{16}
-type span_id() :: binary().

%% This value should be set directly by instrumentation, using the most
%% precise value possible. For example, gettimeofday or multiplying epoch
%% millis by 1000.
-type span_start() :: integer().

%% Epoch **microseconds** of this event.
-type event_timestamp() :: integer().

-type span_duration() :: integer().

%% When present, kind clarifies timestamp, duration and remoteEndpoint. When
%% absent, the span is local or incomplete. Unlike client and server, there
%% is no direct critical path latency relationship between producer and
%% consumer spans.
%%
%% * `CLIENT`
%%   * timestamp is the moment a request was sent to the server. (in v1 "cs")
%%   * duration is the delay until a response or an error was received. (in v1 "cr"-"cs")
%%   * remoteEndpoint is the server. (in v1 "sa")
%% * `SERVER`
%%   * timestamp is the moment a client request was received. (in v1 "sr")
%%   * duration is the delay until a response was sent or an error. (in v1 "ss"-"sr")
%%   * remoteEndpoint is the client. (in v1 "ca")
%% * `PRODUCER`
%%   * timestamp is the moment a message was sent to a destination. (in v1  "ms")
%%   * duration is the delay sending the message, such as batching.
%%   * remoteEndpoint is the broker.
%% * `CONSUMER`
%%   * timestamp is the moment a message was received from an origin. (in v1 "mr")
%%   * duration is the delay consuming the message, such as from backlog.
%%   * remoteEndpoint - Represents the broker. Leave serviceName absent if unknown.
-type span_kind() :: 'CLIENT' | 'SERVER' | 'PRODUCER' | 'CONSUMER'.

-type service_name() :: binary().

%% The network context of a node in the service graph
-type span_endpooint() :: #{
    'serviceName' := undefined | service_name(),
    'ipv4' := undefined | binary(),
    'ipv6' := undefined | binary(),
    'port' := undefined | integer()
}.

%% Associates an event that explains latency with a timestamp.
%% Unlike log statements, annotations are often codes. Ex. "ws" for WireSend
%%
%% Zipkin v1 core annotations such as "cs" and "sr" have been replaced with
%% Span.Kind, which interprets timestamp and duration.
-type span_annotation() :: #{
    timestamp := event_timestamp(),
    %% Usually a short tag indicating an event, like "error"
    %%
    %% While possible to add larger data, such as garbage collection details, low
    %% cardinality event names both keep the size of spans down and also are easy
    %% to search against.
    value := binary()
}.

-type span_tags() :: #{binary() => binary()}.

-type span() :: #{
    'traceId' := trace_id(),
    'id' := undefined | span_id(),
    'parentId' := undefined | span_id(),
    'name' := undefined | binary(),
    'kind' := undefined | span_kind(),
    'timestamp' := undefined | span_start(),
    'duration' := undefined | span_duration(),
    'debug' := boolean(),
    'shared' := integer(),
    %% The host that recorded this span, primarily for query by service name.
    'localEndpoint' := undefined | span_endpooint(),
    %% When an RPC (or messaging) span, indicates the other side of the
    %% connection.
    'remoteEndpoint' := undefined | span_endpooint(),
    %% Associates events that explain latency with the time they happened.
    'annotations' := undefined | list(span_annotation()),
    'tags' := undefined | span_tags()
}.
