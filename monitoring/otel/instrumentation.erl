%% @doc Enterprise OpenTelemetry Instrumentation for erlmcp v3
%% This module provides comprehensive distributed tracing and metrics collection
%% for erlmcp v3 components.
-module(erlmcp_otel_instrumentation).

-behaviour(gen_server).

%% API
-export([start_link/0, span/2, span/3, span/4, measure/2, measure/3,
         trace_request/2, trace_session/2, trace_registry/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the OpenTelemetry instrumentation server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a span with automatic resource attributes
-spec span(Name :: binary(), Kind :: span_kind()) -> opentelemetry:span_ctx().
span(Name, Kind) ->
    span(Name, Kind, #{}, []).

%% @doc Create a span with attributes
-spec span(Name :: binary(), Kind :: span_kind(), Attributes :: map()) -> opentelemetry:span_ctx().
span(Name, Kind, Attributes) ->
    span(Name, Kind, Attributes, []).

%% @doc Create a span with attributes and links
-spec span(Name :: binary(), Kind :: span_kind(), Attributes :: map(), Links :: [opentelemetry:link()]) -> opentelemetry:span_ctx().
span(Name, Kind, Attributes, Links) ->
    %% Add erlmcp-specific resource attributes
    ResourceAttributes = #{
        <<"erlmcp.version">> => <<"3.0.0">>,
        <<"erlmcp.component">> => <<"core">>,
        <<"erlmcp.cluster">> => erlmcp_config:get(cluster, <<"unknown">>),
        <<"erlmcp.environment">> => erlmcp_config:get(environment, <<"unknown">>),
        <<"erlmcp.region">> => erlmcp_config:get(region, <<"unknown">>)
    },

    %% Merge with provided attributes
    FinalAttributes = maps:merge(ResourceAttributes, Attributes),

    %% Create span
    opentelemetry:start_span(Name, Kind, FinalAttributes, Links).

%% @doc Measure execution time of a function
-spec measure(Name :: binary(), Fun :: fun(() -> T)) -> T.
measure(Name, Fun) ->
    measure(Name, Fun, #{}).

%% @doc Measure execution time of a function with attributes
-spec measure(Name :: binary(), Fun :: fun(() -> T), Attributes :: map()) -> T.
measure(Name, Fun, Attributes) ->
    SpanCtx = span(Name, internal, Attributes),
    try
        Result = Fun(),
        _ = opentelemetry:end_span(SpanCtx),
        Result
    catch
        Class:Reason:Stacktrace ->
            %% Record exception
            opentelemetry:record_exception(SpanCtx, Reason, Stacktrace),
            opentelemetry:end_span(SpanCtx),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Trace a request with automatic span creation
-spec trace_request(Request :: map(), Fun :: fun(() -> T)) -> T.
trace_request(Request, Fun) ->
    RequestId = maps:get(request_id, Request, generate_request_id()),
    StartTime = erlang:monotonic_time(millisecond),

    %% Create span for the request
    SpanCtx = span(
        <<"erlmcp.request">>,
        server,
        #{
            <<"request.id">> => RequestId,
            <<"request.method">> => maps:get(method, Request, unknown),
            <<"request.endpoint">> => maps:get(endpoint, Request, unknown),
            <<"request.user_id">> => maps:get(user_id, Request, undefined),
            <<"request.client_ip">> => maps:get(client_ip, Request, unknown),
            <<"erlmcp.component">> => <<"request_handler">>
        }
    ),

    try
        Result = Fun(),

        %% Record request metrics
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Record metrics
        record_request_metrics(RequestId, Duration, Success = true),

        %% Record span attributes
        opentelemetry:record_attributes(SpanCtx, #{
            <<"request.duration_ms">> => Duration,
            <<"request.success">> => true,
            <<"response.status">> => success
        }),

        opentelemetry:end_span(SpanCtx),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,

            %% Record request metrics for error
            record_request_metrics(RequestId, Duration, Success = false),

            %% Record exception
            opentelemetry:record_exception(SpanCtx, Reason, Stacktrace),

            %% Record span attributes
            opentelemetry:record_attributes(SpanCtx, #{
                <<"request.duration_ms">> => Duration,
                <<"request.success">> => false,
                <<"response.status">> => error,
                <<"error.type">> => atom_to_binary(Class, utf8),
                <<"error.message">> => list_to_binary(io_lib:format("~p", [Reason]))
            }),

            opentelemetry:end_span(SpanCtx),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Trace session lifecycle
-spec trace_session(SessionId :: binary(), Fun :: fun(() -> T)) -> T.
trace_session(SessionId, Fun) ->
    StartTime = erlang:monotonic_time(millisecond),

    %% Create span for session operation
    SpanCtx = span(
        <<"erlmcp.session">>,
        internal,
        #{
            <<"session.id">> => SessionId,
            <<"session.start_time">> => StartTime,
            <<"erlmcp.component">> => <<"session_manager">>
        }
    ),

    try
        Result = Fun(),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Record session metrics
        record_session_metrics(SessionId, Duration),

        %% Record span attributes
        opentelemetry:record_attributes(SpanCtx, #{
            <<"session.duration_ms">> => Duration,
            <<"session.status">> => success
        }),

        opentelemetry:end_span(SpanCtx),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,

            %% Record exception
            opentelemetry:record_exception(SpanCtx, Reason, Stacktrace),

            %% Record span attributes
            opentelemetry:record_attributes(SpanCtx, #{
                <<"session.duration_ms">> => Duration,
                <<"session.status">> => error,
                <<"error.type">> => atom_to_binary(Class, utf8),
                <<"error.message">> => list_to_binary(io_lib:format("~p", [Reason]))
            }),

            opentelemetry:end_span(SpanCtx),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Trace registry operations
-spec trace_registry(Operation :: binary(), ResourceId :: binary()) -> opentelemetry:span_ctx().
trace_registry(Operation, ResourceId) ->
    SpanCtx = span(
        <<"erlmcp.registry">>,
        internal,
        #{
            <<"registry.operation">> => Operation,
            <<"registry.resource_id">> => ResourceId,
            <<"erlmcp.component">> => <<"registry">>
        }
    ),

    %% Record registry metrics
    record_registry_metrics(Operation),

    SpanCtx.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize OpenTelemetry
    erlmcp_otel_utils:init(),

    %% Register metrics
    register_metrics(),

    %% Start metric collection
    erlang:send_after(1000, self(), collect_metrics),

    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect system metrics
    collect_system_metrics(),

    %% Schedule next collection
    erlang:send_after(1000, self(), collect_metrics),

    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate request ID
generate_request_id() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

%% @private Record request metrics
record_request_metrics(RequestId, Duration, Success) ->
    %% Record Prometheus-style metrics
    erlmcp_metrics:inc(erlmcp_requests_total, 1, #{
        <<"status">> => case Success of true -> <<"success">>; false -> <<"error">> end
    }),

    erlmcp_metrics:observe(erlmcp_request_duration_seconds, Duration / 1000, #{}),

    %% Record OTEL metric
    erlmcp_otel_metrics:record_counter(
        <<"erlmcp.requests">>,
        1,
        #{
            <<"request.id">> => RequestId,
            <<"request.success">> => Success
        }
    ).

%% @private Record session metrics
record_session_metrics(SessionId, Duration) ->
    erlmcp_metrics:observe(erlmcp_session_duration_seconds, Duration / 1000, #{}),

    erlmcp_metrics:inc(erlmcp_sessions_created_total, 1, #{}).

%% @private Record registry metrics
record_registry_metrics(Operation) ->
    erlmcp_metrics:inc(erlmcp_registry_operations_total, 1, #{
        <<"operation">> => Operation
    }).

%% @private Register metrics
register_metrics() ->
    %% Request metrics
    erlmcp_metrics:counter(erlmcp_requests_total, #{
        description => "Total number of requests",
        unit => "{request}"
    }),

    erlmcp_metrics:histogram(erlmcp_request_duration_seconds, #{
        description => "Request duration in seconds",
        unit => "s",
        buckets => [0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1, 2.5, 5, 7.5, 10]
    }),

    %% Session metrics
    erlmcp_metrics:histogram(erlmcp_session_duration_seconds, #{
        description => "Session duration in seconds",
        unit => "s",
        buckets => [1, 5, 10, 30, 60, 300, 600, 1800]
    }),

    erlmcp_metrics:counter(erlmcp_sessions_created_total, #{
        description => "Total number of sessions created",
        unit => "{session}"
    }),

    %% Registry metrics
    erlmcp_metrics:counter(erlmcp_registry_operations_total, #{
        description => "Total number of registry operations",
        unit => "{operation}",
        labels => [<<"operation">>]
    }).

%% @private Collect system metrics
collect_system_metrics() ->
    %% CPU usage
    CpuUsage = cpu_sup:util([time, busy, idle]),
    erlmcp_metrics:gauge(erlmcp_system_cpu_usage, CpuUsage, #{}),

    %% Memory usage
    {memory, MemoryInfo} = erlang:memory(),
    erlmcp_metrics:gauge(erlmcp_system_memory_bytes, MemoryInfo, #{}),

    %% Process count
    ProcessCount = erlang:system_info(process_count),
    erlmcp_metrics:gauge(erlmcp_system_process_count, ProcessCount, #{}).

%%====================================================================
%% Module Info
%%====================================================================

-define(METRICS_MODULE, erlmcp_metrics).
-define(OTEL_METRICS_MODULE, erlmcp_otel_metrics).