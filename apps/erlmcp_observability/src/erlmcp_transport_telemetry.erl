%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Telemetry Integration with OpenTelemetry
%%%
%%% Provides comprehensive OTEL metrics and tracing for transport health.
%%% Includes uptime tracking, throughput metrics, error rates, and latency.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_telemetry).

-include("erlmcp.hrl").

%% API
-export([init/0, record_transport_metrics/3, record_health_check/4,
         record_connection_event/3, record_error/3, record_latency/3]).
%% Metric recording helpers
-export([record_uptime/2, record_throughput/3, record_error_rate/3]).

-define(TRANSPORT_UPTIME_METRIC, 'erlmcp.transport.uptime').
-define(TRANSPORT_THROUGHPUT_METRIC, 'erlmcp.transport.throughput').
-define(TRANSPORT_ERROR_RATE_METRIC, 'erlmcp.transport.error_rate').
-define(TRANSPORT_LATENCY_METRIC, 'erlmcp.transport.latency').
-define(TRANSPORT_CONNECTIONS_METRIC, 'erlmcp.transport.connections').

-type transport_type() :: stdio | tcp | http | websocket | sse.
-type metric_value() :: number().
-type health_status() :: healthy | unhealthy | degraded | unknown.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Initialize transport telemetry metrics
-spec init() -> ok | {error, term()}.
init() ->
    try
        % Register metrics with OpenTelemetry
        ok = register_metrics(),
        logger:info("Transport telemetry initialized successfully"),
        ok
    catch
        Class:Reason:Stacktrace ->
            logger:error("Failed to initialize transport telemetry: ~p:~p~n~p",
                        [Class, Reason, Stacktrace]),
            {error, {initialization_failed, {Class, Reason}}}
    end.

%% @doc Record comprehensive transport metrics
-spec record_transport_metrics(transport_type(), atom(), metric_value()) -> ok.
record_transport_metrics(TransportType, MetricName, Value) when is_atom(MetricName) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {metric_name, MetricName},
            {node, node()}
        ],
        record_metric(?TRANSPORT_THROUGHPUT_METRIC, Value, Attributes),
        ok
    catch
        Class:Reason:Stacktrace ->
            logger:error("Failed to record transport metric ~p:~p: ~p:~p",
                        [TransportType, MetricName, Class, Reason]),
            {error, {metric_recording_failed, {Class, Reason}}}
    end.

%% @doc Record health check result with OTEL
-spec record_health_check(transport_type(), binary(), health_status(), number()) -> ok.
record_health_check(TransportType, TransportId, Status, LatencyMs) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {transport_id, TransportId},
            {status, Status},
            {latency_ms, LatencyMs}
        ],
        % Record latency as histogram
        record_metric(?TRANSPORT_LATENCY_METRIC, LatencyMs, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            % Telemetry failures shouldn't break the application
            ok
    end.

%% @doc Record connection event (connect/disconnect)
-spec record_connection_event(transport_type(), binary(), connected | disconnected) -> ok.
record_connection_event(TransportType, TransportId, Event) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {transport_id, TransportId},
            {event, Event}
        ],
        % Increment connection counter
        record_metric(?TRANSPORT_CONNECTIONS_METRIC, 1, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            ok
    end.

%% @doc Record transport error
-spec record_error(transport_type(), binary(), term()) -> ok.
record_error(TransportType, TransportId, ErrorReason) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {transport_id, TransportId},
            {error_type, element(1, ErrorReason)}
        ],
        % Increment error counter
        record_metric(?TRANSPORT_ERROR_RATE_METRIC, 1, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            ok
    end.

%% @doc Record operation latency
-spec record_latency(transport_type(), binary(), number()) -> ok.
record_latency(TransportType, TransportId, LatencyMs) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {transport_id, TransportId}
        ],
        record_metric(?TRANSPORT_LATENCY_METRIC, LatencyMs, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            ok
    end.

%%%===================================================================
%%% Metric Recording Helpers
%%%===================================================================

%% @doc Record transport uptime in seconds
-spec record_uptime(transport_type(), number()) -> ok.
record_uptime(TransportType, UptimeSeconds) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {node, node()}
        ],
        record_metric(?TRANSPORT_UPTIME_METRIC, UptimeSeconds, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            ok
    end.

%% @doc Record throughput metrics (messages/second)
-spec record_throughput(transport_type(), binary(), number()) -> ok.
record_throughput(TransportType, TransportId, MessagesPerSecond) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {transport_id, TransportId}
        ],
        record_metric(?TRANSPORT_THROUGHPUT_METRIC, MessagesPerSecond, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            ok
    end.

%% @doc Record error rate as percentage
-spec record_error_rate(transport_type(), binary(), float()) -> ok.
record_error_rate(TransportType, TransportId, ErrorRate) ->
    try
        Attributes = [
            {transport_type, TransportType},
            {transport_id, TransportId}
        ],
        record_metric(?TRANSPORT_ERROR_RATE_METRIC, ErrorRate, Attributes),
        ok
    catch
        _Class:_Reason:_Stacktrace ->
            ok
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% Register metrics with OpenTelemetry
-spec register_metrics() -> ok.
register_metrics() ->
    % Check if opentelemetry_api is available
    case code:is_loaded(opentelemetry_api) of
        false ->
            logger:warning("OpenTelemetry API not available, metrics will be no-ops"),
            ok;
        _ ->
            try
                % Register counter for connections
                _ = otel_meter:create_counter(
                    ?TRANSPORT_CONNECTIONS_METRIC,
                    [{'description', <<"Number of active transport connections">>}]
                ),
                % Register histogram for latency
                _ = otel_meter:create_histogram(
                    ?TRANSPORT_LATENCY_METRIC,
                    [{'description', <<"Transport operation latency in milliseconds">>},
                     {'unit', 'ms'}]
                ),
                % Register gauge for error rate
                _ = otel_meter:create_gauge(
                    ?TRANSPORT_ERROR_RATE_METRIC,
                    [{'description', <<"Transport error rate percentage">>},
                     {'unit', '%'}]
                ),
                % Register counter for throughput
                _ = otel_meter:create_counter(
                    ?TRANSPORT_THROUGHPUT_METRIC,
                    [{'description', <<"Transport throughput in messages per second">>},
                     {'unit', 'msg/s'}]
                ),
                ok
            catch
                _:_ ->
                    % OTEL might not be fully configured, that's OK
                    logger:info("OpenTelemetry metrics registration skipped (not fully configured)"),
                    ok
            end
    end.

%% @private
%% Record a metric with OpenTelemetry (safe wrapper)
-spec record_metric(atom(), number(), proplist:proplist()) -> ok.
record_metric(MetricName, Value, Attributes) ->
    case code:is_loaded(opentelemetry_api) of
        false ->
            ok;
        _ ->
            try
                % Convert proplist to list of {Key, Value} tuples for OTEL
                OtelAttributes = convert_attributes(Attributes),
                % Record the metric based on type
                case MetricName of
                    ?TRANSPORT_CONNECTIONS_METRIC ->
                        otel_counter:add(MetricName, Value, OtelAttributes);
                    ?TRANSPORT_ERROR_RATE_METRIC ->
                        otel_gauge:record(MetricName, Value, OtelAttributes);
                    ?TRANSPORT_THROUGHPUT_METRIC ->
                        otel_counter:add(MetricName, Value, OtelAttributes);
                    ?TRANSPORT_LATENCY_METRIC ->
                        otel_histogram:record(MetricName, Value, OtelAttributes);
                    ?TRANSPORT_UPTIME_METRIC ->
                        otel_gauge:record(MetricName, Value, OtelAttributes);
                    _ ->
                        logger:warning("Unknown metric type: ~p", [MetricName])
                end,
                ok
            catch
                _:_ ->
                    % Silently fail on telemetry errors
                    ok
            end
    end.

%% @private
%% Convert attributes to OTEL format
-spec convert_attributes(proplist:proplist()) -> list({atom(), term()}).
convert_attributes(Attributes) ->
    lists:map(
        fun
            ({Key, Value}) when is_atom(Key) -> {Key, Value};
            ({Key, Value}) when is_binary(Key) -> {binary_to_atom(Key, utf8), Value};
            ({Key, Value}) when is_list(Key) -> {list_to_atom(Key), Value}
        end,
        Attributes
    ).
