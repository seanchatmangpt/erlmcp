%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Health Dashboard and Transport Telemetry
%%%
%%% Tests HTTP endpoints, WebSocket streaming, OTEL integration,
%%% and dashboard functionality.
%%%
%%% Chicago School TDD: Real gen_server processes, no mocks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health_dashboard_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

dashboard_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Dashboard starts on default port", fun test_dashboard_start/0},
      {"Dashboard starts on custom port", fun test_dashboard_custom_port/0},
      {"Overall health endpoint returns JSON", fun test_health_endpoint/0},
      {"Transport health endpoint returns JSON", fun test_transport_health_endpoint/0},
      {"Prometheus metrics endpoint works", fun test_prometheus_metrics_endpoint/0},
      {"Dashboard HTML renders", fun test_dashboard_html/0}
     ]}.

telemetry_test_() ->
    {setup,
     fun setup_telemetry/0,
     fun cleanup_telemetry/1,
     [
      {"Telemetry initializes successfully", fun test_telemetry_init/0},
      {"Transport metrics are recorded", fun test_record_transport_metrics/0},
      {"Health checks are recorded", fun test_record_health_check/0},
      {"Connection events are recorded", fun test_record_connection_event/0},
      {"Errors are recorded", fun test_record_error/0},
      {"Latency is recorded", fun test_record_latency/0},
      {"Uptime is recorded", fun test_record_uptime/0},
      {"Throughput is recorded", fun test_record_throughput/0},
      {"Error rate is recorded", fun test_record_error_rate/0}
     ]}.

%%%===================================================================
%%% Setup & Teardown
%%%===================================================================

setup() ->
    % Start required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(erlmcp_observability),

    % Start health monitor
    {ok, HealthPid} = erlmcp_transport_health:start_link(#{}),
    HealthPid.

cleanup(_HealthPid) ->
    gen_server:stop(erlmcp_transport_health),
    ok.

setup_telemetry() ->
    % Start required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(opentelemetry),
    application:ensure_all_started(erlmcp_observability),
    ok.

cleanup_telemetry(_Arg) ->
    ok.

%%%===================================================================
%%% Dashboard Tests
%%%===================================================================

test_dashboard_start() ->
    % Start dashboard on default port
    {ok, Pid} = erlmcp_health_dashboard:start_link(),
    ?assert(is_pid(Pid)),

    % Verify it's running
    {ok, Port} = erlmcp_health_dashboard:get_port(),
    ?assert(is_integer(Port)),

    % Cleanup
    erlmcp_health_dashboard:stop().

test_dashboard_custom_port() ->
    % Start dashboard on custom port
    CustomPort = 9092,
    {ok, Pid} = erlmcp_health_dashboard:start_link(CustomPort),
    ?assert(is_pid(Pid)),

    % Verify port
    {ok, Port} = erlmcp_health_dashboard:get_port(),
    ?assertEqual(CustomPort, Port),

    % Cleanup
    erlmcp_health_dashboard:stop().

test_health_endpoint() ->
    % Start dashboard
    {ok, _Pid} = erlmcp_health_dashboard:start_link(),
    {ok, Port} = erlmcp_health_dashboard:get_port(),

    % Make HTTP request to /health endpoint
    Url = lists:flatten(io_lib:format("http://localhost:~p/health", [Port])),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            % Verify JSON response
            Data = jsx:decode(Body, [return_maps]),
            ?assert(is_map(Data)),
            ?assert(maps:is_key(<<"overall_status">>, Data)),
            ?assert(maps:is_key(<<"total_transports">>, Data));
        {error, Reason} ->
            ?debugFmt("HTTP request failed: ~p~n", [Reason]),
            ?assert(false, "HTTP request failed")
    end,

    % Cleanup
    erlmcp_health_dashboard:stop().

test_transport_health_endpoint() ->
    % Start dashboard and register a transport
    {ok, _Pid} = erlmcp_health_dashboard:start_link(),
    {ok, Port} = erlmcp_health_dashboard:get_port(),

    % Register a test transport
    {ok, TestPid} = start_test_transport(test_http),
    ok = erlmcp_transport_health:register_transport(test_http, TestPid, #{}),

    % Make HTTP request to /health/test_http endpoint
    Url = lists:flatten(io_lib:format("http://localhost:~p/health/test_http", [Port])),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            % Verify JSON response
            Data = jsx:decode(Body, [return_maps]),
            ?assert(is_map(Data)),
            ?assert(maps:is_key(<<"transport_id">>, Data)),
            ?assertEqual(<<"test_http">>, maps:get(<<"transport_id">>, Data));
        {error, Reason} ->
            ?debugFmt("HTTP request failed: ~p~n", [Reason]),
            ?assert(false, "HTTP request failed")
    end,

    % Cleanup
    erlmcp_transport_health:unregister_transport(test_http),
    stop_test_transport(TestPid),
    erlmcp_health_dashboard:stop().

test_prometheus_metrics_endpoint() ->
    % Start dashboard
    {ok, _Pid} = erlmcp_health_dashboard:start_link(),
    {ok, Port} = erlmcp_health_dashboard:get_port(),

    % Make HTTP request to /health/metrics endpoint
    Url = lists:flatten(io_lib:format("http://localhost:~p/health/metrics", [Port])),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            % Verify text/plain response
            ContentType = proplists:get_value("content-type", Headers, ""),
            ?assert(string:str(ContentType, "text/plain") > 0),

            % Verify Prometheus format
            ?assert(is_list(Body)),
            ?assert(string:str(Body, "# HELP") > 0),
            ?assert(string:str(Body, "# TYPE") > 0);
        {error, Reason} ->
            ?debugFmt("HTTP request failed: ~p~n", [Reason]),
            ?assert(false, "HTTP request failed")
    end,

    % Cleanup
    erlmcp_health_dashboard:stop().

test_dashboard_html() ->
    % Start dashboard
    {ok, _Pid} = erlmcp_health_dashboard:start_link(),
    {ok, Port} = erlmcp_health_dashboard:get_port(),

    % Make HTTP request to /health/dashboard endpoint
    Url = lists:flatten(io_lib:format("http://localhost:~p/health/dashboard", [Port])),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            % Verify HTML response
            ContentType = proplists:get_value("content-type", Headers, ""),
            ?assert(string:str(ContentType, "text/html") > 0),

            % Verify HTML content
            ?assert(is_list(Body)),
            ?assert(string:str(Body, "<!DOCTYPE html>") > 0),
            ?assert(string:str(Body, "erlmcp Transport Health") > 0),
            ?assert(string:str(Body, "<script>") > 0);
        {error, Reason} ->
            ?debugFmt("HTTP request failed: ~p~n", [Reason]),
            ?assert(false, "HTTP request failed")
    end,

    % Cleanup
    erlmcp_health_dashboard:stop().

%%%===================================================================
%%% Telemetry Tests
%%%===================================================================

test_telemetry_init() ->
    % Initialize telemetry
    Result = erlmcp_transport_telemetry:init(),
    ?assertEqual(ok, Result).

test_record_transport_metrics() ->
    erlmcp_transport_telemetry:init(),

    % Record some metrics
    ok = erlmcp_transport_telemetry:record_transport_metrics(tcp, messages_sent, 100),
    ok = erlmcp_transport_telemetry:record_transport_metrics(http, bytes_received, 1024),

    % Verify no errors (actual OTEL recording is best-effort)
    ?assert(true).

test_record_health_check() ->
    erlmcp_transport_telemetry:init(),

    % Record health check
    ok = erlmcp_transport_telemetry:record_health_check(tcp, <<"tcp_1">>, healthy, 5.5),

    % Verify no errors
    ?assert(true).

test_record_connection_event() ->
    erlmcp_transport_telemetry:init(),

    % Record connection events
    ok = erlmcp_transport_telemetry:record_connection_event(stdio, <<"stdio_1">>, connected),
    ok = erlmcp_transport_telemetry:record_connection_event(tcp, <<"tcp_1">>, disconnected),

    % Verify no errors
    ?assert(true).

test_record_error() ->
    erlmcp_transport_telemetry:init(),

    % Record errors
    ok = erlmcp_transport_telemetry:record_error(http, <<"http_1">>, {connection_failed, econnrefused}),

    % Verify no errors
    ?assert(true).

test_record_latency() ->
    erlmcp_transport_telemetry:init(),

    % Record latency
    ok = erlmcp_transport_telemetry:record_latency(websocket, <<"ws_1">>, 12.5),

    % Verify no errors
    ?assert(true).

test_record_uptime() ->
    erlmcp_transport_telemetry:init(),

    % Record uptime
    ok = erlmcp_transport_telemetry:record_uptime(tcp, 3600),

    % Verify no errors
    ?assert(true).

test_record_throughput() ->
    erlmcp_transport_telemetry:init(),

    % Record throughput
    ok = erlmcp_transport_telemetry:record_throughput(stdio, <<"stdio_1">>, 1000),

    % Verify no errors
    ?assert(true).

test_record_error_rate() ->
    erlmcp_transport_telemetry:init(),

    % Record error rate
    ok = erlmcp_transport_telemetry:record_error_rate(http, <<"http_1">>, 5.5),

    % Verify no errors
    ?assert(true).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Start a simple test transport process
start_test_transport(Name) ->
    Pid = spawn(fun() ->
        erlang:register(Name, self()),
        test_transport_loop()
    end),
    {ok, Pid}.

%% @doc Stop a test transport process
stop_test_transport(Pid) when is_pid(Pid) ->
    exit(Pid, shutdown),
    timer:sleep(100).

%% @doc Test transport message loop
test_transport_loop() ->
    receive
        _Msg ->
            test_transport_loop()
    end.
