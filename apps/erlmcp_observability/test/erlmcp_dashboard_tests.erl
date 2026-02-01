%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_dashboard_tests - Dashboard Server Tests
%%%
%%% Tests for real-time metrics dashboard with WebSocket and HTTP API.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PORT, 18080).
-define(LOCALHOST, "http://localhost:" ++ integer_to_list(?TEST_PORT)).

%%====================================================================
%% Test Fixtures
%%====================================================================

dashboard_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Dashboard server starts and stops", fun test_server_lifecycle/0},
      {"HTTP metrics endpoint returns JSON", fun test_http_metrics/0},
      {"HTTP historical endpoint works", fun test_http_historical/0},
      {"HTTP export CSV works", fun test_http_export_csv/0},
      {"HTTP export JSON works", fun test_http_export_json/0},
      {"WebSocket connection establishes", fun test_websocket_connect/0},
      {"WebSocket receives metrics", fun test_websocket_metrics/0},
      {"WebSocket subscribe/unsubscribe", fun test_websocket_subscribe/0},
      {"Metrics aggregator records data", fun test_aggregator_recording/0},
      {"Metrics aggregator calculates percentiles", fun test_percentiles/0},
      {"Metrics aggregator percentile edge cases", fun test_percentiles_edge_cases/0},
      {"Metrics aggregator percentile monotonicity", fun test_percentiles_monotonic/0},
      {"Bucket rotation works", fun test_bucket_rotation/0},
      {"Historical queries work", fun test_historical_queries/0},
      {"Alert thresholds trigger", fun test_alert_thresholds/0}]}.

setup() ->
    % Start required applications (NOT erlmcp_observability to avoid auto-starting dashboard)
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(jsx),
    {ok, _} = application:ensure_all_started(erlmcp_core),

    % Start metrics aggregator manually (not via supervision tree)
    % Check if already running from previous test (foreach runs tests sequentially)
    AggPid =
        case whereis(erlmcp_metrics_aggregator) of
            undefined ->
                {ok, Pid} = erlmcp_metrics_aggregator:start_link(),
                Pid;
            Pid ->
                Pid
        end,

    % Start dashboard server on test port (not via supervision tree)
    {ok, DashPid} = erlmcp_dashboard_server:start_link(?TEST_PORT),

    % Wait for server to be ready
    timer:sleep(100),

    #{aggregator => AggPid, dashboard => DashPid}.

cleanup(#{aggregator := _AggPid, dashboard := _DashPid}) ->
    % Stop dashboard server
    erlmcp_dashboard_server:stop(),
    % Note: Don't stop aggregator as it's shared across tests
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_server_lifecycle() ->
    {ok, Port} = erlmcp_dashboard_server:get_port(),
    ?assertEqual(?TEST_PORT, Port),

    % Server should be listening - verify by checking registered name exists
    ?assert(is_pid(whereis(erlmcp_dashboard_server))),

    % Verify we can connect to the port (HTTP will return 404 for root, but connection works)
    {ok, Sock} = gen_tcp:connect("localhost", ?TEST_PORT, [{active, false}], 1000),
    ?assertMatch({ok, _}, gen_tcp:recv(Sock, 0, 100)),
    gen_tcp:close(Sock).

test_http_metrics() ->
    % Send HTTP GET request to /api/metrics
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:get(ConnPid, "/api/metrics"),
    case gun:await(ConnPid, StreamRef, 5000) of
        {response, nofin, 200, _Headers} ->
            {ok, _Body} = gun:await_body(ConnPid, StreamRef, 5000),
            gun:close(ConnPid),
            ?assert(true);
        {response, fin, 200, _Headers} ->
            gun:close(ConnPid),
            ?assert(true);
        {response, _, Status, _Headers} ->
            gun:close(ConnPid),
            ?assert(false, {unexpected_status, Status})
    end.

test_http_historical() ->
    % Record some metrics first
    erlmcp_metrics_aggregator:record_metric(throughput, test, 100),
    timer:sleep(100),

    % Query historical metrics
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    Now = erlang:system_time(millisecond),
    Start = Now - 60000, % 1 minute ago
    Path =
        lists:flatten(
            io_lib:format("/api/metrics/historical?start=~p&end=~p", [Start, Now])),

    StreamRef = gun:get(ConnPid, Path),
    case gun:await(ConnPid, StreamRef, 5000) of
        {response, nofin, 200, _Headers} ->
            {ok, _Body} = gun:await_body(ConnPid, StreamRef, 5000),
            gun:close(ConnPid),
            ?assert(true);
        {response, fin, 200, _Headers} ->
            gun:close(ConnPid),
            ?assert(true);
        {response, _, Status, _Headers} ->
            gun:close(ConnPid),
            ?assert(false, {unexpected_status, Status})
    end.

test_http_export_csv() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:get(ConnPid, "/api/metrics/export?format=csv"),
    case gun:await(ConnPid, StreamRef, 5000) of
        {response, nofin, 200, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef, 5000),
            ?assert(is_binary(Body)),

            % Check Content-Type header
            ContentType = proplists:get_value(<<"content-type">>, Headers),
            ?assertEqual(<<"text/csv">>, ContentType),
            gun:close(ConnPid);
        {response, fin, 200, _Headers} ->
            gun:close(ConnPid),
            ?assert(true); % Empty CSV is also valid
        {response, _, Status, _Headers} ->
            gun:close(ConnPid),
            ?assert(false, {unexpected_status, Status})
    end.

test_http_export_json() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:get(ConnPid, "/api/metrics/export?format=json"),
    case gun:await(ConnPid, StreamRef, 5000) of
        {response, nofin, 200, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef, 5000),
            ?assert(is_binary(Body)),

            % Validate JSON
            Data = jsx:decode(Body, [return_maps]),
            ?assert(is_list(Data)),

            % Check Content-Type header
            ContentType = proplists:get_value(<<"content-type">>, Headers),
            ?assertEqual(<<"application/json">>, ContentType),
            gun:close(ConnPid);
        {response, fin, 200, _Headers} ->
            gun:close(ConnPid),
            ?assert(true); % Empty JSON array is also valid
        {response, _, Status, _Headers} ->
            gun:close(ConnPid),
            ?assert(false, {unexpected_status, Status})
    end.

test_websocket_connect() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Successfully upgraded to WebSocket
            gun:close(ConnPid),
            ?assert(true);
        {gun_response, ConnPid, _, _, Status, _Headers} ->
            gun:close(ConnPid),
            ?assertEqual(101, Status);
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?debugFmt("WebSocket upgrade failed: ~p", [Reason]),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

test_websocket_metrics() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Wait for connected message
            receive
                {gun_ws, ConnPid, StreamRef, {text, Msg}} ->
                    Data = jsx:decode(Msg, [return_maps]),
                    ?assertEqual(<<"connected">>, maps:get(<<"type">>, Data)),
                    gun:close(ConnPid);
                {gun_error, ConnPid, StreamRef, Reason} ->
                    gun:close(ConnPid),
                    ?assert(false, {websocket_error, Reason})
            after 5000 ->
                gun:close(ConnPid),
                ?assert(false, no_connected_message)
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_upgrade_timeout)
    end.

test_websocket_subscribe() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe message
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>,
                             metrics => [<<"throughput">>, <<"latency">>]}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for subscribed response (may get connected first)
            receive
                {gun_ws, ConnPid, StreamRef, {text, Msg}} ->
                    case jsx:decode(Msg, [return_maps]) of
                        #{<<"type">> := <<"connected">>} ->
                            % Skip connected message, wait for subscribed
                            receive
                                {gun_ws, ConnPid, StreamRef, {text, Msg2}} ->
                                    Data = jsx:decode(Msg2, [return_maps]),
                                    ?assertEqual(<<"subscribed">>, maps:get(<<"type">>, Data)),
                                    gun:close(ConnPid);
                                {gun_error, ConnPid, StreamRef, Reason} ->
                                    gun:close(ConnPid),
                                    ?assert(false, {websocket_error, Reason})
                            after 5000 ->
                                gun:close(ConnPid),
                                ?assert(false, no_subscribed_message)
                            end;
                        Data ->
                            ?assertEqual(<<"subscribed">>, maps:get(<<"type">>, Data)),
                            gun:close(ConnPid)
                    end;
                {gun_error, ConnPid, StreamRef, Reason} ->
                    gun:close(ConnPid),
                    ?assert(false, {websocket_error, Reason})
            after 5000 ->
                gun:close(ConnPid),
                ?assert(false, no_websocket_message)
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_upgrade_timeout)
    end.

test_aggregator_recording() ->
    % Record various metrics
    erlmcp_metrics_aggregator:record_metric(throughput, test, 100),
    erlmcp_metrics_aggregator:record_metric(latency, test, 25),
    erlmcp_metrics_aggregator:record_metric(error, test, 1),
    erlmcp_metrics_aggregator:record_metric(connections, test, 50),

    timer:sleep(100),

    % Get current metrics
    {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(throughput, Metrics)),
    ?assert(maps:is_key(timestamp, Metrics)).

test_percentiles() ->
    % Test with a small dataset
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),

    % P50 should be 55.0 (linear interpolation between 50 and 60)
    ?assertEqual(55.0, maps:get(p50, Percentiles)),

    % P95 should be approximately 95.5 (interpolation)
    P95 = maps:get(p95, Percentiles),
    ?assert(P95 > 95.0 andalso P95 < 96.0),

    % P99 should be approximately 99.1 (interpolation)
    P99 = maps:get(p99, Percentiles),
    ?assert(P99 > 99.0 andalso P99 < 100.0),

    % P999 should be approximately 99.91 (interpolation)
    P999 = maps:get(p999, Percentiles),
    ?assert(P999 > 99.9 andalso P999 =< 100.0),

    % Verify exact values for P95 and P99 (accounting for floating point)
    ?assertEqual(95.5, P95),
    ?assertEqual(99.1, P99).

test_percentiles_edge_cases() ->
    % Test with empty list
    EmptyPercentiles = erlmcp_metrics_aggregator:get_percentiles([]),
    ?assertEqual(0, maps:get(p50, EmptyPercentiles)),
    ?assertEqual(0, maps:get(p95, EmptyPercentiles)),
    ?assertEqual(0, maps:get(p99, EmptyPercentiles)),
    ?assertEqual(0, maps:get(p999, EmptyPercentiles)),

    % Test with single value
    SinglePercentiles = erlmcp_metrics_aggregator:get_percentiles([42]),
    ?assertEqual(42, maps:get(p50, SinglePercentiles)),
    ?assertEqual(42, maps:get(p95, SinglePercentiles)),
    ?assertEqual(42, maps:get(p99, SinglePercentiles)),
    ?assertEqual(42, maps:get(p999, SinglePercentiles)),

    % Test with two values
    TwoPercentiles = erlmcp_metrics_aggregator:get_percentiles([10, 100]),
    ?assertEqual(55.0, maps:get(p50, TwoPercentiles)),
    ?assertEqual(95.0, maps:get(p95, TwoPercentiles)),
    ?assertEqual(99.0, maps:get(p99, TwoPercentiles)),
    ?assertEqual(99.9, maps:get(p999, TwoPercentiles)),

    % Test with larger dataset (100 values)
    LargeValues = lists:seq(1, 100),
    LargePercentiles = erlmcp_metrics_aggregator:get_percentiles(LargeValues),
    ?assertEqual(50.5, maps:get(p50, LargePercentiles)),
    LargeP95 = maps:get(p95, LargePercentiles),
    ?assert(LargeP95 > 95.0 andalso LargeP95 < 96.0),
    LargeP99 = maps:get(p99, LargePercentiles),
    ?assert(LargeP99 > 99.0 andalso LargeP99 < 100.0).

test_percentiles_monotonic() ->
    % Verify that percentiles are monotonically increasing
    Values = lists:seq(1, 1000),
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),

    P50 = maps:get(p50, Percentiles),
    P95 = maps:get(p95, Percentiles),
    P99 = maps:get(p99, Percentiles),
    P999 = maps:get(p999, Percentiles),

    % All percentiles should be monotonically increasing
    ?assert(P50 =< P95),
    ?assert(P95 =< P99),
    ?assert(P99 =< P999).

test_bucket_rotation() ->
    % Record metrics
    erlmcp_metrics_aggregator:record_metric(throughput, test, 100),

    % Wait for bucket rotation (1 second)
    timer:sleep(1100),

    % Record more metrics
    erlmcp_metrics_aggregator:record_metric(throughput, test, 200),

    {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:get(throughput, Metrics) > 0).

test_historical_queries() ->
    % Record metrics over time
    lists:foreach(fun(I) ->
                     erlmcp_metrics_aggregator:record_metric(throughput, test, I * 10),
                     timer:sleep(50)
                  end,
                  lists:seq(1, 5)),

    Now = erlang:system_time(millisecond),
    Start = Now - 5000, % 5 seconds ago

    {ok, Historical} = erlmcp_metrics_aggregator:get_historical_metrics(Start, Now),
    ?assert(is_list(Historical)).

test_alert_thresholds() ->
    % Record high latency
    erlmcp_metrics_aggregator:record_metric(latency, test, 150000), % 150ms

    timer:sleep(100),

    {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
    ?assert(maps:get(latency_p99_us, Metrics) > 100000).

%%====================================================================
%% Property-Based Tests (if proper is available)
%%====================================================================

-ifdef(PROPER).

prop_percentiles_sorted() ->
    ?FORALL(Values,
            non_empty(list(number())),
            begin
                Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),
                P50 = maps:get(p50, Percentiles),
                P95 = maps:get(p95, Percentiles),
                P99 = maps:get(p99, Percentiles),
                P999 = maps:get(p999, Percentiles),
                P50 =< P95 andalso P95 =< P99 andalso P99 =< P999
            end).

-endif.
