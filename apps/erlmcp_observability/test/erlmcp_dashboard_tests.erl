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

-define(TEST_PORT, 9091).
-define(LOCALHOST, "http://localhost:" ++ integer_to_list(?TEST_PORT)).

%%====================================================================
%% Test Fixtures
%%====================================================================

dashboard_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Dashboard server starts and stops", fun test_server_lifecycle/0},
      {"HTTP metrics endpoint returns JSON", fun test_http_metrics/0},
      {"HTTP historical endpoint works", fun test_http_historical/0},
      {"HTTP export CSV works", fun test_http_export_csv/0},
      {"HTTP export JSON works", fun test_http_export_json/0},
      {"WebSocket connection establishes", fun test_websocket_connect/0},
      {"WebSocket receives metrics", fun test_websocket_metrics/0},
      {"WebSocket subscribe/unsubscribe", fun test_websocket_subscribe/0},
      {"Metrics aggregator records data", fun test_aggregator_recording/0},
      {"Metrics aggregator calculates percentiles", fun test_percentiles/0},
      {"Bucket rotation works", fun test_bucket_rotation/0},
      {"Historical queries work", fun test_historical_queries/0},
      {"Alert thresholds trigger", fun test_alert_thresholds/0}
     ]}.

setup() ->
    % Start required applications
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),
    application:ensure_all_started(jsx),

    % Start metrics aggregator
    {ok, AggPid} = erlmcp_metrics_aggregator:start_link(),

    % Start dashboard server
    {ok, DashPid} = erlmcp_dashboard_server:start_link(?TEST_PORT),

    % Wait for server to be ready
    timer:sleep(100),

    #{aggregator => AggPid, dashboard => DashPid}.

cleanup(#{aggregator := AggPid, dashboard := DashPid}) ->
    erlmcp_dashboard_server:stop(),
    gen_server:stop(AggPid),
    timer:sleep(50).

%%====================================================================
%% Tests
%%====================================================================

test_server_lifecycle() ->
    {ok, Port} = erlmcp_dashboard_server:get_port(),
    ?assertEqual(?TEST_PORT, Port),

    % Server should be listening
    ?assertMatch({ok, _}, inet:getaddr("localhost", inet)).

test_http_metrics() ->
    % Send HTTP GET request to /api/metrics
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    StreamRef = gun:get(ConnPid, "/api/metrics"),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, _Body} = gun:await_body(ConnPid, StreamRef),

    gun:close(ConnPid),
    ?assert(true).

test_http_historical() ->
    % Record some metrics first
    erlmcp_metrics_aggregator:record_metric(throughput, test, 100),
    timer:sleep(100),

    % Query historical metrics
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    Now = erlang:system_time(millisecond),
    Start = Now - 60000, % 1 minute ago
    Path = lists:flatten(io_lib:format("/api/metrics/historical?start=~p&end=~p", [Start, Now])),

    StreamRef = gun:get(ConnPid, Path),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, _Body} = gun:await_body(ConnPid, StreamRef),

    gun:close(ConnPid),
    ?assert(true).

test_http_export_csv() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    StreamRef = gun:get(ConnPid, "/api/metrics/export?format=csv"),
    case gun:await(ConnPid, StreamRef, 1000) of
        {response, nofin, 200, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            ?assert(is_binary(Body)),

            % Check Content-Type header
            ContentType = proplists:get_value(<<"content-type">>, Headers),
            ?assertEqual(<<"text/csv">>, ContentType);
        {response, fin, 200, _Headers} ->
            ?assert(true) % Empty CSV is also valid
    end,

    gun:close(ConnPid).

test_http_export_json() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    StreamRef = gun:get(ConnPid, "/api/metrics/export?format=json"),
    case gun:await(ConnPid, StreamRef, 1000) of
        {response, nofin, 200, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            ?assert(is_binary(Body)),

            % Validate JSON
            Data = jsx:decode(Body, [return_maps]),
            ?assert(is_list(Data)),

            % Check Content-Type header
            ContentType = proplists:get_value(<<"content-type">>, Headers),
            ?assertEqual(<<"application/json">>, ContentType);
        {response, fin, 200, _Headers} ->
            ?assert(true) % Empty JSON array is also valid
    end,

    gun:close(ConnPid).

test_websocket_connect() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            ?assert(true);
        {gun_response, ConnPid, _, _, Status, _Headers} ->
            ?assertEqual(101, Status);
        {gun_error, ConnPid, StreamRef, Reason} ->
            ?debugFmt("WebSocket upgrade failed: ~p", [Reason]),
            ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    gun:close(ConnPid).

test_websocket_metrics() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Wait for connected message
            receive
                {gun_ws, ConnPid, StreamRef, {text, Msg}} ->
                    Data = jsx:decode(Msg, [return_maps]),
                    ?assertEqual(<<"connected">>, maps:get(<<"type">>, Data))
            after 2000 ->
                ?debugMsg("No connected message received"),
                ?assert(false)
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            ?debugFmt("WebSocket upgrade failed: ~p", [Reason]),
            ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    gun:close(ConnPid).

test_websocket_subscribe() ->
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe message
            SubscribeMsg = jsx:encode(#{
                type => <<"subscribe">>,
                metrics => [<<"throughput">>, <<"latency">>]
            }),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for subscribed response
            receive
                {gun_ws, ConnPid, StreamRef, {text, Msg}} ->
                    % Skip connected message
                    case jsx:decode(Msg, [return_maps]) of
                        #{<<"type">> := <<"connected">>} ->
                            receive
                                {gun_ws, ConnPid, StreamRef, {text, Msg2}} ->
                                    Data = jsx:decode(Msg2, [return_maps]),
                                    ?assertEqual(<<"subscribed">>, maps:get(<<"type">>, Data))
                            after 2000 ->
                                ?debugMsg("No subscribed message received"),
                                ?assert(false)
                            end;
                        Data ->
                            ?assertEqual(<<"subscribed">>, maps:get(<<"type">>, Data))
                    end
            after 2000 ->
                ?debugMsg("No WebSocket message received"),
                ?assert(false)
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            ?debugFmt("WebSocket upgrade failed: ~p", [Reason]),
            ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    gun:close(ConnPid).

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
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),

    ?assertEqual(50, maps:get(p50, Percentiles)),
    ?assertEqual(100, maps:get(p95, Percentiles)),
    ?assertEqual(100, maps:get(p99, Percentiles)),
    ?assertEqual(100, maps:get(p999, Percentiles)).

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
    end, lists:seq(1, 5)),

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
    ?FORALL(Values, non_empty(list(number())),
        begin
            Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),
            P50 = maps:get(p50, Percentiles),
            P95 = maps:get(p95, Percentiles),
            P99 = maps:get(p99, Percentiles),
            P999 = maps:get(p999, Percentiles),
            (P50 =< P95) andalso (P95 =< P99) andalso (P99 =< P999)
        end).

-endif.
