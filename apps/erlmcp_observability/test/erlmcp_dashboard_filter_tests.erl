%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_dashboard_filter_tests - WebSocket Metrics Subscription Filter Tests
%%%
%%% Tests for filtering metrics based on subscription preferences.
%%% Implements Joe Armstrong's principle: "Send only what's needed".
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_filter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PORT, 18081).
-define(LOCALHOST, "http://localhost:" ++ integer_to_list(?TEST_PORT)).

%%====================================================================
%% Test Fixtures
%%====================================================================

dashboard_filter_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Filter by CPU metrics only", fun test_filter_cpu_only/0},
      {"Filter by memory metrics only", fun test_filter_memory_only/0},
      {"Filter by multiple metric types", fun test_filter_multiple_types/0},
      {"Filter reduces bandwidth", fun test_filter_reduces_bandwidth/0},
      {"No filter sends all metrics", fun test_no_filter_sends_all/0},
      {"Empty filter sends all metrics", fun test_empty_filter_sends_all/0},
      {"Unsubscribe clears filter", fun test_unsubscribe_clears_filter/0},
      {"Invalid filter types are ignored", fun test_invalid_filter_ignored/0},
      {"Filter applies to structured metrics", fun test_filter_structured_metrics/0},
      {"Subscribe with filter works", fun test_subscribe_with_filter/0}]}.

setup() ->
    % Start required applications
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(jsx),
    {ok, _} = application:ensure_all_started(erlmcp_core),

    % Start metrics aggregator
    AggPid =
        case whereis(erlmcp_metrics_aggregator) of
            undefined ->
                {ok, Pid} = erlmcp_metrics_aggregator:start_link(),
                Pid;
            Pid ->
                Pid
        end,

    % Start dashboard server on test port
    {ok, DashPid} = erlmcp_dashboard_server:start_link(?TEST_PORT),

    % Wait for server to be ready
    timer:sleep(100),

    #{aggregator => AggPid, dashboard => DashPid}.

cleanup(#{aggregator := _AggPid, dashboard := _DashPid}) ->
    erlmcp_dashboard_server:stop(),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% @doc Test filtering by CPU metrics only
test_filter_cpu_only() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(cpu, test, 45.5),
    erlmcp_metrics_aggregator:record_metric(memory, test, 512.0),
    erlmcp_metrics_aggregator:record_metric(throughput, test, 1000),

    % Connect WebSocket and subscribe with CPU filter
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with filter
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>, filter => #{types => [<<"cpu_percent">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for metrics message (may receive connected first)
            Metrics = receive_metrics(ConnPid, StreamRef, 2),

            gun:close(ConnPid),

            % Verify filtered metrics
            Data = maps:get(<<"data">>, Metrics),
            ?assert(maps:is_key(<<"cpu_percent">>, Data)),
            ?assertNot(maps:is_key(<<"memory_mb">>, Data)),
            ?assertNot(maps:is_key(<<"throughput">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test filtering by memory metrics only
test_filter_memory_only() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(memory, test, 1024.0),
    erlmcp_metrics_aggregator:record_metric(throughput, test, 500),

    % Connect WebSocket and subscribe with memory filter
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with filter
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>, filter => #{types => [<<"memory_mb">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for metrics message
            Metrics = receive_metrics(ConnPid, StreamRef, 2),

            gun:close(ConnPid),

            % Verify filtered metrics
            Data = maps:get(<<"data">>, Metrics),
            ?assert(maps:is_key(<<"memory_mb">>, Data)),
            ?assertNot(maps:is_key(<<"cpu_percent">>, Data)),
            ?assertNot(maps:is_key(<<"throughput">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test filtering by multiple metric types
test_filter_multiple_types() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(cpu, test, 75.0),
    erlmcp_metrics_aggregator:record_metric(memory, test, 2048.0),
    erlmcp_metrics_aggregator:record_metric(throughput, test, 2000),
    erlmcp_metrics_aggregator:record_metric(connections, test, 100),

    % Connect WebSocket and subscribe with multiple types filter
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with filter for CPU and memory only
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>,
                             filter => #{types => [<<"cpu_percent">>, <<"memory_mb">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for metrics message
            Metrics = receive_metrics(ConnPid, StreamRef, 2),

            gun:close(ConnPid),

            % Verify filtered metrics
            Data = maps:get(<<"data">>, Metrics),
            ?assert(maps:is_key(<<"cpu_percent">>, Data)),
            ?assert(maps:is_key(<<"memory_mb">>, Data)),
            ?assertNot(maps:is_key(<<"throughput">>, Data)),
            ?assertNot(maps:is_key(<<"connections">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test that filtering reduces bandwidth (Joe Armstrong principle)
test_filter_reduces_bandwidth() ->
    % Record comprehensive metrics
    erlmcp_metrics_aggregator:record_metric(cpu, test, 50.0),
    erlmcp_metrics_aggregator:record_metric(memory, test, 1024.0),
    erlmcp_metrics_aggregator:record_metric(throughput, test, 1500),
    erlmcp_metrics_aggregator:record_metric(latency, test, 25000),
    erlmcp_metrics_aggregator:record_metric(connections, test, 50),

    % Connect WebSocket without filter
    {ok, ConnPid1} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid1, 5000),

    StreamRef1 = gun:ws_upgrade(ConnPid1, "/ws"),
    receive
        {gun_upgrade, ConnPid1, StreamRef1, [<<"websocket">>], _Headers} ->
            ok
    after 5000 ->
        gun:close(ConnPid1),
        ?assert(false, websocket_timeout)
    end,

    % Wait for unfiltered metrics
    UnfilteredMsg = receive_metrics(ConnPid1, StreamRef1, 1),
    UnfilteredSize = byte_size(UnfilteredMsg),
    gun:close(ConnPid1),

    % Connect WebSocket with CPU filter
    {ok, ConnPid2} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid2, 5000),

    StreamRef2 = gun:ws_upgrade(ConnPid2, "/ws"),
    receive
        {gun_upgrade, ConnPid2, StreamRef2, [<<"websocket">>], _Headers2} ->
            % Send subscribe with CPU filter
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>, filter => #{types => [<<"cpu_percent">>]}}),
            gun:ws_send(ConnPid2, StreamRef2, {text, SubscribeMsg})
    after 5000 ->
        gun:close(ConnPid2),
        ?assert(false, websocket_timeout)
    end,

    % Wait for filtered metrics
    FilteredMsg = receive_metrics(ConnPid2, StreamRef2, 2),
    FilteredSize = byte_size(FilteredMsg),
    gun:close(ConnPid2),

    % Verify filtered message is smaller (bandwidth reduction)
    ?assert(FilteredSize < UnfilteredSize),

    % Verify we saved at least 20% bandwidth
    SavingsPercent = (UnfilteredSize - FilteredSize) * 100 / UnfilteredSize,
    ?assert(SavingsPercent > 20.0).

%% @doc Test that no filter sends all metrics
test_no_filter_sends_all() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(cpu, test, 55.0),
    erlmcp_metrics_aggregator:record_metric(memory, test, 768.0),

    % Connect WebSocket without subscribing (default: all)
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Just wait for metrics, no subscription needed
            Metrics = receive_metrics(ConnPid, StreamRef, 1),

            gun:close(ConnPid),

            % Verify all metrics are present
            Data = maps:get(<<"data">>, Metrics),
            ?assert(maps:is_key(<<"timestamp">>, Data)),
            ?assert(maps:is_key(<<"cpu_percent">>, Data) orelse maps:is_key(<<"memory_mb">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test that empty filter sends all metrics
test_empty_filter_sends_all() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(throughput, test, 2000),

    % Connect WebSocket with empty filter
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with empty filter
            SubscribeMsg = jsx:encode(#{type => <<"subscribe">>, filter => #{}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for metrics message
            Metrics = receive_metrics(ConnPid, StreamRef, 2),

            gun:close(ConnPid),

            % Empty filter should send all metrics
            Data = maps:get(<<"data">>, Metrics),
            ?assert(maps:is_key(<<"timestamp">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test that unsubscribe clears filter
test_unsubscribe_clears_filter() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(cpu, test, 60.0),
    erlmcp_metrics_aggregator:record_metric(memory, test, 512.0),

    % Connect WebSocket
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Subscribe with CPU filter
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>, filter => #{types => [<<"cpu_percent">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for filtered metrics
            FilteredMetrics = receive_metrics(ConnPid, StreamRef, 2),
            FilteredData = maps:get(<<"data">>, FilteredMetrics),
            ?assert(maps:is_key(<<"cpu_percent">>, FilteredData)),

            % Unsubscribe
            UnsubscribeMsg = jsx:encode(#{type => <<"unsubscribe">>}),
            gun:ws_send(ConnPid, StreamRef, {text, UnsubscribeMsg}),

            % Wait for unsubscribe response
            receive
                {gun_ws, ConnPid, StreamRef, {text, UnsubMsg}} ->
                    UnsubData = jsx:decode(UnsubMsg, [return_maps]),
                    ?assertEqual(<<"unsubscribed">>, maps:get(<<"type">>, UnsubData))
            after 1000 ->
                ok
            end,

            % Now should receive no more metrics (or empty)
            receive
                {gun_ws, ConnPid, StreamRef, {text, _Msg}} ->
                    % Should not receive metrics after unsubscribe
                    ?assert(false, received_metrics_after_unsubscribe)
            after 1500 ->
                % No metrics received, which is correct
                ok
            end,

            gun:close(ConnPid);
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test that invalid filter types are ignored
test_invalid_filter_ignored() ->
    % Record some metrics
    erlmcp_metrics_aggregator:record_metric(cpu, test, 65.0),

    % Connect WebSocket with invalid filter
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with invalid metric type
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>,
                             filter => #{types => [<<"nonexistent_metric">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for metrics message
            Metrics = receive_metrics(ConnPid, StreamRef, 2),

            gun:close(ConnPid),

            % Should receive message but with minimal data
            Data = maps:get(<<"data">>, Metrics),
            ?assertNot(maps:is_key(<<"nonexistent_metric">>, Data)),
            % Should still have timestamp
            ?assert(maps:is_key(<<"timestamp">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test filter applies to structured metrics
test_filter_structured_metrics() ->
    % Record structured metrics
    erlmcp_metrics_aggregator:record_metric(latency, test, 15000),
    erlmcp_metrics_aggregator:record_metric(latency, test, 20000),
    erlmcp_metrics_aggregator:record_metric(latency, test, 25000),

    % Connect WebSocket with latency filter
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with latency filter
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>,
                             filter =>
                                 #{types =>
                                       [<<"latency_p50_us">>,
                                        <<"latency_p95_us">>,
                                        <<"latency_p99_us">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for metrics message
            Metrics = receive_metrics(ConnPid, StreamRef, 2),

            gun:close(ConnPid),

            % Verify filtered latency metrics
            Data = maps:get(<<"data">>, Metrics),
            ?assert(maps:is_key(<<"latency_p50_us">>, Data)),
            ?assert(maps:is_key(<<"latency_p95_us">>, Data)),
            ?assert(maps:is_key(<<"latency_p99_us">>, Data)),
            ?assertNot(maps:is_key(<<"cpu_percent">>, Data));
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%% @doc Test subscribe with filter protocol
test_subscribe_with_filter() ->
    % Connect WebSocket
    {ok, ConnPid} = gun:open("localhost", ?TEST_PORT),
    {ok, http} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            % Send subscribe with filter
            SubscribeMsg =
                jsx:encode(#{type => <<"subscribe">>,
                             filter => #{types => [<<"cpu_percent">>, <<"memory_mb">>]}}),
            gun:ws_send(ConnPid, StreamRef, {text, SubscribeMsg}),

            % Wait for subscribed response
            receive
                {gun_ws, ConnPid, StreamRef, {text, ResponseMsg}} ->
                    Response = jsx:decode(ResponseMsg, [return_maps]),

                    gun:close(ConnPid),

                    % Verify response structure
                    ?assertEqual(<<"subscribed">>, maps:get(<<"type">>, Response)),
                    ?assert(maps:is_key(<<"filter">>, Response)),
                    Filter = maps:get(<<"filter">>, Response),
                    ?assertEqual([<<"cpu_percent">>, <<"memory_mb">>],
                                 maps:get(<<"types">>, Filter));
                {gun_error, ConnPid, StreamRef, Reason} ->
                    gun:close(ConnPid),
                    ?assert(false, {websocket_error, Reason})
            after 5000 ->
                gun:close(ConnPid),
                ?assert(false, no_subscribed_response)
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            gun:close(ConnPid),
            ?assert(false, {websocket_upgrade_failed, Reason})
    after 5000 ->
        gun:close(ConnPid),
        ?assert(false, websocket_timeout)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Receive metrics message from WebSocket, skipping non-metrics messages
receive_metrics(ConnPid, StreamRef, Count) ->
    receive_metrics(ConnPid, StreamRef, Count, #{}).

receive_metrics(_ConnPid, _StreamRef, 0, Acc) ->
    Acc;
receive_metrics(ConnPid, StreamRef, Count, Acc) ->
    receive
        {gun_ws, ConnPid, StreamRef, {text, Msg}} ->
            Data = jsx:decode(Msg, [return_maps]),
            case maps:get(<<"type">>, Data, undefined) of
                <<"metrics">> ->
                    % Found metrics message
                    Data;
                <<"connected">> ->
                    % Skip connected message, wait for next
                    receive_metrics(ConnPid, StreamRef, Count - 1, Acc);
                <<"subscribed">> ->
                    % Skip subscribed message, wait for next
                    receive_metrics(ConnPid, StreamRef, Count - 1, Acc);
                _ ->
                    % Unknown message type, skip
                    receive_metrics(ConnPid, StreamRef, Count - 1, Acc)
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.
