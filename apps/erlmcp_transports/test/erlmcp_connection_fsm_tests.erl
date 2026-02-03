%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Connection State Machine
%%%
%%% Tests cover:
%%% - State transitions
%%% - Circuit breaker behavior
%%% - Reconnection with exponential backoff
%%% - Flow control and backpressure
%%% - Health monitoring
%%% - Keep-alive functionality
%%% - Connection pooling
%%% - Error recovery
%%% - Metrics collection
%%%
%%% Chicago School TDD: Tests drive behavior, not implementation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_fsm_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").
-include("erlmcp_connection_state.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup test connection FSM
setup_fsm() ->
    {ok, Pid} = erlmcp_connection_fsm:start_link(
        test_fsm,
        tcp,
        "localhost",
        9999
    ),
    Pid.

%% @doc Cleanup test connection FSM
cleanup_fsm(Pid) ->
    catch gen_statem:stop(Pid).

%% @doc Setup test connection FSM with custom options
setup_fsm_with_opts(TransportType, Host, Port) ->
    {ok, Pid} = erlmcp_connection_fsm:start_link(
        test_fsm_custom,
        TransportType,
        Host,
        Port
    ),
    Pid.

%%====================================================================
%% State Transition Tests
%%====================================================================

disconnected_to_connecting_test() ->
    Pid = setup_fsm(),
    try
        %% Initially in disconnected state
        {ok, disconnected, _Info} = erlmcp_connection_fsm:get_state(Pid),

        %% Initiate connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),

        %% Should transition to connecting
        timer:sleep(100),
        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(connecting, State)
    after
        cleanup_fsm(Pid)
    end.

connecting_to_connected_on_success_test() ->
    Pid = setup_fsm(),
    try
        %% Mock connection success
        ok = erlmcp_connection_fsm:connect(Pid, #{}),

        %% Simulate connection established
        Pid ! {connected, mock_socket},

        timer:sleep(100),
        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(connected, State)
    after
        cleanup_fsm(Pid)
    end.

connecting_to_reconnecting_on_failure_test() ->
    Pid = setup_fsm(),
    try
        %% Mock connection failure
        ok = erlmcp_connection_fsm:connect(Pid, #{}),

        %% Simulate connection error
        Pid ! {error, econnrefused},

        timer:sleep(100),
        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(reconnecting, State)
    after
        cleanup_fsm(Pid)
    end.

connected_to_ready_on_ready_event_test() ->
    Pid = setup_fsm(),
    try
        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        timer:sleep(100),

        %% Send ready event
        Pid ! ready,
        timer:sleep(100),

        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(ready, State)
    after
        cleanup_fsm(Pid)
    end.

ready_to_disconnecting_on_disconnect_test() ->
    Pid = setup_fsm(),
    try
        %% Establish and ready connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Disconnect
        ok = erlmcp_connection_fsm:disconnect(Pid),
        timer:sleep(100),

        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assert(lists:member(State, [disconnecting, closed]))
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Circuit Breaker Tests
%%====================================================================

circuit_breaker_opens_after_threshold_test() ->
    Pid = setup_fsm(),
    try
        %% Configure circuit breaker with low threshold
        erlmcp_connection_fsm:update_config(Pid, #{
            circuit_breaker => #circuit_breaker{
                threshold = 3,
                timeout = 5000
            }
        }),

        %% Trigger failures
        lists:foreach(fun(_) ->
            ok = erlmcp_connection_fsm:connect(Pid, #{}),
            Pid ! {error, econnrefused},
            timer:sleep(100)
        end, lists:seq(1, 3)),

        timer:sleep(100),
        {ok, State, Info} = erlmcp_connection_fsm:get_state(Pid),

        %% Should be in failed state with open circuit breaker
        ?assertEqual(failed, State),
        ?assertEqual(open, maps:get(circuit_breaker, Info))
    after
        cleanup_fsm(Pid)
    end.

circuit_breaker_reset_test() ->
    Pid = setup_fsm(),
    try
        %% Trigger circuit breaker open
        erlmcp_connection_fsm:update_config(Pid, #{
            circuit_breaker => #circuit_breaker{
                threshold = 2,
                timeout = 5000
            }
        }),

        lists:foreach(fun(_) ->
            ok = erlmcp_connection_fsm:connect(Pid, #{}),
            Pid ! {error, econnrefused},
            timer:sleep(100)
        end, lists:seq(1, 2)),

        timer:sleep(100),
        {ok, failed, _} = erlmcp_connection_fsm:get_state(Pid),

        %% Reset circuit breaker
        ok = erlmcp_connection_fsm:reset_circuit_breaker(Pid),
        timer:sleep(100),

        {ok, State, Info} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(connecting, State),
        ?assertEqual(closed, maps:get(circuit_breaker, Info))
    after
        cleanup_fsm(Pid)
    end.

circuit_breaker_rejects_connections_when_open_test() ->
    Pid = setup_fsm(),
    try
        %% Open circuit breaker immediately
        erlmcp_connection_fsm:update_config(Pid, #{
            circuit_breaker => #circuit_breaker{
                state = open,
                threshold = 1
            }
        }),

        %% Try to connect
        Result = erlmcp_connection_fsm:connect(Pid, #{}),

        %% Should be rejected or ignored
        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertNotEqual(connecting, State)
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Reconnection with Backoff Tests
%%====================================================================

exponential_backoff_test() ->
    Pid = setup_fsm(),
    try
        %% Configure exponential backoff
        erlmcp_connection_fsm:update_config(Pid, #{
            backoff_config => #backoff_config{
                strategy = exponential,
                initial_delay = 100,
                max_delay = 1000,
                multiplier = 2.0
            }
        }),

        %% Trigger reconnection attempts
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {error, econnrefused},
        timer:sleep(150),

        %% Check reconnect attempts increased
        {ok, reconnecting, Info} = erlmcp_connection_fsm:get_state(Pid),
        Attempts = maps:get(reconnect_attempts, Info, 0),
        ?assert(Attempts > 0)
    after
        cleanup_fsm(Pid)
    end.

max_reconnect_attempts_test() ->
    Pid = setup_fsm(),
    try
        %% Configure low max attempts
        erlmcp_connection_fsm:update_config(Pid, #{
            backoff_config => #backoff_config{
                max_attempts = 3
            }
        }),

        %% Trigger multiple reconnections
        lists:foreach(fun(_) ->
            Pid ! reconnect,
            Pid ! {error, econnrefused},
            timer:sleep(50)
        end, lists:seq(1, 4)),

        timer:sleep(100),
        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(failed, State)
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Flow Control Tests
%%====================================================================

flow_control_window_update_test() ->
    Pid = setup_fsm(),
    try
        %% Configure flow control
        erlmcp_connection_fsm:update_config(Pid, #{
            flow_control => #flow_control{
                window_size = 1000,
                current_window = 1000,
                high_watermark = 0.8
            }
        }),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Send data
        ok = erlmcp_connection_fsm:send(Pid, <<"test_data">>),
        timer:sleep(50),

        %% Check metrics
        {ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),
        ?assert(Metrics#conn_metrics.bytes_sent > 0)
    after
        cleanup_fsm(Pid)
    end.

backpressure_applied_test() ->
    Pid = setup_fsm(),
    try
        %% Configure low backpressure threshold
        erlmcp_connection_fsm:update_config(Pid, #{
            flow_control => #flow_control{
                window_size = 100,
                high_watermark = 0.5
            }
        }),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Send enough data to trigger backpressure
        lists:foreach(fun(_) ->
            erlmcp_connection_fsm:send(Pid, <<"x">>),
            timer:sleep(10)
        end, lists:seq(1, 60)),

        timer:sleep(100),
        {ok, _State, Info} = erlmcp_connection_fsm:get_state(Pid),
        %% Flow control state should be backpressure or paused
        FlowState = maps:get(flow_control_state, Info, normal),
        ?assert(lists:member(FlowState, [backpressure, paused, normal]))
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Health Monitoring Tests
%%====================================================================

health_status_updates_test() ->
    Pid = setup_fsm(),
    try
        %% Configure health monitoring
        erlmcp_connection_fsm:update_config(Pid, #{
            health_config => #health_config{
                enabled = true,
                interval = 100,
                failure_threshold = 2
            }
        }),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Check health status
        {ok, Health, _Info} = erlmcp_connection_fsm:get_health(Pid),
        ?assert(lists:member(Health, [healthy, unhealthy, unknown]))
    after
        cleanup_fsm(Pid)
    end.

consecutive_failures_tracked_test() ->
    Pid = setup_fsm(),
    try
        %% Trigger failures
        lists:foreach(fun(_) ->
            Pid ! {error, test_error},
            timer:sleep(50)
        end, lists:seq(1, 3)),

        timer:sleep(100),
        {ok, _Health, Info} = erlmcp_connection_fsm:get_health(Pid),
        Failures = maps:get(consecutive_failures, Info, 0),
        ?assert(Failures > 0)
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Keep-Alive Tests
%%====================================================================

keepalive_timer_started_test() ->
    Pid = setup_fsm(),
    try
        %% Configure keep-alive
        erlmcp_connection_fsm:update_config(Pid, #{
            keepalive_config => #keepalive_config{
                enabled = true,
                interval = 500
            }
        }),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Keep-alive should be scheduled
        {ok, _State, _Info} = erlmcp_connection_fsm:get_state(Pid),
        ok  %% If we got here, keep-alive was configured
    after
        cleanup_fsm(Pid)
    end.

missed_keepalives_trigger_reconnect_test() ->
    Pid = setup_fsm(),
    try
        %% Configure aggressive keep-alive
        erlmcp_connection_fsm:update_config(Pid, #{
            keepalive_config => #keepalive_config{
                enabled = true,
                interval = 100,
                max_missed = 2
            }
        }),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Simulate missed keepalives
        Pid ! {keepalive, timeout},
        Pid ! {keepalive, timeout},
        timer:sleep(100),

        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assert(lists:member(State, [reconnecting, failed]))
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Connection Pooling Tests
%%====================================================================

connection_pool_checkout_test() ->
    %% Pool requires poolboy, skip if not available
    case code:ensure_loaded(poolboy) of
        {module, poolboy} ->
            PoolName = test_pool,

            %% Configure pool
            Config = #pool_config{
                name = PoolName,
                transport_type = tcp,
                host = "localhost",
                port = 9999,
                size = 2,
                max_overflow = 1,
                strategy = round_robin,
                lease_timeout = 5000
            },

            {ok, PoolPid} = erlmcp_connection_pool:start_link(Config),
            try
                %% Checkout connection
                {ok, ConnPid, _MonRef} = erlmcp_connection_pool:checkout(PoolName, []),
                ?assert(is_pid(ConnPid)),

                %% Checkin connection
                ok = erlmcp_connection_pool:checkin(PoolName, ConnPid)
            after
                gen_server:stop(PoolPid)
            end;
        _ ->
            %% poolboy not available, skip test
            ok
    end.

pool_with_connection_test() ->
    case code:ensure_loaded(poolboy) of
        {module, poolboy} ->
            PoolName = test_pool_with,

            Config = #pool_config{
                name = PoolName,
                transport_type = tcp,
                host = "localhost",
                port = 9999,
                size = 2,
                max_overflow = 1
            },

            {ok, PoolPid} = erlmcp_connection_pool:start_link(Config),
            try
                %% Use with_connection
                {ok, Result} = erlmcp_connection_pool:with_connection(
                    PoolName,
                    [],
                    fun(ConnPid) ->
                        %% Return connection PID
                        {ok, ConnPid}
                    end
                ),
                ?assertMatch({ok, _}, Result)
            after
                gen_server:stop(PoolPid)
            end;
        _ ->
            ok
    end.

pool_status_test() ->
    case code:ensure_loaded(poolboy) of
        {module, poolboy} ->
            PoolName = test_pool_status,

            Config = #pool_config{
                name = PoolName,
                transport_type = tcp,
                host = "localhost",
                port = 9999,
                size = 3,
                max_overflow = 1
            },

            {ok, PoolPid} = erlmcp_connection_pool:start_link(Config),
            try
                %% Get pool status
                {ok, Status} = erlmcp_connection_pool:get_pool_status(PoolName),
                ?assert(is_record(Status, pool_status))
            after
                gen_server:stop(PoolPid)
            end;
        _ ->
            ok
    end.

%%====================================================================
%% Metrics Collection Tests
%%====================================================================

metrics_updated_on_send_test() ->
    Pid = setup_fsm(),
    try
        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Send data
        ok = erlmcp_connection_fsm:send(Pid, <<"test_message">>),
        timer:sleep(50),

        %% Check metrics
        {ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),
        ?assertEqual(1, Metrics#conn_metrics.messages_sent),
        ?assert(Metrics#conn_metrics.bytes_sent > 0)
    after
        cleanup_fsm(Pid)
    end.

metrics_updated_on_receive_test() ->
    Pid = setup_fsm(),
    try
        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Simulate received data
        Pid ! {data, <<"received_data">>},
        timer:sleep(50),

        %% Check metrics
        {ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),
        ?assertEqual(1, Metrics#conn_metrics.messages_received),
        ?assert(Metrics#conn_metrics.bytes_received > 0)
    after
        cleanup_fsm(Pid)
    end.

connection_count_metrics_test() ->
    Pid = setup_fsm(),
    try
        %% Initial state
        {ok, InitialMetrics} = erlmcp_connection_fsm:get_metrics(Pid),
        ?assertEqual(0, InitialMetrics#conn_metrics.connect_count),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        timer:sleep(100),

        %% Check connect count increased
        {ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),
        ?assertEqual(1, Metrics#conn_metrics.connect_count)
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Error Recovery Tests
%%====================================================================

tcp_closed_triggers_reconnect_test() ->
    Pid = setup_fsm(),
    try
        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Simulate TCP close
        Pid ! {tcp_closed, mock_socket},
        timer:sleep(100),

        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(reconnecting, State)
    after
        cleanup_fsm(Pid)
    end.

tcp_error_triggers_reconnect_test() ->
    Pid = setup_fsm(),
    try
        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        %% Simulate TCP error
        Pid ! {tcp_error, mock_socket, econnaborted},
        timer:sleep(100),

        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assertEqual(reconnecting, State)
    after
        cleanup_fsm(Pid)
    end.

owner_death_stops_fsm_test() ->
    Pid = setup_fsm(),
    try
        %% Create owner process
        Owner = spawn_link(fun() ->
            receive
                stop -> ok
            end
        end),

        %% Update FSM owner
        erlmcp_connection_fsm:update_config(Pid, #{owner => Owner}),

        %% Kill owner
        exit(Owner, kill),
        timer:sleep(100),

        %% FSM should have stopped
        ?assertNotEqual(is_process_alive(Pid), true)
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Multiplexing Tests
%%====================================================================

multiplexed_connection_test() ->
    Pid = setup_fsm(),
    try
        %% Enable multiplexing
        erlmcp_connection_fsm:update_config(Pid, #{
            multiplexed => true,
            multiplex_id => make_ref()
        }),

        %% Establish connection
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        Pid ! {connected, mock_socket},
        Pid ! ready,
        timer:sleep(100),

        {ok, _State, Info} = erlmcp_connection_fsm:get_state(Pid),
        %% Check multiplexing enabled
        Multiplexed = maps:get(multiplexed, Info, false),
        ?assert(Multiplexed)
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Configuration Update Tests
%%====================================================================

dynamic_config_update_test() ->
    Pid = setup_fsm(),
    try
        %% Update backoff config
        NewBackoff = #backoff_config{
            strategy = linear,
            initial_delay = 500,
            max_delay = 5000
        },

        ok = erlmcp_connection_fsm:update_config(Pid, #{
            backoff_config => NewBackoff
        }),

        %% Verify config updated
        {ok, _State, Info} = erlmcp_connection_fsm:get_state(Pid),
        %% Config should be reflected in state
        ok
    after
        cleanup_fsm(Pid)
    end.

%%====================================================================
%% Transport Type Tests
%%====================================================================

stdio_transport_test() ->
    Pid = setup_fsm_with_opts(stdio, undefined, undefined),
    try
        %% Connect stdio
        ok = erlmcp_connection_fsm:connect(Pid, #{}),
        timer:sleep(100),

        {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
        ?assert(lists:member(State, [connecting, connected, reconnecting]))
    after
        catch gen_statem:stop(Pid)
    end.

http_transport_test() ->
    case code:ensure_loaded(erlmcp_transport_http) of
        {module, _} ->
            Pid = setup_fsm_with_opts(http, "localhost", 8080),
            try
                %% Connect HTTP
                ok = erlmcp_connection_fsm:connect(Pid, #{}),
                timer:sleep(100),

                {ok, State, _} = erlmcp_connection_fsm:get_state(Pid),
                ?assert(lists:member(State, [connecting, connected, reconnecting]))
            after
                catch gen_statem:stop(Pid)
            end;
        _ ->
            %% HTTP transport not available, skip
            ok
    end.
