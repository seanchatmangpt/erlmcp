%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Tests for Transport Registry
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_start_stop/1,
      fun test_register_unregister/1,
      fun test_get_transport/1,
      fun test_health_status/1,
      fun test_record_success_failure/1,
      fun test_select_transport/1,
      fun test_statistics/1,
      fun test_process_monitoring/1
     ]}.

setup() ->
    % Start required applications
    application:ensure_all_started(gproc),

    % Start registry
    {ok, Pid} = erlmcp_transport_registry:start_link(),

    Pid.

cleanup(Pid) ->
    % Stop registry gracefully using gen_server:stop
    try
        gen_server:stop(Pid, normal, 1000)
    catch
        exit:noproc ->
            % Already stopped, that's fine
            ok;
        exit:{timeout, _} ->
            % Timeout stopping, force exit
            case is_process_alive(Pid) of
                true -> exit(Pid, kill);
                false -> ok
            end
    end,

    % Wait for cleanup to complete
    timer:sleep(100),

    ok.

%%====================================================================
%% Tests
%%====================================================================

test_start_stop(_Pid) ->
    [
     ?_assert(is_pid(whereis(erlmcp_transport_registry)))
    ].

test_register_unregister(_Pid) ->
    % Spawn a dummy transport process that will respond to stop
    TransportPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),

    % Verify process is alive
    ?assert(is_process_alive(TransportPid)),

    Config = #{
        type => tcp,
        host => "localhost",
        port => 3000
    },

    % Register transport
    Result1 = erlmcp_transport_registry:register_transport(test_transport, TransportPid, Config),

    % Try to register again (should fail)
    Result2 = erlmcp_transport_registry:register_transport(test_transport, TransportPid, Config),

    % Unregister transport
    Result3 = erlmcp_transport_registry:unregister_transport(test_transport),

    % Clean up the transport process
    TransportPid ! stop,

    % Wait for cleanup
    timer:sleep(50),

    [
     ?_assertEqual(ok, Result1),
     ?_assertEqual({error, already_registered}, Result2),
     ?_assertEqual(ok, Result3)
    ].

test_get_transport(_Pid) ->
    % Register a transport
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{type => tcp, host => "localhost", port => 3000},

    Result1 = erlmcp_transport_registry:register_transport(test_get, TransportPid, Config),

    % Get transport info
    Result2 = erlmcp_transport_registry:get_transport(test_get),

    % Clean up
    erlmcp_transport_registry:unregister_transport(test_get),
    TransportPid ! stop,
    timer:sleep(50),

    [
     ?_assertEqual(ok, Result1),
     ?_assertMatch({ok, #{transport_id := test_get, type := tcp, health_status := up}}, Result2)
    ].

test_health_status(_Pid) ->
    % Register a transport
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{type => tcp},

    erlmcp_transport_registry:register_transport(test_health, TransportPid, Config),

    % Check initial status
    {ok, Status1} = erlmcp_transport_registry:get_transport_status(test_health),

    % Update status
    ok = erlmcp_transport_registry:update_health_status(test_health, degraded),
    {ok, Status2} = erlmcp_transport_registry:get_transport_status(test_health),

    % Update again
    ok = erlmcp_transport_registry:update_health_status(test_health, down),
    {ok, Status3} = erlmcp_transport_registry:get_transport_status(test_health),

    % Clean up
    erlmcp_transport_registry:unregister_transport(test_health),
    TransportPid ! stop,
    timer:sleep(50),

    [
     ?_assertEqual(up, Status1),
     ?_assertEqual(degraded, Status2),
     ?_assertEqual(down, Status3)
    ].

test_record_success_failure(_Pid) ->
    % Register a transport
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{type => tcp},

    erlmcp_transport_registry:register_transport(test_stats, TransportPid, Config),

    % Record successes
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_success(test_stats)
    end, lists:seq(1, 10)),

    % Record failures
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_failure(test_stats, timeout)
    end, lists:seq(1, 3)),

    % Wait for async operations to complete
    timer:sleep(200),

    % Get statistics
    {ok, Stats} = erlmcp_transport_registry:get_statistics(test_stats),

    % Clean up
    erlmcp_transport_registry:unregister_transport(test_stats),
    TransportPid ! stop,
    timer:sleep(50),

    [
     ?_assertEqual(10, maps:get(successes, Stats)),
     ?_assertEqual(3, maps:get(failures, Stats))
    ].

test_select_transport(_Pid) ->
    % Register multiple transports of same type
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),

    erlmcp_transport_registry:register_transport(tcp1, Pid1, #{type => tcp}),
    erlmcp_transport_registry:register_transport(tcp2, Pid2, #{type => tcp}),

    % Record successes on tcp1
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_success(tcp1)
    end, lists:seq(1, 20)),

    % Record fewer successes on tcp2
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_success(tcp2)
    end, lists:seq(1, 5)),

    % Wait for async operations
    timer:sleep(200),

    % Select best transport
    {ok, Selected} = erlmcp_transport_registry:select_transport(tcp),

    % Clean up
    erlmcp_transport_registry:unregister_transport(tcp1),
    erlmcp_transport_registry:unregister_transport(tcp2),
    Pid1 ! stop,
    Pid2 ! stop,
    timer:sleep(50),

    [
     ?_assertEqual(tcp1, Selected)  % tcp1 has better success rate
    ].

test_statistics(_Pid) ->
    % Register a transport
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{type => tcp},

    erlmcp_transport_registry:register_transport(test_detailed_stats, TransportPid, Config),

    % Get initial statistics
    {ok, Stats1} = erlmcp_transport_registry:get_statistics(test_detailed_stats),

    % Record some operations
    erlmcp_transport_registry:record_success(test_detailed_stats),
    timer:sleep(50),
    erlmcp_transport_registry:record_failure(test_detailed_stats, network_error),
    timer:sleep(50),

    % Get updated statistics
    {ok, Stats2} = erlmcp_transport_registry:get_statistics(test_detailed_stats),

    % Clean up
    erlmcp_transport_registry:unregister_transport(test_detailed_stats),
    TransportPid ! stop,
    timer:sleep(50),

    [
     ?_assertEqual(0, maps:get(successes, Stats1)),
     ?_assertEqual(0, maps:get(failures, Stats1)),
     ?_assertEqual(1, maps:get(successes, Stats2)),
     ?_assertEqual(1, maps:get(failures, Stats2))
    ].

test_process_monitoring(_Pid) ->
    % Register a transport
    TransportPid = spawn(fun() ->
        receive
            die -> exit(normal)
        end
    end),

    Config = #{type => tcp},
    erlmcp_transport_registry:register_transport(test_monitor, TransportPid, Config),

    % Check initial status
    {ok, Status1} = erlmcp_transport_registry:get_transport_status(test_monitor),

    % Kill the transport process
    TransportPid ! die,

    % Wait for monitor to trigger and process message
    timer:sleep(300),

    % Check status after death
    {ok, Status2} = erlmcp_transport_registry:get_transport_status(test_monitor),

    % Clean up
    erlmcp_transport_registry:unregister_transport(test_monitor),

    [
     ?_assertEqual(up, Status1),
     ?_assertEqual(down, Status2)
    ].

%%====================================================================
%% Health Evaluation Tests
%%====================================================================

health_evaluation_test() ->
    % Simulate different failure scenarios
    Stats1 = #{successes => 100, failures => 0},
    Stats2 = #{successes => 50, failures => 3},
    Stats3 = #{successes => 50, failures => 5},
    Stats4 = #{successes => 50, failures => 10},

    State = #{failure_threshold => 5, degraded_threshold => 3},

    Result1 = evaluate_health_helper(Stats1, State),
    Result2 = evaluate_health_helper(Stats2, State),
    Result3 = evaluate_health_helper(Stats3, State),
    Result4 = evaluate_health_helper(Stats4, State),

    ?assertEqual(up, Result1),
    ?assertEqual(degraded, Result2),
    ?assertEqual(down, Result3),
    ?assertEqual(down, Result4).

evaluate_health_helper(Stats, State) ->
    Failures = maps:get(failures, Stats, 0),
    FailureThreshold = maps:get(failure_threshold, State),
    DegradedThreshold = maps:get(degraded_threshold, State),

    if
        Failures >= FailureThreshold -> down;
        Failures >= DegradedThreshold -> degraded;
        true -> up
    end.

%%====================================================================
%% Transport Selection Tests
%%====================================================================

success_rate_test() ->
    Stats1 = #{successes => 100, failures => 0},
    Stats2 = #{successes => 80, failures => 20},
    Stats3 = #{successes => 50, failures => 50},
    Stats4 = #{successes => 0, failures => 0},

    Rate1 = calculate_success_rate(Stats1),
    Rate2 = calculate_success_rate(Stats2),
    Rate3 = calculate_success_rate(Stats3),
    Rate4 = calculate_success_rate(Stats4),

    ?assertEqual(1.0, Rate1),
    ?assertEqual(0.8, Rate2),
    ?assertEqual(0.5, Rate3),
    ?assertEqual(1.0, Rate4).  % No data, assume perfect

calculate_success_rate(Stats) ->
    Successes = maps:get(successes, Stats, 0),
    Failures = maps:get(failures, Stats, 0),
    Total = Successes + Failures,

    case Total of
        0 -> 1.0;
        _ -> Successes / Total
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

get_all_transports_test() ->
    % Setup
    {ok, RegistryPid} = erlmcp_transport_registry:start_link(),

    % Register multiple transports
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),
    Pid3 = spawn(fun() -> receive stop -> ok end end),

    erlmcp_transport_registry:register_transport(t1, Pid1, #{type => tcp}),
    erlmcp_transport_registry:register_transport(t2, Pid2, #{type => http}),
    erlmcp_transport_registry:register_transport(t3, Pid3, #{type => websocket}),

    % Get all transports
    AllTransports = erlmcp_transport_registry:get_all_transports(),

    % Mark one as down
    erlmcp_transport_registry:update_health_status(t2, down),

    % Get only healthy
    HealthyTransports = erlmcp_transport_registry:get_healthy_transports(),

    % Clean up
    erlmcp_transport_registry:unregister_transport(t1),
    erlmcp_transport_registry:unregister_transport(t2),
    erlmcp_transport_registry:unregister_transport(t3),

    Pid1 ! stop,
    Pid2 ! stop,
    Pid3 ! stop,

    timer:sleep(100),

    gen_server:stop(RegistryPid, normal, 1000),

    ?assertEqual(3, length(AllTransports)),
    ?assertEqual(2, length(HealthyTransports)).
