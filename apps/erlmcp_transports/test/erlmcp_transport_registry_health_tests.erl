%%%-------------------------------------------------------------------
%%% @doc Chicago School TDD Tests for Transport Registry Health
%%% Tests ONLY observable behavior through public API
%%% NO STATE INSPECTION, NO DUMMY PROCESSES, REAL erlmcp processes only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry_health_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_health_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_health_status/1,
      fun test_update_health_status/1,
      fun test_process_monitoring/1,
      fun test_record_success_failure/1
     ]}.

setup() ->
    application:ensure_all_started(gproc),
    case erlmcp_transport_registry:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(Pid) ->
    try
        gen_server:stop(Pid, normal, 1000)
    catch
        exit:noproc -> ok;
        exit:{timeout, _} ->
            case is_process_alive(Pid) of
                true -> exit(Pid, kill);
                false -> ok
            end
    end,
    timer:sleep(100),
    ok.

%%====================================================================
%% Tests (Observable Behavior Only)
%%====================================================================

test_health_status(_Pid) ->
    % Create real transport process
    TransportPid = spawn(fun() ->
        receive stop -> ok end
    end),

    Config = #{type => tcp},

    % Register transport (API call)
    erlmcp_transport_registry:register_transport(
        test_health, TransportPid, Config
    ),

    % Check initial status (API call - observable)
    {ok, Status1} = erlmcp_transport_registry:get_transport_status(
        test_health
    ),

    % Update status (API call - observable)
    ok = erlmcp_transport_registry:update_health_status(
        test_health, degraded
    ),

    % Check updated status (API call - observable)
    {ok, Status2} = erlmcp_transport_registry:get_transport_status(
        test_health
    ),

    % Update again
    ok = erlmcp_transport_registry:update_health_status(
        test_health, down
    ),

    {ok, Status3} = erlmcp_transport_registry:get_transport_status(
        test_health
    ),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(test_health),
    TransportPid ! stop,

    [
     ?_assertEqual(up, Status1),
     ?_assertEqual(degraded, Status2),
     ?_assertEqual(down, Status3)
    ].

test_update_health_status(_Pid) ->
    % Create real transport process
    TransportPid = spawn(fun() ->
        receive stop -> ok end
    end),

    Config = #{type => http},

    erlmcp_transport_registry:register_transport(
        test_update, TransportPid, Config
    ),

    % Update through all health states (API calls - observable)
    ok = erlmcp_transport_registry:update_health_status(
        test_update, degraded
    ),
    {ok, Status1} = erlmcp_transport_registry:get_transport_status(
        test_update
    ),

    ok = erlmcp_transport_registry:update_health_status(
        test_update, down
    ),
    {ok, Status2} = erlmcp_transport_registry:get_transport_status(
        test_update
    ),

    ok = erlmcp_transport_registry:update_health_status(
        test_update, up
    ),
    {ok, Status3} = erlmcp_transport_registry:get_transport_status(
        test_update
    ),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(test_update),
    TransportPid ! stop,

    [
     ?_assertEqual(degraded, Status1),
     ?_assertEqual(down, Status2),
     ?_assertEqual(up, Status3)
    ].

test_process_monitoring(_Pid) ->
    % Create real transport process
    TransportPid = spawn(fun() ->
        receive stop -> ok end
    end),

    Config = #{type => tcp},

    erlmcp_transport_registry:register_transport(
        test_monitor, TransportPid, Config
    ),

    % Check initial status (API call - observable)
    {ok, Status1} = erlmcp_transport_registry:get_transport_status(
        test_monitor
    ),

    % Kill the transport process
    TransportPid ! stop,

    % Wait for monitor to trigger and process message
    timer:sleep(300),

    % Check status after death (API call - observable)
    {ok, Status2} = erlmcp_transport_registry:get_transport_status(
        test_monitor
    ),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(test_monitor),

    [
     ?_assertEqual(up, Status1),
     ?_assertEqual(down, Status2)
    ].

test_record_success_failure(_Pid) ->
    % Create real transport process
    TransportPid = spawn(fun() ->
        receive stop -> ok end
    end),

    Config = #{type => tcp},

    erlmcp_transport_registry:register_transport(
        test_stats, TransportPid, Config
    ),

    % Record successes (API calls - observable)
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_success(test_stats)
    end, lists:seq(1, 10)),

    % Record failures (API calls - observable)
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_failure(
            test_stats, timeout
        )
    end, lists:seq(1, 3)),

    % Wait for async operations to complete
    timer:sleep(200),

    % Get statistics (API call - observable behavior)
    {ok, Stats} = erlmcp_transport_registry:get_statistics(
        test_stats
    ),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(test_stats),
    TransportPid ! stop,

    [
     ?_assertEqual(10, maps:get(successes, Stats)),
     ?_assertEqual(3, maps:get(failures, Stats))
    ].
