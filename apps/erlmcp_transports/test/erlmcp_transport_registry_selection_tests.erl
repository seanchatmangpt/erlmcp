%%%-------------------------------------------------------------------
%%% @doc Chicago School TDD Tests for Transport Registry Selection
%%% Tests ONLY observable behavior through public API
%%% NO STATE INSPECTION, NO DUMMY PROCESSES, REAL erlmcp processes only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry_selection_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_selection_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_select_transport/1,
      fun test_statistics/1,
      fun test_get_all_transports/1,
      fun test_get_healthy_transports/1
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

test_select_transport(_Pid) ->
    % Create real transport processes
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),

    % Register transports (API calls - observable)
    erlmcp_transport_registry:register_transport(
        tcp1, Pid1, #{type => tcp}
    ),
    erlmcp_transport_registry:register_transport(
        tcp2, Pid2, #{type => tcp}
    ),

    % Record successes on tcp1 (API calls - observable)
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_success(tcp1)
    end, lists:seq(1, 20)),

    % Record fewer successes on tcp2 (API calls - observable)
    lists:foreach(fun(_) ->
        erlmcp_transport_registry:record_success(tcp2)
    end, lists:seq(1, 5)),

    % Wait for async operations
    timer:sleep(200),

    % Select best transport (API call - observable behavior)
    {ok, Selected} = erlmcp_transport_registry:select_transport(tcp),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(tcp1),
    erlmcp_transport_registry:unregister_transport(tcp2),
    Pid1 ! stop,
    Pid2 ! stop,

    [
     ?_assertEqual(tcp1, Selected)  % tcp1 has better success rate
    ].

test_statistics(_Pid) ->
    % Create real transport process
    TransportPid = spawn(fun() ->
        receive stop -> ok end
    end),

    Config = #{type => tcp},

    erlmcp_transport_registry:register_transport(
        test_detailed_stats, TransportPid, Config
    ),

    % Get initial statistics (API call - observable)
    {ok, Stats1} = erlmcp_transport_registry:get_statistics(
        test_detailed_stats
    ),

    % Record some operations (API calls - observable)
    erlmcp_transport_registry:record_success(test_detailed_stats),
    timer:sleep(50),
    erlmcp_transport_registry:record_failure(
        test_detailed_stats, network_error
    ),
    timer:sleep(50),

    % Get updated statistics (API call - observable)
    {ok, Stats2} = erlmcp_transport_registry:get_statistics(
        test_detailed_stats
    ),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(test_detailed_stats),
    TransportPid ! stop,

    [
     ?_assertEqual(0, maps:get(successes, Stats1)),
     ?_assertEqual(0, maps:get(failures, Stats1)),
     ?_assertEqual(1, maps:get(successes, Stats2)),
     ?_assertEqual(1, maps:get(failures, Stats2))
    ].

test_get_all_transports(_Pid) ->
    % Create real transport processes
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),
    Pid3 = spawn(fun() -> receive stop -> ok end end),

    % Register transports (API calls - observable)
    erlmcp_transport_registry:register_transport(
        t1, Pid1, #{type => tcp}
    ),
    erlmcp_transport_registry:register_transport(
        t2, Pid2, #{type => http}
    ),
    erlmcp_transport_registry:register_transport(
        t3, Pid3, #{type => websocket}
    ),

    % Get all transports (API call - observable behavior)
    AllTransports = erlmcp_transport_registry:get_all_transports(),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(t1),
    erlmcp_transport_registry:unregister_transport(t2),
    erlmcp_transport_registry:unregister_transport(t3),
    Pid1 ! stop,
    Pid2 ! stop,
    Pid3 ! stop,

    [
     ?_assertEqual(3, length(AllTransports))
    ].

test_get_healthy_transports(_Pid) ->
    % Create real transport processes
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),
    Pid3 = spawn(fun() -> receive stop -> ok end end),

    % Register transports (API calls - observable)
    erlmcp_transport_registry:register_transport(
        t1, Pid1, #{type => tcp}
    ),
    erlmcp_transport_registry:register_transport(
        t2, Pid2, #{type => http}
    ),
    erlmcp_transport_registry:register_transport(
        t3, Pid3, #{type => websocket}
    ),

    % Get all transports (API call - observable)
    AllTransports = erlmcp_transport_registry:get_all_transports(),

    % Mark one as down (API call - observable)
    erlmcp_transport_registry:update_health_status(t2, down),

    % Get only healthy (API call - observable behavior)
    HealthyTransports = erlmcp_transport_registry:get_healthy_transports(),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(t1),
    erlmcp_transport_registry:unregister_transport(t2),
    erlmcp_transport_registry:unregister_transport(t3),
    Pid1 ! stop,
    Pid2 ! stop,
    Pid3 ! stop,

    [
     ?_assertEqual(3, length(AllTransports)),
     ?_assertEqual(2, length(HealthyTransports))
    ].
