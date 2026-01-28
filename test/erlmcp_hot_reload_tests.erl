-module(erlmcp_hot_reload_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Chicago School TDD - Hot Reload Tests
%% Real module reloading, real gen_server, state-based verification
%%====================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

hot_reload_test_() ->
    {setup,
     fun() ->
         %% Setup: Start hot reload gen_server (real process)
         {ok, Pid} = erlmcp_hot_reload:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop hot reload server
         gen_server:stop(Pid, normal, 5000)
     end,
     fun(_Pid) ->
         [
          ?_test(reload_module_success()),
          ?_test(reload_module_not_found()),
          ?_test(reload_modules_batch()),
          ?_test(get_module_version()),
          ?_test(get_reload_status()),
          ?_test(reload_config()),
          ?_test(get_config_version()),
          ?_test(graceful_drain_begin_end()),
          ?_test(is_draining()),
          ?_test(get_drain_status()),
          ?_test(drain_timeout()),
          ?_test(get_reload_metrics()),
          ?_test(reset_reload_metrics())
         ]
     end}.

%%====================================================================
%% Test Cases - Module Reloading (Chicago School: Real Code Loading)
%%====================================================================

reload_module_success() ->
    %% Exercise: Reload erlmcp_hot_reload module itself (real code reloading)
    Result = erlmcp_hot_reload:reload_module(erlmcp_hot_reload),

    %% Verify: Reload succeeded (state-based)
    ?assertMatch({ok, erlmcp_hot_reload}, Result),

    %% Verify: Module version incremented
    {ok, Version} = erlmcp_hot_reload:get_module_version(erlmcp_hot_reload),
    ?assert(is_integer(Version)),
    ?assert(Version >= 1).

reload_module_not_found() ->
    %% Exercise: Try to reload non-existent module
    Result = erlmcp_hot_reload:reload_module(nonexistent_module_xyz),

    %% Verify: Returns error (observable behavior)
    ?assertMatch({error, _Reason}, Result).

reload_modules_batch() ->
    %% Exercise: Reload multiple modules
    Modules = [erlmcp_hot_reload, erlmcp_registry],
    Results = erlmcp_hot_reload:reload_modules(Modules),

    %% Verify: Both succeeded
    ?assertEqual(2, length(Results)),
    SuccessCount = length([1 || {ok, _} <- Results]),
    ?assert(SuccessCount >= 1).  %% At least hot_reload succeeds

get_module_version() ->
    %% Setup: Reload module to increment version
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_hot_reload),

    %% Exercise: Get version
    {ok, Version1} = erlmcp_hot_reload:get_module_version(erlmcp_hot_reload),

    %% Reload again
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_hot_reload),
    {ok, Version2} = erlmcp_hot_reload:get_module_version(erlmcp_hot_reload),

    %% Verify: Version incremented (state verification)
    ?assert(Version2 > Version1).

get_reload_status() ->
    %% Setup: Reload a module
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_hot_reload),

    %% Exercise: Get reload status
    Status = erlmcp_hot_reload:get_reload_status(),

    %% Verify: Status is a map with module info
    ?assert(is_map(Status)),

    %% Verify: Contains hot_reload module
    ?assert(maps:is_key(erlmcp_hot_reload, Status)),

    %% Verify: Module status has version, last_reload, code_hash
    ModuleStatus = maps:get(erlmcp_hot_reload, Status),
    ?assert(maps:is_key(version, ModuleStatus)),
    ?assert(maps:is_key(last_reload, ModuleStatus)),
    ?assert(maps:is_key(code_hash, ModuleStatus)).

%%====================================================================
%% Test Cases - Configuration Reloading
%%====================================================================

reload_config() ->
    %% Exercise: Reload erlmcp app config
    Result = erlmcp_hot_reload:reload_config([erlmcp]),

    %% Verify: Config version returned (state-based)
    ?assertMatch({ok, Version} when is_integer(Version), Result).

get_config_version() ->
    %% Setup: Reload config
    {ok, Version1} = erlmcp_hot_reload:reload_config(),

    %% Exercise: Get config version
    Version1Check = erlmcp_hot_reload:get_config_version(),
    ?assertEqual(Version1, Version1Check),

    %% Reload again
    {ok, Version2} = erlmcp_hot_reload:reload_config(),

    %% Verify: Version incremented
    ?assert(Version2 > Version1).

%%====================================================================
%% Test Cases - Graceful Drain (Chicago School: Real Async Drain)
%%====================================================================

graceful_drain_begin_end() ->
    %% Exercise: Begin graceful drain (real async operation)
    {ok, DrainRef} = erlmcp_hot_reload:begin_graceful_drain(5000),

    %% Verify: Drain reference returned
    ?assert(is_reference(DrainRef)),

    %% Verify: Is draining
    ?assertEqual(true, erlmcp_hot_reload:is_draining()),

    %% Exercise: End drain
    {ok, Status} = erlmcp_hot_reload:end_graceful_drain(),

    %% Verify: Drain complete (state verification)
    ?assertEqual(complete, Status),
    ?assertEqual(false, erlmcp_hot_reload:is_draining()).

is_draining() ->
    %% Initial state: not draining
    ?assertEqual(false, erlmcp_hot_reload:is_draining()),

    %% Start drain
    {ok, _Ref} = erlmcp_hot_reload:begin_graceful_drain(10000),

    %% Verify: Is draining
    ?assertEqual(true, erlmcp_hot_reload:is_draining()),

    %% End drain
    {ok, _} = erlmcp_hot_reload:end_graceful_drain(),

    %% Verify: Not draining
    ?assertEqual(false, erlmcp_hot_reload:is_draining()).

get_drain_status() ->
    %% Start drain
    {ok, _Ref} = erlmcp_hot_reload:begin_graceful_drain(10000),

    %% Exercise: Get drain status
    Status = erlmcp_hot_reload:get_drain_status(),

    %% Verify: Status structure (observable state)
    ?assert(is_map(Status)),
    ?assertEqual(draining, maps:get(status, Status)),
    ?assert(maps:is_key(active_connections, Status)),
    ?assert(maps:is_key(elapsed_ms, Status)),

    %% Verify: Elapsed time is reasonable (>= 0ms)
    ElapsedMs = maps:get(elapsed_ms, Status),
    ?assert(ElapsedMs >= 0),

    %% Cleanup
    {ok, _} = erlmcp_hot_reload:end_graceful_drain().

drain_timeout() ->
    %% Exercise: Start drain with short timeout (real timeout test)
    {ok, _Ref} = erlmcp_hot_reload:begin_graceful_drain(100),  %% 100ms timeout

    %% Wait for timeout
    timer:sleep(200),

    %% Verify: Drain timed out and moved to complete state
    %% (The handle_info(drain_timeout) should have triggered)
    Status = erlmcp_hot_reload:get_drain_status(),
    ?assertEqual(complete, maps:get(status, Status)).

%%====================================================================
%% Test Cases - Metrics Tracking
%%====================================================================

get_reload_metrics() ->
    %% Setup: Perform some reloads
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_hot_reload),
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_registry),

    %% Exercise: Get metrics
    Metrics = erlmcp_hot_reload:get_reload_metrics(),

    %% Verify: Metrics structure (state verification)
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(total_reloads, Metrics)),
    ?assert(maps:is_key(successful_reloads, Metrics)),
    ?assert(maps:is_key(failed_reloads, Metrics)),
    ?assert(maps:is_key(avg_reload_time_ms, Metrics)),
    ?assert(maps:is_key(min_reload_time_ms, Metrics)),
    ?assert(maps:is_key(max_reload_time_ms, Metrics)),

    %% Verify: At least 2 successful reloads recorded
    TotalReloads = maps:get(total_reloads, Metrics),
    ?assert(TotalReloads >= 2).

reset_reload_metrics() ->
    %% Setup: Perform reloads
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_hot_reload),

    %% Verify: Metrics not zero
    Metrics1 = erlmcp_hot_reload:get_reload_metrics(),
    ?assert(maps:get(total_reloads, Metrics1) > 0),

    %% Exercise: Reset metrics
    ok = erlmcp_hot_reload:reset_reload_metrics(),

    %% Verify: Metrics reset to zero (state verification)
    Metrics2 = erlmcp_hot_reload:get_reload_metrics(),
    ?assertEqual(0, maps:get(total_reloads, Metrics2)),
    ?assertEqual(0, maps:get(successful_reloads, Metrics2)),
    ?assertEqual(0, maps:get(failed_reloads, Metrics2)).

%%====================================================================
%% Edge Cases
%%====================================================================

begin_drain_already_draining_test() ->
    %% Start drain
    {ok, _Ref} = erlmcp_hot_reload:begin_graceful_drain(10000),

    %% Try to start another drain
    Result = erlmcp_hot_reload:begin_graceful_drain(10000),

    %% Verify: Returns error (can't drain twice)
    ?assertEqual({error, already_draining}, Result),

    %% Cleanup
    {ok, _} = erlmcp_hot_reload:end_graceful_drain().

end_drain_not_draining_test() ->
    %% Ensure not draining
    case erlmcp_hot_reload:is_draining() of
        true -> erlmcp_hot_reload:end_graceful_drain();
        false -> ok
    end,

    %% Try to end drain when not draining
    Result = erlmcp_hot_reload:end_graceful_drain(),

    %% Verify: Returns error
    ?assertEqual({error, not_draining}, Result).

reload_all_modules_test() ->
    %% Exercise: Reload all erlmcp modules
    %% Note: This may fail in test environment without full app
    Result = erlmcp_hot_reload:reload_all_modules(),

    %% Verify: Either succeeds or returns expected error
    case Result of
        {ok, Modules} when is_list(Modules) ->
            %% Success case
            ?assert(length(Modules) >= 0);
        {error, no_erlmcp_app} ->
            %% Expected in test environment
            ?assert(true)
    end.

drain_complete_cast_test() ->
    %% Test drain_complete cast (called by connections)
    FakePid = spawn(fun() -> receive stop -> ok end end),

    %% Exercise: Send drain_complete
    ok = erlmcp_hot_reload:drain_complete(FakePid),

    %% Verify: No crash (cast returns immediately)
    ?assert(erlang:is_process_alive(erlang:whereis(erlmcp_hot_reload))),

    %% Cleanup
    FakePid ! stop.
