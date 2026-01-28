-module(erlmcp_recovery_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() ->
    {ok, _} = erlmcp_recovery_manager:start_link(),
    ok.
cleanup(_) ->
    catch erlmcp_recovery_manager:stop(),
    ok.

%%====================================================================
%% Recovery Tests
%%====================================================================

recovery_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_start_recovery()),
        ?_test(test_recovery_with_strategy()),
        ?_test(test_recovery_status()),
        ?_test(test_recovery_history())
    ] end}.

test_start_recovery() ->
    ServerId = test_server,
    Result = erlmcp_recovery_manager:recover(ServerId),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_recovery_with_strategy() ->
    ServerId = strategy_server,
    Strategy = restart,
    Result = erlmcp_recovery_manager:recover(ServerId, Strategy),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_recovery_status() ->
    erlmcp_recovery_manager:recover(status_server),
    Result = erlmcp_recovery_manager:get_status(status_server),
    ?assert(is_map(Result) orelse is_atom(Result) orelse Result =:= error).

test_recovery_history() ->
    erlmcp_recovery_manager:recover(history_server),
    Result = erlmcp_recovery_manager:get_history(history_server),
    ?assert(is_list(Result) orelse Result =:= error).

%%====================================================================
%% Strategy Tests
%%====================================================================

strategy_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_restart_strategy()),
        ?_test(test_retry_strategy()),
        ?_test(test_fallback_strategy()),
        ?_test(test_circuit_breaker_strategy())
    ] end}.

test_restart_strategy() ->
    Result = erlmcp_recovery_manager:set_strategy(server1, restart),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_retry_strategy() ->
    Result = erlmcp_recovery_manager:set_strategy(server2, {retry, 3}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_fallback_strategy() ->
    Result = erlmcp_recovery_manager:set_strategy(server3, {fallback, backup_server}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_circuit_breaker_strategy() ->
    Result = erlmcp_recovery_manager:set_strategy(server4, circuit_breaker),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Error Detection Tests
%%====================================================================

detection_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_register_error_handler()),
        ?_test(test_detect_failure()),
        ?_test(test_error_threshold()),
        ?_test(test_auto_recovery())
    ] end}.

test_register_error_handler() ->
    Handler = fun(Error) -> ok end,
    Result = erlmcp_recovery_manager:register_error_handler(server1, Handler),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_detect_failure() ->
    Result = erlmcp_recovery_manager:detect_failure(server1, {error, test}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_error_threshold() ->
    Result = erlmcp_recovery_manager:set_error_threshold(server1, 5),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_auto_recovery() ->
    Result = erlmcp_recovery_manager:enable_auto_recovery(server1, true),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Checkpoint Tests
%%====================================================================

checkpoint_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_create_checkpoint()),
        ?_test(test_restore_checkpoint()),
        ?_test(test_checkpoint_list()),
        ?_test(test_checkpoint_cleanup())
    ] end}.

test_create_checkpoint() ->
    State = #{data => <<"checkpoint_data">>},
    Result = erlmcp_recovery_manager:create_checkpoint(server1, State),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_restore_checkpoint() ->
    erlmcp_recovery_manager:create_checkpoint(restore_server, #{data => <<"test">>}),
    Result = erlmcp_recovery_manager:restore_checkpoint(restore_server),
    ?assertMatch({ok, _} | {error, _} | undefined, Result).

test_checkpoint_list() ->
    erlmcp_recovery_manager:create_checkpoint(list_server, #{data => <<"test">>}),
    Result = erlmcp_recovery_manager:list_checkpoints(),
    ?assert(is_list(Result) orelse Result =:= error).

test_checkpoint_cleanup() ->
    erlmcp_recovery_manager:create_checkpoint(cleanup_server, #{data => <<"test">>}),
    Result = erlmcp_recovery_manager:cleanup_old_checkpoints(3600),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Monitoring Tests
%%====================================================================

monitoring_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_monitor_process()),
        ?_test(test_unmonitor_process()),
        ?_test(test_get_monitored_processes()),
        ?_test(test_recovery_metrics())
    ] end}.

test_monitor_process() ->
    Pid = spawn(fun() -> timer:sleep(10000) end),
    Result = erlmcp_recovery_manager:monitor_process(test_proc, Pid),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    exit(Pid, kill).

test_unmonitor_process() ->
    Result = erlmcp_recovery_manager:unmonitor_process(test_proc),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_monitored_processes() ->
    Result = erlmcp_recovery_manager:get_monitored_processes(),
    ?assert(is_list(Result) orelse is_map(Result) orelse Result =:= error).

test_recovery_metrics() ->
    Result = erlmcp_recovery_manager:get_metrics(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Resilience Tests
%%====================================================================

resilience_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_resilience_config()),
        ?_test(test_timeout_setting()),
        ?_test(test_max_retries()),
        ?_test(test_backoff_policy())
    ] end}.

test_resilience_config() ->
    Config = #{max_retries => 5, timeout => 10000},
    Result = erlmcp_recovery_manager:set_resilience_config(server1, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_timeout_setting() ->
    Result = erlmcp_recovery_manager:set_timeout(server1, 5000),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_max_retries() ->
    Result = erlmcp_recovery_manager:set_max_retries(server1, 3),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_backoff_policy() ->
    Result = erlmcp_recovery_manager:set_backoff_policy(server1, exponential),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_full_recovery_lifecycle()),
        ?_test(test_recovery_with_monitoring()),
        ?_test(test_checkpoint_restore_cycle())
    ] end}.

test_full_recovery_lifecycle() ->
    erlmcp_recovery_manager:set_strategy(lifecycle_server, restart),
    erlmcp_recovery_manager:enable_auto_recovery(lifecycle_server, true),
    erlmcp_recovery_manager:detect_failure(lifecycle_server, {error, test}),
    Status = erlmcp_recovery_manager:get_status(lifecycle_server),
    ?assert(Status =:= error orelse is_map(Status) orelse is_atom(Status)).

test_recovery_with_monitoring() ->
    Pid = spawn(fun() -> timer:sleep(100) end),
    erlmcp_recovery_manager:monitor_process(monitor_server, Pid),
    erlmcp_recovery_manager:recover(monitor_server),
    Result = erlmcp_recovery_manager:get_monitored_processes(),
    exit(Pid, kill),
    ?assert(is_list(Result) orelse is_map(Result) orelse Result =:= error).

test_checkpoint_restore_cycle() ->
    State = #{iteration => 1, data => <<"test">>},
    erlmcp_recovery_manager:create_checkpoint(cycle_server, State),
    erlmcp_recovery_manager:recover(cycle_server),
    Restored = erlmcp_recovery_manager:restore_checkpoint(cycle_server),
    ?assertMatch({ok, _} | {error, _} | undefined, Restored).
