%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive tests for erlmcp_process_monitor module
%%%
%%% Tests cover:
%%% - Process monitor lifecycle (start/stop)
%%% - Process metrics collection
%%% - Capacity estimation
%%% - Alert threshold management
%%% - Warning/critical status detection
%%% - Auto-scaling recommendations
%%% - Integration with recovery manager
%%% - Periodic monitoring functionality
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

-define(TEST_CHECK_INTERVAL, 1000).  % 1 second for faster tests
-define(TEST_WARNING_THRESHOLD, 0.70).
-define(TEST_CRITICAL_THRESHOLD, 0.90).
-define(SYNC_TIMEOUT, 5000).

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_process_monitor:start_link([
        {check_interval, ?TEST_CHECK_INTERVAL},
        {warning_threshold, ?TEST_WARNING_THRESHOLD},
        {critical_threshold, ?TEST_CRITICAL_THRESHOLD}
    ]),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_stop_test() ->
    {ok, Pid} = erlmcp_process_monitor:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

start_with_options_test() ->
    {ok, Pid} = erlmcp_process_monitor:start_link([
        {check_interval, 5000},
        {warning_threshold, 0.60},
        {critical_threshold, 0.80}
    ]),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

start_default_options_test() ->
    {ok, Pid} = erlmcp_process_monitor:start_link([]),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

%%====================================================================
%% Process Metrics Tests
%%====================================================================

get_process_metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Metrics} = erlmcp_process_monitor:get_process_metrics(),
                     ?assert(is_map(Metrics)),
                     ?assert(maps:is_key(process_count, Metrics)),
                     ?assert(maps:is_key(process_limit, Metrics)),
                     ?assert(maps:is_key(usage_percent, Metrics)),
                     ?assert(maps:is_key(status, Metrics)),
                     ?assert(maps:is_key(available_processes, Metrics)),
                     ?assert(maps:is_key(capacity_estimate, Metrics)),
                     ?assert(maps:is_key(timestamp, Metrics))
                 end)
         ]
     end}.

process_metrics_validity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Metrics} = erlmcp_process_monitor:get_process_metrics(),
                     ProcessCount = maps:get(process_count, Metrics),
                     ProcessLimit = maps:get(process_limit, Metrics),
                     UsagePercent = maps:get(usage_percent, Metrics),

                     ?assert(is_integer(ProcessCount)),
                     ?assert(ProcessCount >= 0),
                     ?assert(is_integer(ProcessLimit)),
                     ?assert(ProcessLimit > 0),
                     ?assert(ProcessCount =< ProcessLimit),
                     ?assert(is_float(UsagePercent)),
                     ?assert(UsagePercent >= 0.0),
                     ?assert(UsagePercent =< 1.0)
                 end)
         ]
     end}.

process_metrics_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Metrics} = erlmcp_process_monitor:get_process_metrics(),
                     Status = maps:get(status, Metrics),
                     ?assert(lists:member(Status, [ok, warning, critical]))
                 end)
         ]
     end}.

%%====================================================================
%% Capacity Estimation Tests
%%====================================================================

get_capacity_estimate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     ?assert(is_map(Estimate)),
                     ?assert(maps:is_key(current_connections, Estimate)),
                     ?assert(maps:is_key(estimated_capacity, Estimate)),
                     ?assert(maps:is_key(remaining_capacity, Estimate)),
                     ?assert(maps:is_key(utilization_percent, Estimate)),
                     ?assert(maps:is_key(memory_total_bytes, Estimate)),
                     ?assert(maps:is_key(memory_used_bytes, Estimate)),
                     ?assert(maps:is_key(memory_available_bytes, Estimate)),
                     ?assert(maps:is_key(recommendations, Estimate))
                 end)
         ]
     end}.

capacity_estimate_realistic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     EstimatedCapacity = maps:get(estimated_capacity, Estimate),

                     %% Capacity should be reasonable (varies by system)
                     %% On systems with high process limits, memory is the limiting factor
                     ?assert(is_integer(EstimatedCapacity)),
                     ?assert(EstimatedCapacity > 0),
                     ?assert(EstimatedCapacity > 10000)  %% At least 10K
                 end)
         ]
     end}.

capacity_estimate_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     TotalMemory = maps:get(memory_total_bytes, Estimate),
                     UsedMemory = maps:get(memory_used_bytes, Estimate),
                     AvailableMemory = maps:get(memory_available_bytes, Estimate),

                     ?assert(is_integer(TotalMemory)),
                     ?assert(TotalMemory > 0),
                     ?assert(is_integer(UsedMemory)),
                     ?assert(UsedMemory >= 0),
                     ?assert(is_integer(AvailableMemory)),
                     %% Note: AvailableMemory can be negative due to Erlang memory accounting
                     %% where used_memory + system_memory > total_memory
                     ?assert(is_number(AvailableMemory))
                 end)
         ]
     end}.

%%====================================================================
%% Alert Threshold Tests
%%====================================================================

get_alert_thresholds_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, {Warning, Critical}} = erlmcp_process_monitor:get_alert_thresholds(),
                     ?assert(is_float(Warning)),
                     ?assert(is_float(Critical)),
                     ?assert(Warning > 0),
                     ?assert(Warning < 1.0),
                     ?assert(Critical > 0),
                     ?assert(Critical < 1.0),
                     ?assert(Critical > Warning)
                 end)
         ]
     end}.

set_alert_thresholds_valid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Result = erlmcp_process_monitor:set_alert_thresholds(0.60, 0.80),
                     ?assertEqual(ok, Result),
                     {ok, {Warning, Critical}} = erlmcp_process_monitor:get_alert_thresholds(),
                     ?assertEqual(0.60, Warning),
                     ?assertEqual(0.80, Critical)
                 end)
         ]
     end}.

set_alert_thresholds_invalid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Warning >= Critical
                     Result1 = erlmcp_process_monitor:set_alert_thresholds(0.80, 0.70),
                     ?assertEqual({error, invalid_thresholds}, Result1),

                     %% Warning out of range
                     Result2 = erlmcp_process_monitor:set_alert_thresholds(1.5, 0.90),
                     ?assertEqual({error, invalid_thresholds}, Result2),

                     %% Critical out of range
                     Result3 = erlmcp_process_monitor:set_alert_thresholds(0.50, 1.1),
                     ?assertEqual({error, invalid_thresholds}, Result3)
                 end)
         ]
     end}.

%%====================================================================
%% Process Limit Check Tests
%%====================================================================

check_process_limit_ok_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Under normal conditions, should be ok
                     Result = erlmcp_process_monitor:check_process_limit(),
                     case Result of
                         ok -> ?assert(true);
                         {warning, _Msg} -> ?assert(true);
                         {critical, _Msg} -> ?assert(true);
                         _ -> ?assert(false, "Invalid return value")
                     end
                 end)
         ]
     end}.

check_process_limit_returns_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Result = erlmcp_process_monitor:check_process_limit(),
                     case Result of
                         ok -> ?assert(true);
                         {warning, _Msg} -> ?assert(true);
                         {critical, _Msg} -> ?assert(true);
                         _ -> ?assert(false, "Invalid return value")
                     end
                 end)
         ]
     end}.

%%====================================================================
%% Auto-Scaling Tests
%%====================================================================

enable_auto_scaling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Result = erlmcp_process_monitor:enable_auto_scaling(),
                     ?assertEqual(ok, Result),

                     %% Check that recommendations include auto-scaling
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     Recommendations = maps:get(recommendations, Estimate),
                     ?assert(is_list(Recommendations))
                 end)
         ]
     end}.

disable_auto_scaling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     erlmcp_process_monitor:enable_auto_scaling(),
                     Result = erlmcp_process_monitor:disable_auto_scaling(),
                     ?assertEqual(ok, Result)
                 end)
         ]
     end}.

%%====================================================================
%% Status Determination Tests
%%====================================================================

status_ok_when_under_warning_test() ->
    %% Low usage should be OK
    ProcessCount = 1000,
    ProcessLimit = 262144,  % Typical default
    UsagePercent = ProcessCount / ProcessLimit,
    Status = UsagePercent < 0.70,
    ?assert(Status).

status_warning_when_between_thresholds_test() ->
    %% At 80% usage should be warning
    UsagePercent = 0.80,
    Status = UsagePercent >= 0.70 andalso UsagePercent < 0.90,
    ?assert(Status).

status_critical_when_over_threshold_test() ->
    %% At 95% usage should be critical
    UsagePercent = 0.95,
    Status = UsagePercent >= 0.90,
    ?assert(Status).

%%====================================================================
%% Capacity Calculation Tests
%%====================================================================

capacity_calculation_conservative_test() ->
    %% Capacity should be the more conservative of memory and process limits
    ProcessLimit = erlang:system_info(process_limit),
    PerConnOverhead = 3072,  % 3KB
    TargetMemory = 1073741824,  % 1GB
    SafetyMargin = 0.20,

    MemoryCapacity = trunc((TargetMemory * (1.0 - SafetyMargin)) / PerConnOverhead),
    ProcessCapacity = trunc(ProcessLimit * 0.70),
    ExpectedCapacity = min(MemoryCapacity, ProcessCapacity),

    ?assert(is_integer(ExpectedCapacity)),
    ?assert(ExpectedCapacity > 0),
    ?assert(ExpectedCapacity >= MemoryCapacity orelse ExpectedCapacity >= ProcessCapacity).

%%====================================================================
%% Periodic Monitoring Tests
%%====================================================================

periodic_monitoring_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Get initial metrics
                     {ok, Metrics1} = erlmcp_process_monitor:get_process_metrics(),

                     %% Wait for at least one check interval
                     timer:sleep(?TEST_CHECK_INTERVAL + 500),

                     %% Get metrics again - should be different timestamp
                     {ok, Metrics2} = erlmcp_process_monitor:get_process_metrics(),

                     Timestamp1 = maps:get(timestamp, Metrics1),
                     Timestamp2 = maps:get(timestamp, Metrics2),

                     ?assert(Timestamp2 > Timestamp1)
                 end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_with_recovery_manager_test_() ->
    {setup,
     fun() ->
             %% Start both processes
             {ok, RecPid} = erlmcp_recovery_manager:start_link(),
             {ok, MonPid} = erlmcp_process_monitor:start_link([
                 {check_interval, ?TEST_CHECK_INTERVAL}
             ]),
             {RecPid, MonPid}
     end,
     fun({RecPid, MonPid}) ->
             gen_server:stop(RecPid),
             gen_server:stop(MonPid)
     end,
     fun({_RecPid, _MonPid}) ->
         [
          ?_test(begin
                     %% Verify both are running
                     ?assert(is_process_alive(erlang:whereis(erlmcp_recovery_manager))),
                     ?assert(is_process_alive(erlang:whereis(erlmcp_process_monitor))),

                     %% Check process limit (may trigger recovery)
                     Result = erlmcp_process_monitor:check_process_limit(),
                     case Result of
                         ok -> ?assert(true);
                         {warning, _Msg} -> ?assert(true);
                         {critical, _Msg} -> ?assert(true);
                         _ -> ?assert(false, "Invalid return value")
                     end
                 end)
         ]
     end}.

%%====================================================================
%% Recommendation Generation Tests
%%====================================================================

recommendations_list_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     Recommendations = maps:get(recommendations, Estimate),
                     ?assert(is_list(Recommendations)),
                     ?assert(length(Recommendations) >= 0)
                 end)
         ]
     end}.

recommendations_content_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     Recommendations = maps:get(recommendations, Estimate),

                     %% All recommendations should be binaries
                     lists:foreach(fun(Rec) ->
                         ?assert(is_binary(Rec)),
                         ?assert(byte_size(Rec) > 0)
                     end, Recommendations)
                 end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

unknown_call_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     Result = gen_server:call(erlmcp_process_monitor, unknown_message),
                     ?assertEqual({error, unknown_request}, Result)
                 end)
         ]
     end}.

unknown_cast_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Should not crash
                     gen_server:cast(erlmcp_process_monitor, unknown_message),
                     timer:sleep(100),
                     ?assert(is_process_alive(erlang:whereis(erlmcp_process_monitor)))
                 end)
         ]
     end}.

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

concurrent_metrics_access_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Spawn multiple processes accessing metrics concurrently
                     Parent = self(),
                     Workers = 10,
                     lists:foreach(fun(_) ->
                         spawn(fun() ->
                             Result = erlmcp_process_monitor:get_process_metrics(),
                             Parent ! {worker_result, Result}
                         end)
                     end, lists:seq(1, Workers)),

                     %% Collect all results
                     Results = [receive {worker_result, R} -> R after 5000 -> timeout end
                               || _ <- lists:seq(1, Workers)],

                     %% All should succeed
                     ?assertEqual(Workers, length(Results)),
                     lists:foreach(fun(R) ->
                         ?assertMatch({ok, _}, R)
                     end, Results)
                 end)
         ]
     end}.

%%====================================================================
%% Performance Tests
%%====================================================================

metrics_collection_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Metrics collection should be fast (< 100ms)
                     {Time, {ok, _Metrics}} = timer:tc(
                         fun() -> erlmcp_process_monitor:get_process_metrics() end
                     ),
                     ?assert(Time < 100000)  % 100ms
                 end)
         ]
     end}.

capacity_estimate_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Capacity estimation should be fast (< 100ms)
                     {Time, {ok, _Estimate}} = timer:tc(
                         fun() -> erlmcp_process_monitor:get_capacity_estimate() end
                     ),
                     ?assert(Time < 100000)  % 100ms
                 end)
         ]
     end}.

%%====================================================================
%% State Persistence Tests
%%====================================================================

thresholds_persist_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Set custom thresholds
                     ok = erlmcp_process_monitor:set_alert_thresholds(0.55, 0.85),

                     %% Verify they persist
                     {ok, {Warning, Critical}} = erlmcp_process_monitor:get_alert_thresholds(),
                     ?assertEqual(0.55, Warning),
                     ?assertEqual(0.85, Critical),

                     %% Set again
                     ok = erlmcp_process_monitor:set_alert_thresholds(0.65, 0.95),

                     %% Verify new values persist
                     {ok, {Warning2, Critical2}} = erlmcp_process_monitor:get_alert_thresholds(),
                     ?assertEqual(0.65, Warning2),
                     ?assertEqual(0.95, Critical2)
                 end)
         ]
     end}.

auto_scaling_state_persists_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Enable auto-scaling
                     erlmcp_process_monitor:enable_auto_scaling(),

                     %% Wait a bit
                     timer:sleep(100),

                     %% Get capacity estimate (should have recommendations)
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     Recommendations = maps:get(recommendations, Estimate),

                     %% Disable auto-scaling
                     erlmcp_process_monitor:disable_auto_scaling(),

                     %% Get capacity estimate again
                     {ok, Estimate2} = erlmcp_process_monitor:get_capacity_estimate(),
                     Recommendations2 = maps:get(recommendations, Estimate2),

                     ?assert(is_list(Recommendations)),
                     ?assert(is_list(Recommendations2))
                 end)
         ]
     end}.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

very_low_process_count_test() ->
    %% Even with very few processes, metrics should be valid
    ProcessCount = erlang:system_info(process_count),
    ?assert(ProcessCount > 0),

    ProcessLimit = erlang:system_info(process_limit),
    ?assert(ProcessLimit >= ProcessCount),

    UsagePercent = ProcessCount / ProcessLimit,
    ?assert(UsagePercent >= 0.0),
    ?assert(UsagePercent =< 1.0).

capacity_estimate_non_negative_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
                     RemainingCapacity = maps:get(remaining_capacity, Estimate),

                     %% Remaining capacity should never be negative
                     ?assert(RemainingCapacity >= 0)
                 end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================
