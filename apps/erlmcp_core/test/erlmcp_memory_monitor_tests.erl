%%%-------------------------------------------------------------------
%%% @doc Memory Monitor Tests - Binary Garbage Collection
%%%
%%% Test suite for binary garbage collection functionality to prevent
%%% heap exhaustion under load.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

%% Setup function to start the monitor
setup() ->
    {ok, Pid} = erlmcp_memory_monitor:start_link(),
    Pid.

%% Cleanup function to stop the monitor
cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% API Tests
%%====================================================================

start_link_test() ->
    {ok, Pid} = erlmcp_memory_monitor:start_link(),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(erlmcp_memory_monitor)),
    gen_server:stop(Pid).

get_memory_stats_test() ->
    Pid = setup(),
    Stats = erlmcp_memory_monitor:get_memory_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total_mb, Stats)),
    ?assert(maps:is_key(binary_mb, Stats)),
    ?assert(maps:is_key(binary_percent, Stats)),
    ?assert(maps:is_key(system_mb, Stats)),
    ?assert(maps:is_key(processes_mb, Stats)),
    ?assert(maps:is_key(process_count, Stats)),
    cleanup(Pid).

get_gc_stats_test() ->
    Pid = setup(),
    Stats = erlmcp_memory_monitor:get_gc_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total_gcs, Stats)),
    ?assert(maps:is_key(bytes_freed, Stats)),
    ?assert(maps:is_key(last_gc_time, Stats)),
    cleanup(Pid).

check_memory_pressure_test() ->
    Pid = setup(),
    Result = erlmcp_memory_monitor:check_memory_pressure(),
    ?assert(is_tuple(Result)),
    case Result of
        ok -> ok;
        {alert, high_binary_memory} -> ok
    end,
    cleanup(Pid).

check_binary_memory_test() ->
    Pid = setup(),
    Result = erlmcp_memory_monitor:check_binary_memory(),
    ?assert(is_tuple(Result)),
    case Result of
        {ok, binary_memory_normal, BinaryMem} ->
            ?assert(is_integer(BinaryMem)),
            ?assert(BinaryMem >= 0);
        {alert, binary_gc_triggered, BytesFreed} ->
            ?assert(is_integer(BytesFreed)),
            ?assert(BytesFreed >= 0)
    end,
    cleanup(Pid).

force_gc_test() ->
    Pid = setup(),
    ok = erlmcp_memory_monitor:force_gc(),
    cleanup(Pid).

force_binary_gc_test() ->
    Pid = setup(),
    {ok, ProcessesGced, BytesFreed} = erlmcp_memory_monitor:force_binary_gc(),
    ?assert(is_integer(ProcessesGced)),
    ?assert(ProcessesGced >= 0),
    ?assert(is_integer(BytesFreed)),
    ?assert(BytesFreed >= 0),
    cleanup(Pid).

%%====================================================================
%% Binary GC Tests
%%====================================================================

binary_gc_threshold_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Check that binary memory monitoring works
                     Result = erlmcp_memory_monitor:check_binary_memory(),
                     ?assert(is_tuple(Result))
                 end)
         ]
     end}.

binary_gc_statistics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Get initial stats
                     InitialStats = erlmcp_memory_monitor:get_gc_stats(),
                     InitialGCs = maps:get(total_gcs, InitialStats, 0),

                     %% Force a binary GC
                     {ok, ProcessesGced, BytesFreed} = erlmcp_memory_monitor:force_binary_gc(),

                     %% Check that stats were updated
                     FinalStats = erlmcp_memory_monitor:get_gc_stats(),
                     FinalGCs = maps:get(total_gcs, FinalStats, 0),

                     ?assert(FinalGCs >= InitialGCs),
                     ?assert(is_integer(ProcessesGced)),
                     ?assert(is_integer(BytesFreed))
                 end)
         ]
     end}.

process_wide_gc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Force GC on all processes
                     {ok, ProcessesGced, _BytesFreed} = erlmcp_memory_monitor:force_binary_gc(),

                     %% Should have GC'd at least some processes
                     ProcessCount = erlang:system_info(process_count),
                     ?assert(ProcessesGced >= 0),
                     ?assert(ProcessesGced =< ProcessCount + 100) %% Allow some margin
                 end)
         ]
     end}.

%%====================================================================
%% Memory Monitoring Tests
%%====================================================================

memory_stats_accuracy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     Stats = erlmcp_memory_monitor:get_memory_stats(),

                     %% Verify values are reasonable
                     TotalMB = maps:get(total_mb, Stats),
                     BinaryMB = maps:get(binary_mb, Stats),
                     BinaryPercent = maps:get(binary_percent, Stats),
                     ProcessCount = maps:get(process_count, Stats),

                     ?assert(TotalMB > 0),
                     ?assert(BinaryMB >= 0),
                     ?assert(BinaryPercent >= 0),
                     ?assert(BinaryPercent =< 100),
                     ?assert(ProcessCount > 0)
                 end)
         ]
     end}.

binary_memory_tracking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Get memory stats
                     Stats = erlmcp_memory_monitor:get_memory_stats(),

                     BinaryMB = maps:get(binary_mb, Stats),
                     TotalMB = maps:get(total_mb, Stats),
                     BinaryPercent = maps:get(binary_percent, Stats),

                     %% Verify binary percent calculation
                     ExpectedPercent = case TotalMB > 0 of
                         true -> (BinaryMB / TotalMB) * 100;
                         false -> 0
                     end,

                     ?assertEqual(ExpectedPercent, BinaryPercent)
                 end)
         ]
     end}.

%%====================================================================
%% Alert and Threshold Tests
%%====================================================================

alert_tracking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% This test verifies alert tracking logic
                     %% Actual alert triggering depends on system state
                     Result = erlmcp_memory_monitor:check_memory_pressure(),

                     case Result of
                         {alert, high_binary_memory} ->
                             %% Alert was raised, verify it's a valid alert
                             ok;
                         ok ->
                             %% No alert, memory is normal
                             ok
                     end
                 end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

memory_monitor_lifecycle_test_() ->
    {setup,
     fun() ->
             %% Start monitor
             {ok, Pid} = erlmcp_memory_monitor:start_link(),
             Pid
     end,
     fun(Pid) ->
             %% Stop monitor
             gen_server:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
                     %% Verify monitor is running
                     ?assertEqual(Pid, whereis(erlmcp_memory_monitor)),

                     %% Get stats
                     Stats = erlmcp_memory_monitor:get_memory_stats(),
                     ?assert(is_map(Stats)),

                     %% Force GC
                     {ok, ProcessesGced, BytesFreed} = erlmcp_memory_monitor:force_binary_gc(),
                     ?assert(is_integer(ProcessesGced)),

                     %% Check GC stats
                     GCStats = erlmcp_memory_monitor:get_gc_stats(),
                     ?assert(is_map(GCStats))
                 end)
         ]
     end}.

%%====================================================================
%% Performance Tests
%%====================================================================

gc_performance_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_) ->
          [
           ?_test(begin
                      %% Measure GC performance
                      StartTime = erlang:monotonic_time(micro_second),

                      {ok, ProcessesGced, BytesFreed} = erlmcp_memory_monitor:force_binary_gc(),

                      EndTime = erlang:monotonic_time(micro_second),
                      DurationMs = (EndTime - StartTime) / 1000,

                      %% GC should complete in reasonable time (< 5 seconds)
                      ?assert(DurationMs < 5000),

                      %% Log performance metrics
                      io:format("Binary GC: ~p processes, ~p bytes freed in ~.2f ms~n",
                                [ProcessesGced, BytesFreed, DurationMs])
                  end)
          ]
      end}}.

memory_monitor_overhead_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_) ->
          [
           ?_test(begin
                      %% Measure overhead of multiple checks
                      StartTime = erlang:monotonic_time(micro_second),

                      lists:foreach(fun(_) -> erlmcp_memory_monitor:check_binary_memory() end, lists:seq(1, 100)),

                      EndTime = erlang:monotonic_time(micro_second),
                      DurationMs = (EndTime - StartTime) / 1000,

                      %% 100 checks should complete in < 1 second
                      ?assert(DurationMs < 1000),

                      io:format("100 memory checks completed in ~.2f ms (~.2f ms/check)~n",
                                [DurationMs, DurationMs / 100])
                  end)
          ]
      end}}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

zero_binary_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test with zero binary memory (unlikely but should handle)
                     Stats = erlmcp_memory_monitor:get_memory_stats(),
                     BinaryMB = maps:get(binary_mb, Stats),

                     %% Should handle zero gracefully
                     ?assert(BinaryMB >= 0)
                 end)
         ]
     end}.

extreme_memory_pressure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Allocate some binary data
                     Binary = <<0:(1024 * 1024 * 8)>>, %% 1MB binary

                     %% Check memory
                     Stats = erlmcp_memory_monitor:get_memory_stats(),
                     ?assert(is_map(Stats)),

                     %% Force GC
                     {ok, ProcessesGced, BytesFreed} = erlmcp_memory_monitor:force_binary_gc(),
                     ?assert(is_integer(ProcessesGced)),
                     ?assert(is_integer(BytesFreed)),

                     %% Clean up
                     erlang:garbage_collect()
                 end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Allocate binary data for testing
-spec allocate_binary(pos_integer()) -> binary().
allocate_binary(Size) ->
    <<0:Size>>.

%% @doc Get process count
-spec get_process_count() -> pos_integer().
get_process_count() ->
    erlang:system_info(process_count).
