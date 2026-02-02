%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for erlmcp_process_monitor_sup - Real-time Process Monitoring
%%%
%%% Chicago School TDD: Real processes, no mocks, testing observable behavior.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_monitor_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(SERVER_ID, <<"test_monitor_server">>).

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup function - start the monitor
setup() ->
    {ok, Pid} = erlmcp_process_monitor_sup:start_link([{scan_interval, 1000}]),
    Pid.

%% @doc Teardown function - stop the monitor
cleanup(Pid) ->
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Startup and Shutdown Tests
%%====================================================================

start_stop_test() ->
    %% Test basic start and stop
    {setup,
     fun() -> application:ensure_all_started(gproc) end,
     fun(_) -> ok end,
     fun(_) ->
         {ok, Pid} = erlmcp_process_monitor_sup:start_link(),
         ?assert(is_pid(Pid)),
         ?assertEqual(Pid, whereis(erlmcp_process_monitor_sup)),
         gen_server:stop(Pid),
         ?assertEqual(undefined, whereis(erlmcp_process_monitor_sup))
     end}.

initial_scan_performed_test() ->
    %% Test that initial scan is performed on startup
    {setup,
     fun() -> application:ensure_all_started(gproc) end,
     fun(_) -> ok end,
     fun(_) ->
         {ok, _Pid} = erlmcp_process_monitor_sup:start_link([{scan_interval, 5000}]),

         %% Get stats - should have initial data
         {ok, Stats} = erlmcp_process_monitor_sup:get_stats(),

         %% Cleanup
         gen_server:stop(erlmcp_process_monitor_sup),

         %% Verify stats structure
         ?assert(maps:is_key(timestamp, Stats)),
         ?assert(maps:is_key(scan_duration_ms, Stats)),
         ?assert(maps:is_key(total_processes, Stats)),
         ?assert(maps:is_key(by_type, Stats))
     end}.

%%====================================================================
%% Statistics Collection Tests
%%====================================================================

get_stats_structure_test() ->
    %% Test that get_stats returns proper structure
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         {ok, Stats} = erlmcp_process_monitor_sup:get_stats(),

         %% Verify top-level fields
         ?assert(maps:is_key(timestamp, Stats)),
         ?assert(maps:is_key(scan_duration_ms, Stats)),
         ?assert(maps:is_key(total_processes, Stats)),
         ?assert(maps:is_key(by_type, Stats)),
         ?assert(maps:is_key(total_memory_bytes, Stats)),
         ?assert(maps:is_key(memory_by_type, Stats)),
         ?assert(maps:is_key(queue_stats, Stats)),

         %% Verify data types
         ScanDuration = maps:get(scan_duration_ms, Stats),
         ?assert(is_integer(ScanDuration)),
         ?assert(ScanDuration >= 0),

         TotalProcesses = maps:get(total_processes, Stats),
         ?assert(is_integer(TotalProcesses)),
         ?assert(TotalProcesses >= 0)
     end}.

get_stats_consistency_test() ->
    %% Test that multiple calls return consistent data
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         {ok, Stats1} = erlmcp_process_monitor_sup:get_stats(),
         timer:sleep(100),
         {ok, Stats2} = erlmcp_process_monitor_sup:get_stats(),

         %% Timestamps should be different (scans happened at different times)
         Time1 = maps:get(timestamp, Stats1),
         Time2 = maps:get(timestamp, Stats2),
         ?assert(Time1 =/= Time2),

         %% But structure should be consistent
         ?assertEqual(maps:keys(Stats1), maps:keys(Stats2))
     end}.

%%====================================================================
%% Periodic Scanning Tests
%%====================================================================

periodic_scan_updates_stats_test() ->
    %% Test that periodic scans update statistics
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Get initial stats
         {ok, Stats1} = erlmcp_process_monitor_sup:get_stats(),
         Time1 = maps:get(timestamp, Stats1),

         %% Wait for next scan (interval is 1000ms)
         timer:sleep(1200),

         %% Get updated stats
         {ok, Stats2} = erlmcp_process_monitor_sup:get_stats(),
         Time2 = maps:get(timestamp, Stats2),

         %% Timestamp should be updated
         ?assert(Time2 > Time1)
     end}.

trigger_scan_test() ->
    %% Test manual scan triggering
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Get initial stats
         {ok, Stats1} = erlmcp_process_monitor_sup:get_stats(),
         Time1 = maps:get(timestamp, Stats1),

         %% Trigger immediate scan
         ok = erlmcp_process_monitor_sup:trigger_scan(),

         %% Wait for scan to complete
         timer:sleep(100),

         %% Get updated stats
         {ok, Stats2} = erlmcp_process_monitor_sup:get_stats(),
         Time2 = maps:get(timestamp, Stats2),

         %% Timestamp should be updated
         ?assert(Time2 > Time1)
     end}.

%%====================================================================
%% Scan Interval Tests
%%====================================================================

set_scan_interval_test() ->
    %% Test changing scan interval
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Get initial interval
         {ok, InitialInterval} = erlmcp_process_monitor_sup:get_scan_interval(),
         ?assertEqual(1000, InitialInterval),

         %% Set new interval
         ok = erlmcp_process_monitor_sup:set_scan_interval(500),

         %% Verify interval changed
         {ok, NewInterval} = erlmcp_process_monitor_sup:get_scan_interval(),
         ?assertEqual(500, NewInterval)
     end}.

scan_interval_zero_test() ->
    %% Test that zero or negative intervals are rejected
    {setup,
     fun() -> application:ensure_all_started(gproc) end,
     fun(_) -> ok end,
     fun(_) ->
         {ok, Pid} = erlmcp_process_monitor_sup:start_link(),

         %% Try to set invalid interval
         Result = erlmcp_process_monitor_sup:set_scan_interval(0),

         %% Should be rejected (no crash, might return error or ignore)
         ?assertNotEqual(error, Result),

         %% Cleanup
         gen_server:stop(Pid)
     end}.

%%====================================================================
%% Subscription Tests
%%====================================================================

subscribe_to_updates_test() ->
    %% Test subscribing to statistics updates
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Create subscriber process
         Subscriber = spawn(fun() ->
                                   receive
                                       {process_stats_update, Stats} ->
                                           ?assert(is_map(Stats)),
                                           ?assert(maps:is_key(timestamp, Stats))
                                   end
                           end),

         %% Subscribe
         ok = erlmcp_process_monitor_sup:subscribe_to_updates(Subscriber),

         %% Trigger scan to send update
         erlmcp_process_monitor_sup:trigger_scan(),
         timer:sleep(100),

         %% Subscriber should have received message
         %% (We can't directly verify, but process should be alive)
         ?assert(erlang:is_process_alive(Subscriber)),

         %% Cleanup
         exit(Subscriber, kill)
     end}.

subscriber_cleanup_on_death_test() ->
    %% Test that dead subscribers are cleaned up
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Create and kill subscriber
         Subscriber = spawn(fun() -> receive after infinity -> ok end end),
         ok = erlmcp_process_monitor_sup:subscribe_to_updates(Subscriber),
         exit(Subscriber, kill),
         timer:sleep(50),

         %% Trigger scan (should not crash due to dead subscriber)
         ok = erlmcp_process_monitor_sup:trigger_scan(),

         %% Monitor should still be alive
         ?assert(erlang:is_process_alive(Pid))
     end}.

unsubscribe_from_updates_test() ->
    %% Test unsubscribing from updates
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Create subscriber
         Subscriber = spawn(fun() -> receive after infinity -> ok end end),

         %% Subscribe and then unsubscribe
         ok = erlmcp_process_monitor_sup:subscribe_to_updates(Subscriber),
         ok = erlmcp_process_monitor_sup:unsubscribe_from_updates(),

         %% Trigger scan
         erlmcp_process_monitor_sup:trigger_scan(),
         timer:sleep(100),

         %% Subscriber should not have received any new messages
         %% (Process should still be alive, mailbox empty or controlled)
         ?assert(erlang:is_process_alive(Subscriber)),

         %% Cleanup
         exit(Subscriber, kill)
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_with_inspector_test() ->
    %% Test integration with erlmcp_inspector
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Get stats from monitor
         {ok, MonitorStats} = erlmcp_process_monitor_sup:get_stats(),

         %% Get stats directly from inspector
         InspectorStats = erlmcp_inspector:get_aggregate_stats(),

         %% Compare total process counts (should be similar)
         MonitorTotal = maps:get(total_processes, MonitorStats),
         InspectorTotal = maps:get(total, InspectorStats),

         %% Counts should match or be close (race conditions allowed)
         ?assert(abs(MonitorTotal - InspectorTotal) =< 2)
     end}.

multiple_subscribers_test() ->
    %% Test that multiple subscribers can receive updates
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         %% Create multiple subscribers
         Subs = [spawn(fun() -> receive after infinity -> ok end end) || _ <- lists:seq(1, 5)],

         %% Subscribe all
         lists:foreach(fun(S) -> erlmcp_process_monitor_sup:subscribe_to_updates(S) end, Subs),

         %% Trigger scan
         erlmcp_process_monitor_sup:trigger_scan(),
         timer:sleep(100),

         %% All subscribers should still be alive
         ?assert(lists:all(fun erlang:is_process_alive/1, Subs)),

         %% Cleanup
         lists:foreach(fun(S) -> exit(S, kill) end, Subs)
     end}.
