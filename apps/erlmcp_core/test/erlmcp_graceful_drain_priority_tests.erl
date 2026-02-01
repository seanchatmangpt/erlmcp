-module(erlmcp_graceful_drain_priority_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

graceful_drain_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_priority_shutdown_signal/1,
      fun test_priority_shutdown_latency/1,
      fun test_graceful_drain_sequence/1,
      fun test_connection_tracking/1,
      fun test_shutdown_with_active_connections/1,
      fun test_shutdown_timeout/1,
      fun test_priority_metrics_tracking/1,
      fun test_concurrent_shutdown_signals/1]}.

setup() ->
    {ok, Pid} = erlmcp_graceful_drain:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            exit(Pid, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.

%%====================================================================
%% Priority Shutdown Tests
%%====================================================================

test_priority_shutdown_signal(Pid) ->
    {"Priority shutdown signal should be processed immediately",
     fun() ->
         StartTime = erlang:monotonic_time(microsecond),

         % Initiate priority shutdown
         ok = erlmcp_graceful_drain:initiate_shutdown(5000),

         % Verify drain status
         timer:sleep(10), % Small delay for processing
         Status = erlmcp_graceful_drain:get_drain_status(),

         EndTime = erlang:monotonic_time(microsecond),
         LatencyUs = EndTime - StartTime,

         % Should have processed shutdown signal quickly
         ?assert(LatencyUs < 100000), % Less than 100ms

         ?debugFmt("OTP 28 priority shutdown signal latency: ~p us", [LatencyUs])
     end}.

test_priority_shutdown_latency(Pid) ->
    {"Priority shutdown should have minimal latency",
     fun() ->
         % Measure multiple shutdown initiations
         Latencies = lists:map(fun(N) ->
             % Restart drain service for each test
             exit(Pid, shutdown),
             timer:sleep(20),
             {ok, NewPid} = erlmcp_graceful_drain:start_link(),

             StartTime = erlang:monotonic_time(microsecond),
             ok = erlmcp_graceful_drain:initiate_shutdown(1000),
             EndTime = erlang:monotonic_time(microsecond),

             exit(NewPid, shutdown),
             EndTime - StartTime
         end, lists:seq(1, 5)),

         AvgLatency = lists:sum(Latencies) / length(Latencies),
         MaxLatency = lists:max(Latencies),

         ?assert(AvgLatency < 1000),
         ?assert(MaxLatency < 2000),
         ?debugFmt("OTP 28 priority shutdown - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency])
     end}.

%%====================================================================
%% Graceful Drain Sequence Tests
%%====================================================================

test_graceful_drain_sequence(Pid) ->
    {"Graceful drain should follow priority signals -> drain -> shutdown sequence",
     fun() ->
         % Simulate active connections
         ok = erlmcp_graceful_drain:connection_started(test_module),
         ok = erlmcp_graceful_drain:connection_started(test_module),
         ok = erlmcp_graceful_drain:connection_started(test_module),

         ActiveBefore = erlmcp_graceful_drain:get_active_connections(),
         ?assertEqual(3, ActiveBefore),

         % Initiate shutdown
         StartTime = erlang:monotonic_time(microsecond),
         ok = erlmcp_graceful_drain:initiate_shutdown(2000),

         % New connections should be rejected
         ok = erlmcp_graceful_drain:connection_started(test_module),
         ActiveAfterShutdown = erlmcp_graceful_drain:get_active_connections(),

         % Should still be 3 (new connection rejected)
         ?assertEqual(3, ActiveAfterShutdown),

         % Finish existing connections
         ok = erlmcp_graceful_drain:connection_finished(test_module),
         ok = erlmcp_graceful_drain:connection_finished(test_module),
         ok = erlmcp_graceful_drain:connection_finished(test_module),

         timer:sleep(50), % Allow shutdown to complete

         EndTime = erlang:monotonic_time(microsecond),
         DrainLatencyUs = EndTime - StartTime,

         % Process should have terminated gracefully
         ?assertNot(is_process_alive(Pid)),

         ?debugFmt("OTP 28 graceful drain sequence: ~p us", [DrainLatencyUs])
     end}.

%%====================================================================
%% Connection Tracking Tests
%%====================================================================

test_connection_tracking(Pid) ->
    {"Connection tracking should accurately count active connections",
     fun() ->
         ?assertEqual(0, erlmcp_graceful_drain:get_active_connections()),

         % Start connections
         ok = erlmcp_graceful_drain:connection_started(module1),
         ?assertEqual(1, erlmcp_graceful_drain:get_active_connections()),

         ok = erlmcp_graceful_drain:connection_started(module2),
         ?assertEqual(2, erlmcp_graceful_drain:get_active_connections()),

         ok = erlmcp_graceful_drain:connection_started(module3),
         ?assertEqual(3, erlmcp_graceful_drain:get_active_connections()),

         % Finish connections
         ok = erlmcp_graceful_drain:connection_finished(module1),
         ?assertEqual(2, erlmcp_graceful_drain:get_active_connections()),

         ok = erlmcp_graceful_drain:connection_finished(module2),
         ?assertEqual(1, erlmcp_graceful_drain:get_active_connections()),

         ok = erlmcp_graceful_drain:connection_finished(module3),
         ?assertEqual(0, erlmcp_graceful_drain:get_active_connections())
     end}.

test_shutdown_with_active_connections(Pid) ->
    {"Shutdown should wait for active connections to drain",
     fun() ->
         % Start connections
         ok = erlmcp_graceful_drain:connection_started(test_module),
         ok = erlmcp_graceful_drain:connection_started(test_module),

         ?assertEqual(2, erlmcp_graceful_drain:get_active_connections()),

         % Initiate shutdown
         ok = erlmcp_graceful_drain:initiate_shutdown(3000),

         % Process should still be alive (waiting for drain)
         timer:sleep(100),
         ?assert(is_process_alive(Pid)),

         % Finish one connection
         ok = erlmcp_graceful_drain:connection_finished(test_module),
         timer:sleep(10),
         ?assert(is_process_alive(Pid)),
         ?assertEqual(1, erlmcp_graceful_drain:get_active_connections()),

         % Finish last connection
         ok = erlmcp_graceful_drain:connection_finished(test_module),
         timer:sleep(100),

         % Process should terminate after all connections drained
         ?assertNot(is_process_alive(Pid))
     end}.

%%====================================================================
%% Timeout Tests
%%====================================================================

test_shutdown_timeout(_Pid) ->
    {"Shutdown should force terminate after timeout",
     fun() ->
         {ok, NewPid} = erlmcp_graceful_drain:start_link(),

         % Start connections
         ok = erlmcp_graceful_drain:connection_started(test_module),
         ok = erlmcp_graceful_drain:connection_started(test_module),

         % Initiate shutdown with short timeout
         ok = erlmcp_graceful_drain:initiate_shutdown(500),

         % Wait for timeout
         timer:sleep(600),

         % Process should terminate even with active connections
         ?assertNot(is_process_alive(NewPid))
     end}.

%%====================================================================
%% Metrics Tests
%%====================================================================

test_priority_metrics_tracking(_Pid) ->
    {"Priority shutdown metrics should be tracked",
     fun() ->
         % Start a fresh drain service for testing metrics
         {ok, MetricsPid} = erlmcp_graceful_drain:start_link(),

         % Simulate connections for metrics
         ok = erlmcp_graceful_drain:connection_started(m1),
         ok = erlmcp_graceful_drain:connection_started(m2),
         ok = erlmcp_graceful_drain:connection_started(m3),

         % Verify connection count
         ActiveCount = erlmcp_graceful_drain:get_active_connections(),
         ?assertEqual(3, ActiveCount),

         % Initiate shutdown with metrics tracking
         StartTime = erlang:monotonic_time(microsecond),
         ok = erlmcp_graceful_drain:initiate_shutdown(2000),

         % Finish connections to trigger graceful shutdown
         ok = erlmcp_graceful_drain:connection_finished(m1),
         ok = erlmcp_graceful_drain:connection_finished(m2),
         ok = erlmcp_graceful_drain:connection_finished(m3),

         % Wait for shutdown
         timer:sleep(50),

         EndTime = erlang:monotonic_time(microsecond),
         ShutdownTimeUs = EndTime - StartTime,

         % Verify process terminated
         ?assertNot(is_process_alive(MetricsPid)),

         ?debugFmt("OTP 28 priority shutdown metrics: ~p us, ~p connections",
                  [ShutdownTimeUs, ActiveCount])
     end}.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_shutdown_signals(_Pid) ->
    {"Concurrent shutdown signals should be handled gracefully",
     fun() ->
         % Start multiple drain services
         Pids = lists:map(fun(_) ->
             {ok, P} = erlmcp_graceful_drain:start_link(),
             P
         end, lists:seq(1, 10)),

         % Send concurrent shutdown signals
         Parent = self(),
         Workers = [spawn_link(fun() ->
             StartTime = erlang:monotonic_time(microsecond),
             ok = erlmcp_graceful_drain:initiate_shutdown(500),
             EndTime = erlang:monotonic_time(microsecond),
             Parent ! {shutdown_latency, EndTime - StartTime}
         end) || _ <- Pids],

         % Collect latencies
         Latencies = [receive {shutdown_latency, L} -> L after 5000 -> error(timeout) end
                      || _ <- Workers],

         AvgLatency = lists:sum(Latencies) / length(Latencies),
         MaxLatency = lists:max(Latencies),

         ?assert(AvgLatency < 2000),
         ?assert(MaxLatency < 5000),
         ?debugFmt("OTP 28 concurrent shutdowns - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency]),

         % Wait for all to terminate
         timer:sleep(1000)
     end}.

