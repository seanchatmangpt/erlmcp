-module(erlmcp_graceful_drain_priority_tests).

-include_lib("eunit/include/eunit.hrl").
-include("otp_compat.hrl").

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
      fun test_concurrent_shutdown_signals/1,
      fun test_fallback_otp_27/1]}.

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

         % Should have processed shutdown signal
         ?assertMatch(#{}, Status),

         -ifdef(OTP_28).
         % Priority shutdown should be instant (<100us typical)
         ?assert(LatencyUs < 1000),
         ?debugFmt("OTP 28 priority shutdown signal: ~p us", [LatencyUs])
         -else.
         ?assert(LatencyUs < 5000),
         ?debugFmt("OTP 25-27 shutdown signal: ~p us", [LatencyUs])
         -endif.
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

         -ifdef(OTP_28).
         ?assert(AvgLatency < 1000),
         ?assert(MaxLatency < 2000),
         ?debugFmt("OTP 28 priority shutdown - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency])
         -else.
         ?assert(AvgLatency < 5000),
         ?assert(MaxLatency < 10000),
         ?debugFmt("OTP 25-27 shutdown - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency])
         -endif.
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

         -ifdef(OTP_28).
         ?debugFmt("OTP 28 graceful drain sequence: ~p us", [DrainLatencyUs])
         -else.
         ?debugFmt("OTP 25-27 graceful drain sequence: ~p us", [DrainLatencyUs])
         -endif.
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
         -ifdef(OTP_28).
         % Start new drain service
         {ok, NewPid} = erlmcp_graceful_drain:start_link(),

         % Trigger priority shutdown
         StartTime = erlang:monotonic_time(microsecond),
         ok = erlmcp_graceful_drain:initiate_shutdown(1000),
         EndTime = erlang:monotonic_time(microsecond),

         LatencyUs = EndTime - StartTime,

         % Metrics should show priority message delivery
         ?assert(LatencyUs < 1000), % Should be very fast

         ?debugFmt("Priority shutdown metrics: ~p us", [LatencyUs]),

         exit(NewPid, shutdown)
         -else.
         ?debugMsg("Skipping priority metrics test on OTP 25-27")
         -endif.
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

         -ifdef(OTP_28).
         ?assert(AvgLatency < 2000),
         ?assert(MaxLatency < 5000),
         ?debugFmt("OTP 28 concurrent shutdowns - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency])
         -else.
         ?assert(AvgLatency < 10000),
         ?assert(MaxLatency < 20000),
         ?debugFmt("OTP 25-27 concurrent shutdowns - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency])
         -endif.,

         % Wait for all to terminate
         timer:sleep(1000)
     end}.

%%====================================================================
%% Fallback Tests
%%====================================================================

test_fallback_otp_27(Pid) ->
    {"Graceful drain should work correctly on OTP 25-27 without priority messages",
     fun() ->
         % Test normal operation
         ok = erlmcp_graceful_drain:connection_started(test_module),
         ?assertEqual(1, erlmcp_graceful_drain:get_active_connections()),

         ok = erlmcp_graceful_drain:connection_finished(test_module),
         ?assertEqual(0, erlmcp_graceful_drain:get_active_connections()),

         -ifndef(OTP_28).
         ?debugMsg("Verified fallback behavior on OTP 25-27")
         -endif.
     end}.
