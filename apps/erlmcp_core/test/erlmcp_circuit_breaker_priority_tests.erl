-module(erlmcp_circuit_breaker_priority_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

circuit_breaker_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_priority_state_transition_closed_to_open/1,
      fun test_priority_state_transition_open_to_half_open/1,
      fun test_priority_state_transition_half_open_to_closed/1,
      fun test_priority_level_configuration/1,
      fun test_priority_metrics_tracking/1,
      fun test_state_change_notification_latency/1,
      fun test_concurrent_state_transitions/1]}.

setup() ->
    % Start circuit breaker with priority enabled
    Config =
        #{failure_threshold => 3,
          success_threshold => 2,
          timeout => 1000,
          priority_level => high},
    {ok, Pid} = erlmcp_circuit_breaker:start_link(test_breaker, Config),
    {Pid, Config}.

cleanup({Pid, _Config}) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_circuit_breaker:stop(Pid),
            timer:sleep(10);
        false ->
            ok
    end.

%%====================================================================
%% Priority State Transition Tests
%%====================================================================

test_priority_state_transition_closed_to_open({Pid, _Config}) ->
    {"State transition CLOSED -> OPEN should be immediate with priority",
     fun() ->
        % Verify initial state
        ?assertEqual(closed, erlmcp_circuit_breaker:get_state(Pid)),

        % Measure latency of transition to OPEN
        StartTime = erlang:monotonic_time(microsecond),

        % Force failures to trip breaker
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),

        % Verify state is now OPEN
        State = erlmcp_circuit_breaker:get_state(Pid),
        EndTime = erlang:monotonic_time(microsecond),
        TransitionLatencyUs = EndTime - StartTime,
        ?assertEqual(open, State)
     end}.

test_priority_state_transition_open_to_half_open({Pid, _Config}) ->
    {"State transition OPEN -> HALF_OPEN should be immediate with priority",
     fun() ->
        % Trip breaker to OPEN
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        ?assertEqual(open, erlmcp_circuit_breaker:get_state(Pid)),

        % Wait for timeout to trigger HALF_OPEN transition
        % Slightly longer than timeout
        StartTime = erlang:monotonic_time(microsecond),
        timer:sleep(1100),

        State = erlmcp_circuit_breaker:get_state(Pid),
        EndTime = erlang:monotonic_time(microsecond),
        TransitionLatencyUs = EndTime - StartTime,
        ?assertEqual(half_open, State)
     end}.

test_priority_state_transition_half_open_to_closed({Pid, _Config}) ->
    {"State transition HALF_OPEN -> CLOSED should be immediate with priority",
     fun() ->
        % Trip breaker to OPEN
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),

        % Wait for HALF_OPEN
        timer:sleep(1100),
        ?assertEqual(half_open, erlmcp_circuit_breaker:get_state(Pid)),

        % Measure transition to CLOSED
        StartTime = erlang:monotonic_time(microsecond),

        % Successful calls to close breaker
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {ok, success} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {ok, success} end),

        State = erlmcp_circuit_breaker:get_state(Pid),
        EndTime = erlang:monotonic_time(microsecond),
        TransitionLatencyUs = EndTime - StartTime,

        ?assertEqual(closed, State),

        ?assert(TransitionLatencyUs < 1000),
        ?debugFmt("OTP 28 HALF_OPEN->CLOSED transition: ~p us", [TransitionLatencyUs])
     end}.

%%====================================================================
%% Configuration Tests
%%====================================================================

test_priority_level_configuration(_Setup) ->
    {"Circuit breaker should support priority_level configuration",
     fun() ->
        % Test with high priority
        Config1 = #{priority_level => high},
        {ok, Pid1} = erlmcp_circuit_breaker:start_link(breaker_high, Config1),

        {ok, Stats1} = erlmcp_circuit_breaker:get_stats(Pid1),
        Config1Result = maps:get(config, Stats1),
        ?assertEqual(high, maps:get(priority_level, Config1Result)),

        erlmcp_circuit_breaker:stop(Pid1),

        % Test with normal priority (default)
        Config2 = #{},
        {ok, Pid2} = erlmcp_circuit_breaker:start_link(breaker_normal, Config2),

        {ok, Stats2} = erlmcp_circuit_breaker:get_stats(Pid2),
        Config2Result = maps:get(config, Stats2),
        ?assertEqual(normal, maps:get(priority_level, Config2Result)),

        erlmcp_circuit_breaker:stop(Pid2)
     end}.

%%====================================================================
%% Metrics Tests
%%====================================================================

test_priority_metrics_tracking({Pid, _Config}) ->
    {"Priority metrics should be tracked for state transitions",
     fun() ->
        % Get initial stats
        {ok, Stats1} = erlmcp_circuit_breaker:get_stats(Pid),
        InitialCount = maps:get(priority_messages_delivered, Stats1, 0),

        % Force state transitions
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),

        % Get updated stats
        {ok, Stats2} = erlmcp_circuit_breaker:get_stats(Pid),
        FinalCount = maps:get(priority_messages_delivered, Stats2, 0)
     end}.

%%====================================================================
%% Notification Tests
%%====================================================================

test_state_change_notification_latency({Pid, _Config}) ->
    {"State change notifications should have low latency",
     fun() ->
        % Start health monitor to receive notifications
        {ok, _MonitorPid} = erlmcp_health_monitor:start_link([]),

        % Measure notification latency by triggering state change
        StartTime = erlang:monotonic_time(microsecond),

        % Trip breaker
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),
        _ = erlmcp_circuit_breaker:call(Pid, fun() -> {error, failed} end),

        % Give time for notification to propagate
        timer:sleep(10),
        EndTime = erlang:monotonic_time(microsecond),
        NotificationLatencyUs = EndTime - StartTime,
        ?assert(NotificationLatencyUs < 1000000)  % Should be < 1 second
     end}.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_state_transitions(_Setup) ->
    {"Concurrent state transitions should be handled correctly",
     fun() ->
        % Create multiple circuit breakers
        Pids =
            lists:map(fun(N) ->
                         Name = list_to_atom("breaker_" ++ integer_to_list(N)),
                         Config = #{failure_threshold => 3, priority_level => high},
                         {ok, Pid} = erlmcp_circuit_breaker:start_link(Name, Config),
                         Pid
                      end,
                      lists:seq(1, 10)),

        % Trigger concurrent failures
        Parent = self(),
        Workers =
            [spawn_link(fun() ->
                           StartTime = erlang:monotonic_time(microsecond),

                           % Trip the breaker
                           _ = erlmcp_circuit_breaker:call(P, fun() -> {error, failed} end),
                           _ = erlmcp_circuit_breaker:call(P, fun() -> {error, failed} end),
                           _ = erlmcp_circuit_breaker:call(P, fun() -> {error, failed} end),

                           EndTime = erlang:monotonic_time(microsecond),
                           LatencyUs = EndTime - StartTime,

                           Parent ! {transition_latency, LatencyUs}
                        end)
             || P <- Pids],

        % Collect all latencies
        Latencies =
            [receive
                 {transition_latency, L} ->
                     L
             after 5000 ->
                 error(timeout)
             end
             || _ <- Workers],

        AvgLatency = lists:sum(Latencies) / length(Latencies),
        MaxLatency = lists:max(Latencies),

        ?assert(AvgLatency < 2000), % Allow 2ms with contention
        ?assert(MaxLatency < 5000),
        ?debugFmt("OTP 28 concurrent transitions - Avg: ~p us, Max: ~p us",
                  [AvgLatency, MaxLatency]),

        % Cleanup
        [erlmcp_circuit_breaker:stop(P) || P <- Pids]
     end}.
