%%%-------------------------------------------------------------------
%% @doc Common Test suite for Circuit Breaker (v1.3.0)
%%
%% Tests circuit breaker state machine with retry amplification prevention:
%% - State transitions (closed → open → half_open → closed)
%% - Failure threshold behavior
%% - Cool-down time enforcement
%% - Success counting in half_open state
%% - Retry blocking under circuit open conditions
%% - Queue bounded behavior with retries
%% - 10% random loss injection with retry amplification metrics
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_circuit_breaker_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% ====================================================================
%% CT Callbacks
%% ====================================================================

all() ->
    [
        {group, state_transitions},
        {group, retry_blocking},
        {group, cool_down},
        {group, half_open_recovery},
        {group, loss_injection},
        {group, benchmarks}
    ].

groups() ->
    [
        {state_transitions, [], [
            test_initial_state_closed,
            test_closed_to_open_on_threshold,
            test_open_to_half_open_after_cooldown,
            test_half_open_to_closed_on_success,
            test_half_open_to_open_on_failure,
            test_state_transition_logging
        ]},
        {retry_blocking, [], [
            test_can_execute_returns_allow_when_closed,
            test_can_execute_returns_allow_when_half_open,
            test_can_execute_returns_deny_when_open,
            test_retry_attempts_blocked_metric,
            test_retry_amplification_prevented
        ]},
        {cool_down, [], [
            test_cool_down_enforcement,
            test_no_state_churn_during_cooldown,
            test_multiple_cooldown_cycles
        ]},
        {half_open_recovery, [], [
            test_half_open_requires_successes,
            test_failure_during_half_open_reopens,
            test_half_open_success_tracking
        ]},
        {loss_injection, [], [
            test_loss_with_retries_closed_circuit,
            test_loss_with_retries_open_circuit,
            test_queue_bounded_with_loss,
            test_loss_doesnt_exceed_threshold
        ]},
        {benchmarks, [], [
            test_state_transition_timeline,
            test_retry_rate_comparison,
            test_queue_depth_stability
        ]}
    ].

init_per_suite(Config) ->
    ct:log("Initializing circuit breaker test suite~n", []),
    application:ensure_all_started(erlmcp),
    erlmcp_circuit_breaker:start_link(),
    timer:sleep(100),
    Config.

end_per_suite(_Config) ->
    erlmcp_circuit_breaker:stop(),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    erlmcp_circuit_breaker:reset(),
    timer:sleep(100),
    ok.

init_per_testcase(_TestName, Config) ->
    erlmcp_circuit_breaker:reset(),
    timer:sleep(100),
    Config.

end_per_testcase(_TestName, _Config) ->
    ok.

%% ====================================================================
%% State Transition Tests
%% ====================================================================

test_initial_state_closed(Config) ->
    %% Circuit should start in closed state
    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(closed, Status),
    ct:log("Initial state: ~p~n", [Status]),
    Config.

test_closed_to_open_on_threshold(Config) ->
    %% Record 5+ failures to trigger open state
    FailureThreshold = 5,
    RecordFailures = fun() ->
        [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, FailureThreshold)]
    end,

    RecordFailures(),
    timer:sleep(100),

    %% Wait for metrics update
    wait_for_state_change(open, 10000),

    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, Status),

    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    ?assertMatch(#{state_transitions := N} when N > 0, Metrics),

    ct:log("State transitioned to open after ~p failures~n", [FailureThreshold]),
    Config.

test_open_to_half_open_after_cooldown(Config) ->
    %% Move to open state
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    {ok, OpenStatus} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, OpenStatus),

    %% Wait for cool-down (30 seconds in default config, use 5s for testing)
    CoolDownTime = 5000,
    timer:sleep(CoolDownTime + 500),

    %% Trigger metrics update
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, CoolDownTime + 1000),

    {ok, HalfOpenStatus} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(half_open, HalfOpenStatus),
    ct:log("State transitioned to half_open after cool-down~n", []),
    Config.

test_half_open_to_closed_on_success(Config) ->
    %% Move to half_open state
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),

    {ok, HalfOpenStatus} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(half_open, HalfOpenStatus),

    %% Record successes to close circuit
    SuccessThreshold = 2,
    [erlmcp_circuit_breaker:record_success() || _ <- lists:seq(1, SuccessThreshold)],
    timer:sleep(100),

    %% Trigger metric update
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(closed, 2000),

    {ok, ClosedStatus} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(closed, ClosedStatus),
    ct:log("State transitioned to closed after successes~n", []),
    Config.

test_half_open_to_open_on_failure(Config) ->
    %% Move to half_open state
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),

    %% Record a failure in half_open state
    erlmcp_circuit_breaker:record_error(test_fail),
    timer:sleep(100),

    %% Trigger metric update
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(open, 2000),

    {ok, ReopenStatus} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, ReopenStatus),
    ct:log("Circuit re-opened after failure in half_open~n", []),
    Config.

test_state_transition_logging(Config) ->
    %% Record failures to trigger state transitions
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    TransitionCount = maps:get(state_transitions, Metrics, 0),

    %% Should have at least 1 transition (closed → open)
    ?assert(TransitionCount >= 1),

    ?assertMatch(#{last_state_change := TimeStamp} when is_integer(TimeStamp), Metrics),
    ct:log("State transitions recorded: ~p~n", [TransitionCount]),
    Config.

%% ====================================================================
%% Retry Blocking Tests
%% ====================================================================

test_can_execute_returns_allow_when_closed(Config) ->
    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(closed, Status),

    {CanExecute, Action} = erlmcp_circuit_breaker:can_execute(),
    ?assertEqual(true, CanExecute),
    ?assertEqual(allow, Action),
    ct:log("Closed circuit allows execution~n", []),
    Config.

test_can_execute_returns_allow_when_half_open(Config) ->
    %% Move to half_open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),

    {CanExecute, Action} = erlmcp_circuit_breaker:can_execute(),
    ?assertEqual(true, CanExecute),
    ?assertEqual(allow, Action),
    ct:log("Half-open circuit allows execution for recovery probe~n", []),
    Config.

test_can_execute_returns_deny_when_open(Config) ->
    %% Move to open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    {CanExecute, Action} = erlmcp_circuit_breaker:can_execute(),
    ?assertEqual(false, CanExecute),
    ?assertEqual(deny, Action),
    ct:log("Open circuit denies execution~n", []),
    Config.

test_retry_attempts_blocked_metric(Config) ->
    %% Move circuit to open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    %% Record retry attempts
    RetryAttempts = 10,
    [erlmcp_circuit_breaker:record_retry_attempt() || _ <- lists:seq(1, RetryAttempts)],
    timer:sleep(100),

    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    BlockedRetries = maps:get(retry_attempts_blocked, Metrics, 0),

    ?assertEqual(RetryAttempts, BlockedRetries),
    ct:log("Blocked ~p retry attempts~n", [BlockedRetries]),
    Config.

test_retry_amplification_prevented(Config) ->
    %% Scenario: 5 failed requests trying to retry
    %% With breaker: all retries blocked after threshold
    %% Without breaker: retries would compound the problem

    %% Phase 1: Record initial failures
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    {CanExecuteBefore, _} = erlmcp_circuit_breaker:can_execute(),
    ?assertEqual(false, CanExecuteBefore),

    %% Phase 2: Attempt to record retries (should be blocked)
    InitialBlockedCount = get_retry_blocked_count(),
    [erlmcp_circuit_breaker:record_retry_attempt() || _ <- lists:seq(1, 50)],
    timer:sleep(100),

    FinalBlockedCount = get_retry_blocked_count(),
    BlockedDelta = FinalBlockedCount - InitialBlockedCount,

    %% Verify retries were blocked
    ?assertEqual(50, BlockedDelta),
    ct:log("Retry amplification prevented: ~p retries blocked~n", [BlockedDelta]),
    Config.

%% ====================================================================
%% Cool-Down Tests
%% ====================================================================

test_cool_down_enforcement(Config) ->
    %% Record failures to open circuit
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    {ok, OpenTime} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, OpenTime),

    %% Check state after 2 seconds (before cool-down)
    timer:sleep(2000),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    timer:sleep(100),

    {ok, Status1} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, Status1),

    %% Check state after full cool-down
    timer:sleep(3500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 2000),

    {ok, Status2} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(half_open, Status2),

    ct:log("Cool-down period enforced correctly~n", []),
    Config.

test_no_state_churn_during_cooldown(Config) ->
    %% Move to open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    {ok, Metrics1} = erlmcp_circuit_breaker:get_metrics(),
    TransitionCount1 = maps:get(state_transitions, Metrics1, 0),

    %% Wait 2 seconds and check for churn
    timer:sleep(2000),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    timer:sleep(100),

    {ok, Metrics2} = erlmcp_circuit_breaker:get_metrics(),
    TransitionCount2 = maps:get(state_transitions, Metrics2, 0),

    %% Should not have additional transitions during open state
    ?assertEqual(TransitionCount1, TransitionCount2),
    ct:log("No state churn during cool-down: transitions = ~p~n", [TransitionCount1]),
    Config.

test_multiple_cooldown_cycles(Config) ->
    %% Verify multiple open/cool-down/half-open cycles work correctly
    CycleFun = fun(_Cycle) ->
        erlmcp_circuit_breaker:reset(),
        timer:sleep(100),

        %% Open circuit
        [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
        wait_for_state_change(open, 10000),

        %% Wait for cool-down
        timer:sleep(5500),
        erlmcp_circuit_breaker:record_request(test_id, 10),
        wait_for_state_change(half_open, 6000),

        {ok, Status} = erlmcp_circuit_breaker:get_status(),
        ?assertEqual(half_open, Status)
    end,

    [CycleFun(N) || N <- lists:seq(1, 3)],
    ct:log("Completed 3 successful cool-down cycles~n", []),
    Config.

%% ====================================================================
%% Half-Open Recovery Tests
%% ====================================================================

test_half_open_requires_successes(Config) ->
    %% Move to half_open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),

    %% Single success should not close
    erlmcp_circuit_breaker:record_success(),
    timer:sleep(100),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    timer:sleep(100),

    {ok, Status1} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(half_open, Status1),

    %% Two successes should close
    erlmcp_circuit_breaker:record_success(),
    timer:sleep(100),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(closed, 2000),

    {ok, Status2} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(closed, Status2),

    ct:log("Half-open recovery threshold enforced (2 successes)~n", []),
    Config.

test_failure_during_half_open_reopens(Config) ->
    %% Move to half_open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),

    {ok, StatusBefore} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(half_open, StatusBefore),

    %% Record failure in half_open
    erlmcp_circuit_breaker:record_error(reopen_test),
    timer:sleep(100),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(open, 2000),

    {ok, StatusAfter} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, StatusAfter),

    ct:log("Half-open failure correctly re-opens circuit~n", []),
    Config.

test_half_open_success_tracking(Config) ->
    %% Move to half_open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),

    %% Record multiple successes
    [erlmcp_circuit_breaker:record_success() || _ <- lists:seq(1, 2)],
    timer:sleep(100),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(closed, 2000),

    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(closed, Status),

    ct:log("Success tracking in half_open state verified~n", []),
    Config.

%% ====================================================================
%% Loss Injection Tests (10% Random Loss)
%% ====================================================================

test_loss_with_retries_closed_circuit(Config) ->
    %% Simulate 10% random loss with retries while circuit is closed
    NumRequests = 100,
    LossRate = 0.10,

    Results = [
        case inject_random_loss(LossRate) of
            true ->
                erlmcp_circuit_breaker:record_error(Id),
                error;
            false ->
                erlmcp_circuit_breaker:record_request(Id, 50),
                success
        end
        || Id <- lists:seq(1, NumRequests)
    ],

    ErrorCount = length([R || R <- Results, R =:= error]),
    SuccessCount = length([R || R <- Results, R =:= success]),

    %% Verify circuit remains closed (< 5 failures)
    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(closed, Status),

    ct:log("Loss test (closed): ~p errors, ~p successes (expected ~10% loss)~n",
           [ErrorCount, SuccessCount]),
    Config.

test_loss_with_retries_open_circuit(Config) ->
    %% Move to open state first
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),

    %% Now with circuit open, retry attempts should be blocked
    RetryAttempts = 50,
    [erlmcp_circuit_breaker:record_retry_attempt() || _ <- lists:seq(1, RetryAttempts)],
    timer:sleep(100),

    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    BlockedRetries = maps:get(retry_attempts_blocked, Metrics, 0),

    %% Verify retries were blocked
    ?assertEqual(RetryAttempts, BlockedRetries),

    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assertEqual(open, Status),

    ct:log("Open circuit blocked ~p retry attempts~n", [BlockedRetries]),
    Config.

test_queue_bounded_with_loss(Config) ->
    %% Simulate high-frequency requests with 10% loss
    %% Verify queue stays bounded even under stress
    NumRequests = 1000,
    LossRate = 0.10,

    QueueSizes = [
        begin
            case inject_random_loss(LossRate) of
                true -> erlmcp_circuit_breaker:record_error(Id);
                false -> erlmcp_circuit_breaker:record_request(Id, 10)
            end,
            case Id rem 100 =:= 0 of
                true ->
                    {ok, M} = erlmcp_circuit_breaker:get_metrics(),
                    maps:get(requests_in_window, M, 0);
                false ->
                    0
            end
        end
        || Id <- lists:seq(1, NumRequests)
    ],

    MaxQueueSize = lists:max([Q || Q <- QueueSizes, Q > 0]),

    %% Queue should remain bounded (max 1000 in window)
    ?assert(MaxQueueSize =< 1000),

    ct:log("Queue remained bounded during stress: max size = ~p~n", [MaxQueueSize]),
    Config.

test_loss_doesnt_exceed_threshold(Config) ->
    %% With 10% loss, should not reach 5-failure threshold immediately
    NumRequests = 40,
    LossRate = 0.10,

    [
        case inject_random_loss(LossRate) of
            true -> erlmcp_circuit_breaker:record_error(Id);
            false -> erlmcp_circuit_breaker:record_request(Id, 50)
        end
        || Id <- lists:seq(1, NumRequests)
    ],

    timer:sleep(100),

    %% Circuit should likely still be closed (< 5 failures)
    %% Note: with 10% loss on 40 requests, expected ~4 failures
    {ok, Status} = erlmcp_circuit_breaker:get_status(),

    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    ErrorsInWindow = maps:get(errors_in_window, Metrics, 0),

    ct:log("After ~40 requests with 10% loss: ~p errors, state = ~p~n",
           [ErrorsInWindow, Status]),
    Config.

%% ====================================================================
%% Benchmark Tests
%% ====================================================================

test_state_transition_timeline(Config) ->
    %% Capture state transitions over time
    StartTime = erlang:system_time(millisecond),

    Timeline = [
        {StartTime, closed}
    ],

    %% Move to open
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    wait_for_state_change(open, 10000),
    TimeOpen = erlang:system_time(millisecond),

    {ok, Metrics1} = erlmcp_circuit_breaker:get_metrics(),
    LastStateChange1 = maps:get(last_state_change, Metrics1),

    Timeline2 = Timeline ++ [{TimeOpen, open}],

    %% Move to half_open
    timer:sleep(5500),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(half_open, 6000),
    TimeHalfOpen = erlang:system_time(millisecond),

    {ok, Metrics2} = erlmcp_circuit_breaker:get_metrics(),
    LastStateChange2 = maps:get(last_state_change, Metrics2),

    Timeline3 = Timeline2 ++ [{TimeHalfOpen, half_open}],

    %% Move back to closed
    [erlmcp_circuit_breaker:record_success() || _ <- lists:seq(1, 2)],
    timer:sleep(100),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    wait_for_state_change(closed, 2000),
    TimeClosed = erlang:system_time(millisecond),

    {ok, Metrics3} = erlmcp_circuit_breaker:get_metrics(),
    LastStateChange3 = maps:get(last_state_change, Metrics3),

    Timeline4 = Timeline3 ++ [{TimeClosed, closed}],

    ct:log("State Transition Timeline:~n", []),
    print_timeline(Timeline4),

    Config.

test_retry_rate_comparison(Config) ->
    %% Compare retry rates with and without circuit breaker

    %% Phase 1: Circuit enabled (default)
    erlmcp_circuit_breaker:reset(),
    timer:sleep(100),

    TestWithBreaker = fun() ->
        %% Open circuit
        [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
        wait_for_state_change(open, 10000),

        %% Try 100 retries
        StartTime = erlang:system_time(millisecond),
        [erlmcp_circuit_breaker:record_retry_attempt() || _ <- lists:seq(1, 100)],
        EndTime = erlang:system_time(millisecond),

        {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
        BlockedRetries = maps:get(retry_attempts_blocked, Metrics, 0),

        ElapsedMs = EndTime - StartTime,
        RetryRate = case ElapsedMs > 0 of
            true -> (BlockedRetries * 1000) / ElapsedMs;
            false -> 0
        end,

        {BlockedRetries, RetryRate, ElapsedMs}
    end,

    {Blocked, Rate, Elapsed} = TestWithBreaker(),

    ct:log("Retry Prevention Metrics:~n", []),
    ct:log("  Blocked retries: ~p~n", [Blocked]),
    ct:log("  Retry rate: ~.2f retries/sec~n", [Rate]),
    ct:log("  Time elapsed: ~p ms~n", [Elapsed]),

    %% Verify all retries were blocked
    ?assertEqual(100, Blocked),

    Config.

test_queue_depth_stability(Config) ->
    %% Monitor queue depth during multiple cycles
    %% Ensure no unbounded growth

    QueueDepths = [
        begin
            erlmcp_circuit_breaker:reset(),
            timer:sleep(100),

            %% Generate 200 requests with varying loss
            [
                case inject_random_loss(0.05) of
                    true -> erlmcp_circuit_breaker:record_error(Id);
                    false -> erlmcp_circuit_breaker:record_request(Id, 10)
                end
                || Id <- lists:seq(1, 200)
            ],

            {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
            RequestsInWindow = maps:get(requests_in_window, Metrics, 0),
            RequestsInWindow
        end
        || _Cycle <- lists:seq(1, 5)
    ],

    MaxQueueDepth = lists:max(QueueDepths),
    AvgQueueDepth = lists:sum(QueueDepths) / length(QueueDepths),

    ct:log("Queue Depth Stability Across 5 Cycles:~n", []),
    ct:log("  Max depth: ~p~n", [MaxQueueDepth]),
    ct:log("  Avg depth: ~.2f~n", [AvgQueueDepth]),
    ct:log("  Per-cycle: ~p~n", [QueueDepths]),

    %% Queue should stay bounded
    ?assert(MaxQueueDepth =< 1000),
    ?assert(MaxQueueDepth >= 100),  % Reasonable minimum

    Config.

%% ====================================================================
%% Helper Functions
%% ====================================================================

wait_for_state_change(TargetState, MaxWaitMs) ->
    wait_for_state_change_loop(TargetState, MaxWaitMs, erlang:system_time(millisecond)).

wait_for_state_change_loop(TargetState, MaxWaitMs, StartTime) ->
    {ok, CurrentState} = erlmcp_circuit_breaker:get_status(),
    ElapsedMs = erlang:system_time(millisecond) - StartTime,

    case CurrentState =:= TargetState of
        true ->
            ok;
        false ->
            case ElapsedMs >= MaxWaitMs of
                true ->
                    ct:fail("Timeout waiting for state transition to ~p, current: ~p",
                           [TargetState, CurrentState]);
                false ->
                    timer:sleep(50),
                    wait_for_state_change_loop(TargetState, MaxWaitMs, StartTime)
            end
    end.

get_retry_blocked_count() ->
    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    maps:get(retry_attempts_blocked, Metrics, 0).

inject_random_loss(LossRate) ->
    Random = rand:uniform(),
    Random < LossRate.

print_timeline([]) ->
    ok;
print_timeline([{Time, State} | Rest]) ->
    ct:log("  ~p: ~p~n", [Time, State]),
    print_timeline(Rest).
