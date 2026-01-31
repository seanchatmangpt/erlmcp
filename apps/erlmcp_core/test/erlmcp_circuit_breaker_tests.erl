-module(erlmcp_circuit_breaker_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Clean up any previously registered breakers to avoid already_registered errors
    lists:foreach(fun(Name) ->
        catch erlmcp_circuit_breaker:unregister_breaker(Name)
    end, [test_breaker, breaker1, breaker2, breaker3]),
    catch erlmcp_circuit_breaker:reset_all(),
    timer:sleep(10),
    undefined.

cleanup(_Pid) ->
    % Clean up all registered breakers
    catch erlmcp_circuit_breaker:reset_all(),
    % Unregister common test breakers to avoid already_registered errors
    lists:foreach(fun(Name) ->
        catch erlmcp_circuit_breaker:unregister_breaker(Name)
    end, [test_breaker, breaker1, breaker2, breaker3]),
    % Give breakers time to process
    timer:sleep(10),
    ok.

%%====================================================================
%% Basic Functionality Tests
%%====================================================================

start_stop_test() ->
    % Test starting and stopping a single breaker
    Config = #{},
    {ok, Pid} = erlmcp_circuit_breaker:start_link(test_breaker, Config),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),
    erlmcp_circuit_breaker:stop(test_breaker),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

register_breaker_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 3, timeout => 1000},
    ?assertEqual(ok, erlmcp_circuit_breaker:register_breaker(test_breaker, Config)),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),
    cleanup(Pid).

unregister_breaker_test() ->
    Pid = setup(),
    Config = #{},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),
    erlmcp_circuit_breaker:unregister_breaker(test_breaker),
    timer:sleep(10),  % Allow cast to process
    % After unregister, the breaker process is dead so get_state will exit with noproc
    ?assertExit({noproc, _}, erlmcp_circuit_breaker:get_state(test_breaker)),
    cleanup(Pid).

%%====================================================================
%% State Transition Tests
%%====================================================================

closed_state_success_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 3},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Successful call should keep breaker closed
    Result = erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success} end),
    ?assertEqual({ok, success}, Result),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    {ok, Stats} = erlmcp_circuit_breaker:get_stats(test_breaker),
    ?assertEqual(1, maps:get(total_calls, Stats)),
    ?assertEqual(1, maps:get(total_successes, Stats)),
    ?assertEqual(0, maps:get(total_failures, Stats)),

    cleanup(Pid).

closed_to_open_transition_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 3, timeout => 60000},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Three failures should trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail3} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    cleanup(Pid).

open_state_rejects_calls_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2, timeout => 60000},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Subsequent calls should be rejected
    Result = erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, should_not_run} end),
    ?assertEqual({error, circuit_breaker_open}, Result),

    {ok, Stats} = erlmcp_circuit_breaker:get_stats(test_breaker),
    ?assertEqual(1, maps:get(total_rejected, Stats)),

    cleanup(Pid).

open_to_half_open_transition_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2, timeout => 100},  % 100ms timeout
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Wait for timeout to transition to half_open
    timer:sleep(150),

    % Next call should transition to half_open
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success} end),

    % May be half_open or closed depending on success_threshold
    State = erlmcp_circuit_breaker:get_state(test_breaker),
    ?assert(State =:= half_open orelse State =:= closed),

    cleanup(Pid).

half_open_to_closed_transition_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2, success_threshold => 2, timeout => 100},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Wait for timeout
    timer:sleep(150),

    % Two successful calls should close the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success2} end),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    cleanup(Pid).

half_open_to_open_transition_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2, timeout => 100},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Wait for timeout
    timer:sleep(150),

    % Failure in half_open should re-open
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail_again} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    cleanup(Pid).

%%====================================================================
%% Failure Rate Tests
%%====================================================================

failure_rate_threshold_test() ->
    Pid = setup(),
    Config = #{
        failure_threshold => 100,  % High consecutive threshold
        failure_rate_threshold => 0.5,  % 50% failure rate
        window_size => 10
    },
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Mix of successes and failures to hit 50% failure rate
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success} end),

    % 5 calls, 2 failures = 40% rate (should still be closed)
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Add more failures to fill window and reach 50%+
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),

    % 10 calls (window full), 7 failures = 70% rate (should trip)
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    cleanup(Pid).

rolling_window_test() ->
    Pid = setup(),
    Config = #{
        failure_threshold => 100,
        failure_rate_threshold => 0.5,
        window_size => 3  % Small window for testing
    },
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Old failures
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, old_fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, old_fail2} end),

    % Window should only contain last 3 calls
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, new_success1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, new_success2} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, new_success3} end),

    % Recent successes should dominate (0% failure in window)
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    cleanup(Pid).

%%====================================================================
%% Fallback Tests
%%====================================================================

fallback_on_open_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2, timeout => 60000},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Call with fallback
    Result = erlmcp_circuit_breaker:call_with_fallback(
        test_breaker,
        fun() -> {ok, primary} end,
        fun() -> {ok, fallback} end
    ),
    ?assertEqual({ok, fallback}, Result),

    cleanup(Pid).

fallback_not_used_when_closed_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 5},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Breaker is closed, primary should be used
    Result = erlmcp_circuit_breaker:call_with_fallback(
        test_breaker,
        fun() -> {ok, primary} end,
        fun() -> {ok, fallback} end
    ),
    ?assertEqual({ok, primary}, Result),

    cleanup(Pid).

%%====================================================================
%% Exception Handling Tests
%%====================================================================

exception_counts_as_failure_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 3, timeout => 60000},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Exceptions should count as failures
    erlmcp_circuit_breaker:call(test_breaker, fun() -> throw(exception1) end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> error(exception2) end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> exit(exception3) end),

    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    {ok, Stats} = erlmcp_circuit_breaker:get_stats(test_breaker),
    ?assertEqual(3, maps:get(total_failures, Stats)),

    cleanup(Pid).

%%====================================================================
%% Statistics Tests
%%====================================================================

statistics_tracking_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 10},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Mix of successes and failures
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, success2} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),

    {ok, Stats} = erlmcp_circuit_breaker:get_stats(test_breaker),
    ?assertEqual(3, maps:get(total_calls, Stats)),
    ?assertEqual(2, maps:get(total_successes, Stats)),
    ?assertEqual(1, maps:get(total_failures, Stats)),
    ?assert(maps:get(success_rate, Stats) > 0.6),
    ?assert(maps:get(failure_rate, Stats) < 0.4),

    cleanup(Pid).

get_all_stats_test() ->
    Pid = setup(),
    Config = #{},
    erlmcp_circuit_breaker:register_breaker(breaker1, Config),
    erlmcp_circuit_breaker:register_breaker(breaker2, Config),

    erlmcp_circuit_breaker:call(breaker1, fun() -> {ok, success} end),
    erlmcp_circuit_breaker:call(breaker2, fun() -> {error, fail} end),

    AllStats = erlmcp_circuit_breaker:get_all_stats(),
    ?assert(maps:is_key(breaker1, AllStats)),
    ?assert(maps:is_key(breaker2, AllStats)),

    cleanup(Pid).

%%====================================================================
%% Reset Tests
%%====================================================================

reset_breaker_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2, timeout => 60000},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail2} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Reset should close it
    erlmcp_circuit_breaker:reset(test_breaker),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    {ok, Stats} = erlmcp_circuit_breaker:get_stats(test_breaker),
    ?assertEqual(0, maps:get(consecutive_failures, Stats)),

    cleanup(Pid).

reset_all_breakers_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 1},
    erlmcp_circuit_breaker:register_breaker(breaker1, Config),
    erlmcp_circuit_breaker:register_breaker(breaker2, Config),

    % Trip both breakers
    erlmcp_circuit_breaker:call(breaker1, fun() -> {error, fail} end),
    erlmcp_circuit_breaker:call(breaker2, fun() -> {error, fail} end),

    erlmcp_circuit_breaker:reset_all(),
    timer:sleep(10),  % Allow cast to process

    States = erlmcp_circuit_breaker:get_all_states(),
    ?assertEqual(closed, maps:get(breaker1, States)),
    ?assertEqual(closed, maps:get(breaker2, States)),

    cleanup(Pid).

%%====================================================================
%% Force State Tests
%%====================================================================

force_open_test() ->
    Pid = setup(),
    Config = #{},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    erlmcp_circuit_breaker:force_open(test_breaker),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Should reject calls
    Result = erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, test} end),
    ?assertEqual({error, circuit_breaker_open}, Result),

    cleanup(Pid).

force_close_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 1},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Trip the breaker
    erlmcp_circuit_breaker:call(test_breaker, fun() -> {error, fail} end),
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Force close
    erlmcp_circuit_breaker:force_close(test_breaker),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test_breaker)),

    % Should accept calls
    Result = erlmcp_circuit_breaker:call(test_breaker, fun() -> {ok, test} end),
    ?assertEqual({ok, test}, Result),

    cleanup(Pid).

%%====================================================================
%% Multiple Breakers Tests
%%====================================================================

multiple_breakers_independence_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 2},
    erlmcp_circuit_breaker:register_breaker(breaker1, Config),
    erlmcp_circuit_breaker:register_breaker(breaker2, Config),

    % Trip breaker1
    erlmcp_circuit_breaker:call(breaker1, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(breaker1, fun() -> {error, fail2} end),

    % breaker1 should be open, breaker2 should be closed
    ?assertEqual(open, erlmcp_circuit_breaker:get_state(breaker1)),
    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(breaker2)),

    % breaker2 should still accept calls
    Result = erlmcp_circuit_breaker:call(breaker2, fun() -> {ok, success} end),
    ?assertEqual({ok, success}, Result),

    cleanup(Pid).

get_all_states_test() ->
    Pid = setup(),
    Config = #{failure_threshold => 1},
    erlmcp_circuit_breaker:register_breaker(breaker1, Config),
    erlmcp_circuit_breaker:register_breaker(breaker2, Config),
    erlmcp_circuit_breaker:register_breaker(breaker3, Config),

    % Trip breaker2
    erlmcp_circuit_breaker:call(breaker2, fun() -> {error, fail} end),

    % Give breaker time to transition state
    timer:sleep(10),

    States = erlmcp_circuit_breaker:get_all_states(),
    ?assertEqual(closed, maps:get(breaker1, States)),
    ?assertEqual(open, maps:get(breaker2, States)),
    ?assertEqual(closed, maps:get(breaker3, States)),

    cleanup(Pid).

%%====================================================================
%% Error Handling Tests
%%====================================================================

breaker_not_found_test() ->
    Pid = setup(),
    % get_state/1 exits with noproc for non-existent breakers (gen_statem behavior)
    ?assertExit({noproc, _}, erlmcp_circuit_breaker:get_state(nonexistent)),
    % call/1 also exits with noproc for non-existent breakers
    ?assertExit({noproc, _}, erlmcp_circuit_breaker:call(nonexistent, fun() -> {ok, test} end)),
    cleanup(Pid).

%%====================================================================
%% Timeout Tests
%%====================================================================

call_timeout_test() ->
    Pid = setup(),
    Config = #{},
    erlmcp_circuit_breaker:register_breaker(test_breaker, Config),

    % Fast timeout should fail for slow function
    % Catch the timeout error from gen_server:call
    Result = try
        erlmcp_circuit_breaker:call(
            test_breaker,
            fun() ->
                timer:sleep(1000),
                {ok, slow}
            end,
            10  % 10ms timeout
        )
    catch
        exit:{timeout, _} -> {error, timeout};
        _:_ -> {error, other}
    end,

    % Should timeout
    ?assertEqual({error, timeout}, Result),

    cleanup(Pid).
