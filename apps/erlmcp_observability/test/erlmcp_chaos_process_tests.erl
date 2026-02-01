%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_chaos_process following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior of chaos primitives
%%% - Use real processes (no mocks)
%%% - Verify effects through process state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_process_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% Test killing supervisor tree
kill_supervisor_tree_nonexistent_test() ->
    Config = #{target => nonexistent_supervisor},

    % Should handle non-existent supervisor gracefully
    ok = erlmcp_chaos_process:kill_supervisor_tree(Config).

kill_supervisor_tree_existing_test() ->
    % Create a simple supervisor for testing
    SupSpec = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % Start a test supervisor
    TestSupPid = spawn(fun() ->
        receive stop -> ok end
    end),

    register(test_chaos_supervisor, TestSupPid),

    Config = #{target => test_chaos_supervisor},

    % Kill it
    ok = erlmcp_chaos_process:kill_supervisor_tree(Config),

    % Supervisor should be gone
    timer:sleep(100),
    ?assertEqual(undefined, whereis(test_chaos_supervisor)).

%% Test clock skew injection
inject_clock_skew_test() ->
    Config = #{
        skew_ms => 100,
        duration => 150
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_process:inject_clock_skew(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Should wait for duration
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 150),
    ?assert(ElapsedTime < 300).

%% Test clock skew with zero duration
clock_skew_zero_duration_test() ->
    Config = #{
        skew_ms => 1000,
        duration => 0
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_process:inject_clock_skew(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Should complete quickly
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime < 50).

%%====================================================================
%% Edge Cases
%%====================================================================

default_config_values_test() ->
    % Test with minimal config (should use defaults)
    MinimalConfig = #{target => test_sup},

    ok = erlmcp_chaos_process:kill_supervisor_tree(MinimalConfig).

multiple_supervisor_kills_test() ->
    % Test killing multiple supervisor trees in sequence
    lists:foreach(fun(N) ->
        SupName = list_to_atom("test_sup_" ++ integer_to_list(N)),
        ok = erlmcp_chaos_process:kill_supervisor_tree(#{target => SupName})
    end, lists:seq(1, 5)).

concurrent_clock_skew_test() ->
    % Multiple concurrent clock skew injections
    Pids = [spawn(fun() ->
        erlmcp_chaos_process:inject_clock_skew(#{
            skew_ms => 50,
            duration => 100
        })
    end) || _ <- lists:seq(1, 3)],

    % Wait for all
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 5000 -> timeout
        end
    end, Pids).
