%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_chaos_resource following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior of resource chaos primitives
%%% - Use real resource constraints (no mocks)
%%% - Verify effects through system state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_resource_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% Test memory exhaustion
exhaust_memory_test() ->
    Config = #{
        target_percent => 0.5,  % 50% - safe test value
        duration => 100         % 100ms
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_resource:exhaust_memory(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Should take at least duration time
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 100),
    ?assert(ElapsedTime < 300).

%% Test memory exhaustion with high target
exhaust_memory_high_target_test() ->
    Config = #{
        target_percent => 0.95,  % 95%
        duration => 50
    },

    % Should handle safely even with high target
    ok = erlmcp_chaos_resource:exhaust_memory(Config).

%% Test CPU saturation
saturate_cpu_test() ->
    Config = #{
        target_load => 0.5,      % 50% of schedulers
        duration => 200          % 200ms
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_resource:saturate_cpu(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Should take at least duration time
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 200),
    ?assert(ElapsedTime < 400).

%% Test CPU saturation with zero load
saturate_cpu_zero_load_test() ->
    Config = #{
        target_load => 0.0,      % 0% - no workers
        duration => 50
    },

    % Should complete quickly with no workers
    ok = erlmcp_chaos_resource:saturate_cpu(Config).

%% Test disk fill simulation
fill_disk_test() ->
    Config = #{
        target_percent => 0.8,
        temp_file => "/tmp/erlmcp_chaos_test",
        duration => 100
    },

    % Should simulate disk fill
    ok = erlmcp_chaos_resource:fill_disk(Config).

%%====================================================================
%% Edge Cases
%%====================================================================

default_config_memory_test() ->
    % Test with minimal config (should use defaults)
    MinimalConfig = #{},

    ok = erlmcp_chaos_resource:exhaust_memory(MinimalConfig).

default_config_cpu_test() ->
    MinimalConfig = #{},

    ok = erlmcp_chaos_resource:saturate_cpu(MinimalConfig).

zero_duration_test() ->
    Config = #{
        target_percent => 0.7,
        duration => 0
    },

    % Should complete immediately
    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_resource:exhaust_memory(Config),
    EndTime = erlang:monotonic_time(millisecond),

    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime < 50).

concurrent_chaos_test() ->
    % Run multiple chaos operations concurrently
    Pid1 = spawn(fun() ->
        erlmcp_chaos_resource:exhaust_memory(#{
            target_percent => 0.5,
            duration => 100
        })
    end),

    Pid2 = spawn(fun() ->
        erlmcp_chaos_resource:saturate_cpu(#{
            target_load => 0.3,
            duration => 100
        })
    end),

    % Wait for both
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 5000 -> timeout
        end
    end, [Pid1, Pid2]).
