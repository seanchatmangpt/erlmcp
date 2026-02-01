%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_chaos_network following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior of chaos primitives
%%% - Use real timeouts and delays (no mocks)
%%% - Verify effects through observable outcomes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_network_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% Test latency injection
inject_latency_test() ->
    Config = #{
        latency => 50,      % 50ms latency
        rate => 1.0,        % 100% of requests
        duration => 200,    % 200ms total duration
        interval => 50      % Check every 50ms
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_network:inject_latency(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Total time should be at least duration (200ms)
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 200),
    % But not too much more (allow 100ms tolerance)
    ?assert(ElapsedTime < 400).

%% Test latency injection with low rate
inject_latency_low_rate_test() ->
    Config = #{
        latency => 100,
        rate => 0.0,        % 0% - no latency injected
        duration => 100,
        interval => 50
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_network:inject_latency(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % With 0% rate, should complete quickly (just duration time)
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 100),
    ?assert(ElapsedTime < 200).

%% Test network partition injection
inject_partition_test() ->
    Config = #{
        duration => 100,    % 100ms partition
        nodes => [node()]   % Only local node
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_network:inject_partition(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Should take at least duration time
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 100),
    ?assert(ElapsedTime < 200).

%% Test packet loss injection (short duration)
inject_packet_loss_short_test() ->
    % Use timeout to limit packet loss loop
    Config = #{
        rate => 0.5,        % 50% packet loss
        interval => 50      % Check every 50ms
    },

    % Run in separate process with timeout
    Pid = spawn(fun() ->
        erlmcp_chaos_network:inject_packet_loss(Config)
    end),

    % Let it run for 200ms
    timer:sleep(200),

    % Kill the process
    exit(Pid, kill),

    % Verify process is dead
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

%% Test connection throttling
throttle_connections_test() ->
    Config = #{
        max_rate => 100,    % 100 connections/sec
        duration => 100     % 100ms duration
    },

    StartTime = erlang:monotonic_time(millisecond),
    ok = erlmcp_chaos_network:throttle_connections(Config),
    EndTime = erlang:monotonic_time(millisecond),

    % Should take at least duration time
    ElapsedTime = EndTime - StartTime,
    ?assert(ElapsedTime >= 100),
    ?assert(ElapsedTime < 200).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test zero duration latency
zero_duration_latency_test() ->
    Config = #{
        latency => 50,
        rate => 1.0,
        duration => 0,      % Zero duration
        interval => 50
    },

    % Should complete immediately
    ok = erlmcp_chaos_network:inject_latency(Config).

%% Test negative remaining time handling
negative_remaining_latency_test() ->
    Config = #{
        latency => 200,     % High latency
        rate => 1.0,
        duration => 50,     % Short duration
        interval => 100
    },

    % Should handle negative remaining gracefully
    ok = erlmcp_chaos_network:inject_latency(Config).

%% Test empty nodes list partition
empty_nodes_partition_test() ->
    Config = #{
        duration => 100,
        nodes => []         % Empty nodes list
    },

    ok = erlmcp_chaos_network:inject_partition(Config).

%% Test with default configuration values
default_config_latency_test() ->
    Config = #{
        latency => 50,
        duration => 100
        % Missing rate and interval - should use defaults
    },

    ok = erlmcp_chaos_network:inject_latency(Config).

%% Test concurrent chaos operations
concurrent_chaos_test() ->
    % Spawn multiple chaos operations concurrently
    Pid1 = spawn(fun() ->
        erlmcp_chaos_network:inject_latency(#{
            latency => 20,
            rate => 1.0,
            duration => 100,
            interval => 20
        })
    end),

    Pid2 = spawn(fun() ->
        erlmcp_chaos_network:inject_partition(#{
            duration => 100,
            nodes => []
        })
    end),

    Pid3 = spawn(fun() ->
        erlmcp_chaos_network:throttle_connections(#{
            max_rate => 100,
            duration => 100
        })
    end),

    % Wait for all to complete
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 5000 -> timeout
        end
    end, [Pid1, Pid2, Pid3]).

%% Test rapid successive calls
rapid_successive_test() ->
    lists:foreach(fun(_N) ->
        erlmcp_chaos_network:throttle_connections(#{
            max_rate => 100,
            duration => 10
        })
    end, lists:seq(1, 10)).
