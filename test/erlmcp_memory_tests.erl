%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Optimization Tests
%%%
%%% Quick tests to verify memory optimization modules compile and run.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test group
all_test_() ->
    [
        {setup,
         fun setup/0,
         fun teardown/1,
         [
             ?_test(memory_stats_test()),
             ?_test(memory_estimation_test()),
             ?_test(buffer_pool_stats_test())
         ]}
    ].

setup() ->
    application:ensure_all_started(erlmcp).

teardown(_) ->
    ok.

%% Test memory stats
memory_stats_test() ->
    Stats = erlmcp_memory_optimization:get_memory_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total_bytes, Stats)),
    ?assert(maps:is_key(processes, Stats)),
    ?assert(maps:is_key(ets, Stats)),
    ?assert(maps:is_key(binary, Stats)).

%% Test connection memory estimation
memory_estimation_test() ->
    Est = erlmcp_memory_optimization:estimate_connection_memory(100),
    ?assert(Est > 0),
    ?assert(Est =:= 100 * 204800).

%% Test buffer pool stats (if available)
buffer_pool_stats_test() ->
    case code:is_loaded(erlmcp_buffer_pool) of
        false -> ok;  % Skip if not loaded
        _ ->
            case erlmcp_buffer_pool:pool_stats() of
                Stats when is_map(Stats) ->
                    ?assert(maps:is_key(available, Stats)),
                    ?assert(maps:is_key(in_use, Stats));
                _ ->
                    ok  % Module exists but not running
            end
    end.
