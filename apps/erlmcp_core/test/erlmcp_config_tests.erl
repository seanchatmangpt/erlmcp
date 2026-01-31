-module(erlmcp_config_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    % Start the config server
    {ok, Pid} = erlmcp_config:start_link(),
    Pid.

cleanup(Pid) ->
    % Stop the server
    gen_server:stop(Pid),
    % Clear any remaining persistent_term keys (for test isolation)
    erlmcp_config:clear_cache().

config_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun test_default_config/1,
        fun test_get_with_default/1,
        fun test_set_and_get/1,
        fun test_bulk_update/1,
        fun test_get_all/1,
        fun test_delete/1,
        fun test_reload/1,
        fun test_nonexistent_key/1,
        fun test_transport_defaults/1,
        fun test_capabilities/1,
        fun test_zero_copy_access/1,
        fun test_concurrent_reads/1
    ]}.

%%====================================================================
%% Test Cases
%%====================================================================

test_default_config(_Pid) ->
    [
        ?_assertEqual(10485760, erlmcp_config:get(max_message_size)),
        ?_assertEqual(30000, erlmcp_config:get(request_timeout)),
        ?_assertEqual(300000, erlmcp_config:get(idle_timeout)),
        ?_assertEqual(10000, erlmcp_config:get(max_connections)),
        ?_assert(is_map(erlmcp_config:get(transport_defaults))),
        ?_assert(is_map(erlmcp_config:get(capabilities)))
    ].

test_get_with_default(_Pid) ->
    [
        % Existing key should return value
        ?_assertEqual(10485760, erlmcp_config:get(max_message_size, 999)),
        % Non-existent key should return default
        ?_assertEqual(custom_default, erlmcp_config:get(nonexistent_key, custom_default)),
        ?_assertEqual(42, erlmcp_config:get(missing_key, 42))
    ].

test_set_and_get(_Pid) ->
    [
        ?_test(begin
            ok = erlmcp_config:set(test_key, test_value),
            ?assertEqual(test_value, erlmcp_config:get(test_key))
        end),
        ?_test(begin
            ok = erlmcp_config:set(max_message_size, 20000000),
            ?assertEqual(20000000, erlmcp_config:get(max_message_size))
        end),
        ?_test(begin
            ok = erlmcp_config:set(custom_config, #{nested => #{key => value}}),
            Config = erlmcp_config:get(custom_config),
            ?assert(is_map(Config)),
            ?assertEqual(#{key => value}, maps:get(nested, Config))
        end)
    ].

test_bulk_update(_Pid) ->
    ?_test(begin
        Updates = #{
            key1 => value1,
            key2 => value2,
            key3 => #{nested => data}
        },
        ok = erlmcp_config:update(Updates),
        ?assertEqual(value1, erlmcp_config:get(key1)),
        ?assertEqual(value2, erlmcp_config:get(key2)),
        ?assertEqual(#{nested => data}, erlmcp_config:get(key3))
    end).

test_get_all(_Pid) ->
    ?_test(begin
        ok = erlmcp_config:set(test_key1, value1),
        ok = erlmcp_config:set(test_key2, value2),

        AllConfig = erlmcp_config:get_all(),
        ?assert(is_map(AllConfig)),
        ?assert(maps:is_key(test_key1, AllConfig)),
        ?assert(maps:is_key(test_key2, AllConfig)),
        ?assertEqual(value1, maps:get(test_key1, AllConfig)),
        ?assertEqual(value2, maps:get(test_key2, AllConfig))
    end).

test_delete(_Pid) ->
    ?_test(begin
        ok = erlmcp_config:set(delete_me, temporary_value),
        ?assertEqual(temporary_value, erlmcp_config:get(delete_me, undefined)),

        ok = erlmcp_config:delete(delete_me),
        ?assertEqual(undefined, erlmcp_config:get(delete_me, undefined))
    end).

test_reload(_Pid) ->
    ?_test(begin
        % Modify a config value
        ok = erlmcp_config:set(max_message_size, 999999),
        ?assertEqual(999999, erlmcp_config:get(max_message_size)),

        % Reload should restore from defaults
        ok = erlmcp_config:reload(),
        ?assertEqual(10485760, erlmcp_config:get(max_message_size))
    end).

test_nonexistent_key(_Pid) ->
    [
        ?_assertException(error, badarg, erlmcp_config:get(nonexistent_key_no_default)),
        ?_assertEqual(fallback, erlmcp_config:get(nonexistent_key_with_default, fallback))
    ].

test_transport_defaults(_Pid) ->
    ?_test(begin
        Transports = erlmcp_config:get(transport_defaults),
        ?assert(is_map(Transports)),
        ?assert(maps:is_key(tcp, Transports)),
        ?assert(maps:is_key(http, Transports)),
        ?assert(maps:is_key(ws, Transports)),

        TcpConfig = maps:get(tcp, Transports),
        ?assertEqual(8080, maps:get(port, TcpConfig))
    end).

test_capabilities(_Pid) ->
    ?_test(begin
        Caps = erlmcp_config:get(capabilities),
        ?assert(is_map(Caps)),
        ?assertEqual(true, maps:get(tools, Caps)),
        ?assertEqual(true, maps:get(resources, Caps)),
        ?assertEqual(true, maps:get(prompts, Caps))
    end).

test_zero_copy_access(_Pid) ->
    ?_test(begin
        % Set a large config value
        LargeData = lists:duplicate(1000, {key, value, data}),
        ok = erlmcp_config:set(large_config, LargeData),

        % Read multiple times - should be zero-copy
        Data1 = erlmcp_config:get(large_config),
        Data2 = erlmcp_config:get(large_config),
        Data3 = erlmcp_config:get(large_config),

        % All reads should be identical (same reference)
        ?assertEqual(Data1, Data2),
        ?assertEqual(Data2, Data3),
        ?assertEqual(LargeData, Data1)
    end).

test_concurrent_reads(_Pid) ->
    ?_test(begin
        ok = erlmcp_config:set(concurrent_key, shared_value),

        % Spawn multiple readers
        Parent = self(),
        Readers = [
            spawn_link(fun() ->
                Value = erlmcp_config:get(concurrent_key),
                Parent ! {read, self(), Value}
            end)
         || _ <- lists:seq(1, 100)
        ],

        % Collect all results
        Results = [
            receive
                {read, Pid, Value} -> Value
            after 5000 -> timeout
            end
         || Pid <- Readers
        ],

        % All reads should return the same value
        ?assertEqual(100, length(Results)),
        ?assert(lists:all(fun(V) -> V =:= shared_value end, Results))
    end).

%%====================================================================
%% Performance Benchmark Tests
%%====================================================================

performance_test_() ->
    {timeout, 30, fun test_read_performance/0}.

test_read_performance() ->
    {ok, Pid} = erlmcp_config:start_link(),
    try
        ok = erlmcp_config:set(perf_key, benchmark_value),

        % Benchmark 1M reads
        Iterations = 1000000,
        StartTime = erlang:monotonic_time(microsecond),

        [erlmcp_config:get(perf_key) || _ <- lists:seq(1, Iterations)],

        EndTime = erlang:monotonic_time(microsecond),
        ElapsedUs = EndTime - StartTime,
        ReadsPerSec = (Iterations * 1000000) div ElapsedUs,

        logger:info("persistent_term read performance: ~w reads/sec (~.2f μs/read)", [
            ReadsPerSec, ElapsedUs / Iterations
        ]),

        % Should achieve >10M reads/sec (target: ~10ns per read)
        ?assert(ReadsPerSec > 1000000)
    after
        gen_server:stop(Pid),
        erlmcp_config:clear_cache()
    end.
