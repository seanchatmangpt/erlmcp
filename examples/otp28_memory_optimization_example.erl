-module(otp28_memory_optimization_example).
-modeldoc """
Example: OTP 28 Memory Optimization

This example demonstrates memory optimization techniques using OTP 28.3.1:
- ETS table compression
- Process dictionary caching
- Memory measurement and comparison

Run with:
  erl -pa _build/default/lib/erlmcp_core/ebin -s otp28_memory_optimization_example run
""" .

-export([run/0,
         %% ETS compression examples
         create_compressed_table/2,
         compare_ets_memory/0,
         %% Process dictionary cache examples
         cache_with_ttl/2,
         cache_demo/0]).

%%%===================================================================
%%% Example API
%%%===================================================================

%% @doc Run the complete example
run() ->
    io:format("~n=== OTP 28 Memory Optimization Example ===~n~n"),

    %% ETS compression demo
    io:format("1. ETS Table Compression Demo~n"),
    io:format("   Creating tables with 10000 entries...~n"),
    {UncompressedSize, CompressedSize} = compare_ets_memory(),
    Savings = UncompressedSize - CompressedSize,
    SavingsPct = (Savings / UncompressedSize) * 100,
    io:format("   Uncompressed table: ~p bytes~n", [UncompressedSize]),
    io:format("   Compressed table: ~p bytes~n", [CompressedSize]),
    io:format("   Memory saved: ~p bytes (~.1f%)~n~n", [Savings, SavingsPct]),

    %% Process dictionary cache demo
    io:format("2. Process Dictionary Cache Demo~n"),
    cache_demo(),
    io:format("~n"),

    io:format("=== Example Complete ===~n~n"),
    ok.

%%%===================================================================
%%% ETS Compression Examples
%%%===================================================================

%% @doc Create a compressed ETS table
create_compressed_table(Name, Options) ->
    erlmcp_otp28_upgrade:ets_compressed_table(Name, Options).

%% @doc Compare memory usage between compressed and uncompressed tables
compare_ets_memory() ->
    %% Create uncompressed table
    UncompressedTable = ets:new(uncompressed_example, [set, private]),

    %% Create compressed table
    CompressedTable = create_compressed_table(compressed_example, [set, private]),

    %% Insert test data (10000 text entries)
    NumEntries = 10000,
    lists:foreach(fun(N) ->
        Key = "key_" ++ integer_to_list(N),
        Value = lists:duplicate(100, $x),  % 100-character string
        true = ets:insert(UncompressedTable, {Key, Value}),
        true = ets:insert(CompressedTable, {Key, Value})
    end, lists:seq(1, NumEntries)),

    %% Get memory usage
    UncompressedInfo = ets:info(UncompressedTable, memory),
    CompressedInfo = ets:info(CompressedTable, memory),

    UncompressedSize = UncompressedInfo * erlang:system_info(wordsize),
    CompressedSize = CompressedInfo * erlang:system_info(wordsize),

    %% Cleanup
    ets:delete(UncompressedTable),
    ets:delete(CompressedTable),

    {UncompressedSize, CompressedSize}.

%%%===================================================================
%%% Process Dictionary Cache Examples
%%%===================================================================

%% @doc Cache value with TTL
cache_with_ttl(Key, {Value, TTL}) ->
    erlmcp_otp28_upgrade:process_dict_cache(Key, {Value, TTL}).

%% @doc Demonstrate process dictionary caching
cache_demo() ->
    io:format("   Caching expensive computation results...~n"),

    %% First call - expensive computation
    io:format("   Call 1: Computing...~n"),
    {T1, Result1} = timer:tc(fun() ->
        cache_with_ttl(expensive_calc, {expensive_computation(), 5000})
    end),
    io:format("   Time: ~.3fms, Result: ~p~n", [T1 / 1000, Result1]),

    %% Second call - cached (instant)
    io:format("   Call 2: Using cache...~n"),
    {T2, Result2} = timer:tc(fun() ->
        cache_with_ttl(expensive_calc, {should_not_compute, 5000})
    end),
    io:format("   Time: ~.3fms, Result: ~p~n", [T2 / 1000, Result2]),
    io:format("   Cache speedup: ~.1fx~n", [T1 / T2]),

    %% Wait for TTL to expire
    io:format("   Waiting for cache to expire (5s)...~n"),
    timer:sleep(5100),

    %% Third call - cache expired, recompute
    io:format("   Call 3: Cache expired, recomputing...~n"),
    {T3, _Result3} = timer:tc(fun() ->
        cache_with_ttl(expensive_calc, {recomputed, 5000})
    end),
    io:format("   Time: ~.3fms~n", [T3 / 1000]),

    ok.

%% @doc Simulate expensive computation
expensive_computation() ->
    %% Simulate work with sleep
    timer:sleep(100),
    computation_result.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Get ETS table memory usage in bytes
get_ets_memory(Table) ->
    MemoryWords = ets:info(Table, memory),
    MemoryWords * erlang:system_info(wordsize).

%% @doc Get process dictionary cache entry
get_cached_value(Key) ->
    case get({cache, Key}) of
        {Value, _Timestamp, _TTL} ->
            {ok, Value};
        undefined ->
            {error, not_found}
    end.

%% @doc Clear all cache entries
clear_cache() ->
    lists:foreach(fun({{cache, _Key}, _Value}) ->
        erase({cache, _Key})
    end, get()).
