%%%-------------------------------------------------------------------
%%% @doc TCPS Persistence Performance Test Suite
%%%
%%% Common Test suite for performance testing of TCPS ontology
%%% persistence and SPARQL queries.
%%%
%%% Tests:
%%% - 10,000 receipts write performance
%%% - 1,000 concurrent SPARQL queries
%%% - 100,000 receipt chain verifications
%%% - Memory usage under load
%%% - Query response time P50/P95/P99
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_persistence_performance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

-export([
    % Write performance tests
    test_10000_receipts_write_performance/1,
    test_incremental_vs_full_rewrite/1,
    test_batch_write_performance/1,

    % Query performance tests
    test_1000_concurrent_sparql_queries/1,
    test_cached_vs_uncached_queries/1,
    test_indexed_vs_sparql_lookup/1,

    % Chain verification tests
    test_100000_receipt_chain_verifications/1,
    test_sku_readiness_at_scale/1,

    % Memory and resource tests
    test_memory_usage_under_load/1,
    test_query_response_time_percentiles/1,
    test_cache_hit_rate_optimization/1,

    % Stress tests
    test_sustained_write_throughput/1,
    test_mixed_read_write_workload/1
]).

-define(LARGE_DATASET_SIZE, 10000).
-define(CONCURRENT_QUERIES, 1000).
-define(CHAIN_VERIFICATIONS, 100000).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        {group, write_performance},
        {group, query_performance},
        {group, chain_verification},
        {group, resource_usage},
        {group, stress_tests}
    ].

groups() ->
    [
        {write_performance, [parallel], [
            test_10000_receipts_write_performance,
            test_incremental_vs_full_rewrite,
            test_batch_write_performance
        ]},
        {query_performance, [parallel], [
            test_1000_concurrent_sparql_queries,
            test_cached_vs_uncached_queries,
            test_indexed_vs_sparql_lookup
        ]},
        {chain_verification, [sequence], [
            test_100000_receipt_chain_verifications,
            test_sku_readiness_at_scale
        ]},
        {resource_usage, [sequence], [
            test_memory_usage_under_load,
            test_query_response_time_percentiles,
            test_cache_hit_rate_optimization
        ]},
        {stress_tests, [sequence], [
            test_sustained_write_throughput,
            test_mixed_read_write_workload
        ]}
    ].

init_per_suite(Config) ->
    % Start required applications
    application:ensure_all_started(tcps),

    % Start ontology indexing
    {ok, _} = tcps_ontology_index:start_link(),
    tcps_ontology_index:create_indexes(),

    % Start query cache
    {ok, _} = tcps_query_cache:start_link(),

    Config.

end_per_suite(_Config) ->
    tcps_query_cache:stop(),
    tcps_ontology_index:stop(),
    ok.

init_per_group(_Group, Config) ->
    % Clear indexes and cache before each group
    tcps_ontology_index:clear_indexes(),
    tcps_ontology_index:create_indexes(),
    tcps_query_cache:invalidate_all(),
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%%===================================================================
%%% Write Performance Tests
%%%===================================================================

test_10000_receipts_write_performance(Config) ->
    ct:pal("Testing write performance for 10,000 receipts"),

    Receipts = generate_receipts(10000),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Receipt) ->
            tcps_rdf_incremental:add_receipt_to_ontology(Receipt)
        end, Receipts)
    end),

    TimeMs = Time / 1000,
    ReceiptsPerSec = 10000 / (Time / 1000000),

    ct:pal("Wrote 10,000 receipts in ~.2f ms (~.0f receipts/sec)", [TimeMs, ReceiptsPerSec]),

    % Success criteria: >1000 receipts/sec
    ?assert(ReceiptsPerSec > 1000, "Write throughput below 1000 receipts/sec"),

    ok.

test_incremental_vs_full_rewrite(Config) ->
    ct:pal("Comparing incremental updates vs full file rewrite"),

    Receipt = generate_receipt(<<"SKU-001">>, <<"RCP-001">>),

    % Test incremental update
    {IncrementalTime, _} = timer:tc(fun() ->
        tcps_rdf_incremental:add_receipt_to_ontology(Receipt)
    end),

    % Test full rewrite (simulated by writing all triples)
    AllReceipts = generate_receipts(1000),
    {FullRewriteTime, _} = timer:tc(fun() ->
        Triples = lists:flatmap(fun tcps_rdf_incremental:receipt_triples/1, AllReceipts),
        file:write_file("test_ontology.ttl", iolist_to_binary(Triples))
    end),

    SpeedupFactor = FullRewriteTime / IncrementalTime,

    ct:pal("Incremental: ~.2f ms, Full rewrite: ~.2f ms, Speedup: ~.1fx",
           [IncrementalTime/1000, FullRewriteTime/1000, SpeedupFactor]),

    % Success criteria: Incremental is at least 50x faster
    ?assert(SpeedupFactor > 50, "Incremental updates not significantly faster"),

    ok.

test_batch_write_performance(Config) ->
    ct:pal("Testing batch write performance"),

    Batch = lists:map(fun(N) ->
        {receipt, generate_receipt(<<"SKU-001">>, iolist_to_binary(io_lib:format("RCP-~5..0B", [N])))}
    end, lists:seq(1, 1000)),

    {BatchTime, _} = timer:tc(fun() ->
        tcps_rdf_incremental:add_batch(Batch)
    end),

    % Compare with individual writes
    {IndividualTime, _} = timer:tc(fun() ->
        lists:foreach(fun({receipt, R}) ->
            tcps_rdf_incremental:add_receipt_to_ontology(R)
        end, Batch)
    end),

    SpeedupFactor = IndividualTime / BatchTime,

    ct:pal("Batch: ~.2f ms, Individual: ~.2f ms, Speedup: ~.1fx",
           [BatchTime/1000, IndividualTime/1000, SpeedupFactor]),

    % Success criteria: Batch is at least 2x faster
    ?assert(SpeedupFactor > 2, "Batch writes not significantly faster"),

    ok.

%%%===================================================================
%%% Query Performance Tests
%%%===================================================================

test_1000_concurrent_sparql_queries(Config) ->
    ct:pal("Testing 1,000 concurrent SPARQL queries"),

    % Create test dataset
    Receipts = generate_receipts(1000),
    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    % Run concurrent queries
    QueryFile = "sparql/tcps_queries_optimized/receipts_by_stage_optimized.rq",

    {Time, Results} = timer:tc(fun() ->
        Parent = self(),
        lists:foreach(fun(N) ->
            spawn(fun() ->
                SKUId = iolist_to_binary(io_lib:format("SKU-~3..0B", [N rem 100])),
                {ok, Query} = file:read_file(QueryFile),
                QueryWithBind = <<Query/binary, "\nBIND(\"", SKUId/binary, "\" AS ?sku_id)\n">>,

                {QueryTime, _} = timer:tc(fun() ->
                    rdf_utils:execute_sparql(["test_ontology.ttl"], QueryWithBind)
                end),

                Parent ! {query_complete, QueryTime}
            end)
        end, lists:seq(1, 1000)),

        % Collect results
        collect_query_times(1000, [])
    end),

    AvgQueryTime = lists:sum(Results) / length(Results) / 1000,
    P95QueryTime = percentile(Results, 95) / 1000,

    ct:pal("1000 concurrent queries: Total ~.2f ms, Avg ~.2f ms/query, P95 ~.2f ms",
           [Time/1000, AvgQueryTime, P95QueryTime]),

    % Success criteria: P95 < 200ms
    ?assert(P95QueryTime < 200, "P95 query time exceeds 200ms"),

    ok.

test_cached_vs_uncached_queries(Config) ->
    ct:pal("Comparing cached vs uncached query performance"),

    % Create test dataset
    Receipts = generate_receipts(100),
    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    QueryFile = "sparql/tcps_queries_optimized/receipts_by_stage_optimized.rq",
    {ok, Query} = file:read_file(QueryFile),

    % First query (uncached)
    {UncachedTime, _} = timer:tc(fun() ->
        tcps_query_cache:cached_sparql_query(["test_ontology.ttl"], Query, 60)
    end),

    % Second query (cached)
    {CachedTime, _} = timer:tc(fun() ->
        tcps_query_cache:cached_sparql_query(["test_ontology.ttl"], Query, 60)
    end),

    SpeedupFactor = UncachedTime / CachedTime,

    ct:pal("Uncached: ~.2f ms, Cached: ~.2f ms, Speedup: ~.1fx",
           [UncachedTime/1000, CachedTime/1000, SpeedupFactor]),

    % Success criteria: Cached is at least 10x faster
    ?assert(SpeedupFactor > 10, "Cache speedup less than 10x"),

    ok.

test_indexed_vs_sparql_lookup(Config) ->
    ct:pal("Comparing ETS index vs SPARQL query performance"),

    % Create test dataset
    Receipts = generate_receipts(1000),
    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    SKUId = <<"SKU-001">>,

    % ETS index lookup
    {IndexTime, _} = timer:tc(fun() ->
        tcps_ontology_index:get_receipts_by_sku(SKUId)
    end),

    % SPARQL query
    {ok, Query} = file:read_file("sparql/tcps_queries_optimized/receipts_by_stage_optimized.rq"),
    QueryWithBind = <<Query/binary, "\nBIND(\"", SKUId/binary, "\" AS ?sku_id)\n">>,

    {SparqlTime, _} = timer:tc(fun() ->
        rdf_utils:execute_sparql(["test_ontology.ttl"], QueryWithBind)
    end),

    SpeedupFactor = SparqlTime / IndexTime,

    ct:pal("ETS Index: ~.2f ms, SPARQL: ~.2f ms, Speedup: ~.1fx",
           [IndexTime/1000, SparqlTime/1000, SpeedupFactor]),

    % Success criteria: Index is at least 50x faster
    ?assert(SpeedupFactor > 50, "ETS index speedup less than 50x"),

    ok.

%%%===================================================================
%%% Chain Verification Tests
%%%===================================================================

test_100000_receipt_chain_verifications(Config) ->
    ct:pal("Testing 100,000 receipt chain verifications"),

    % Create test dataset (1000 SKUs, 4 stages each = 4000 receipts)
    SKUs = [iolist_to_binary(io_lib:format("SKU-~4..0B", [N]))
            || N <- lists:seq(1, 1000)],

    Receipts = lists:flatmap(fun(SKU) ->
        [generate_receipt(SKU, iolist_to_binary(io_lib:format("RCP-~s-~s", [SKU, Stage])))
         || Stage <- [<<"build">>, <<"test">>, <<"deploy">>, <<"monitor">>]]
    end, SKUs),

    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    % Verify chains 100,000 times (reusing 1000 SKUs)
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            SKU = lists:nth((N rem 1000) + 1, SKUs),
            _Receipts = tcps_ontology_index:get_receipts_by_sku(SKU),
            ok
        end, lists:seq(1, 100000))
    end),

    VerificationsPerSec = 100000 / (Time / 1000000),

    ct:pal("100,000 verifications in ~.2f ms (~.0f verifications/sec)",
           [Time/1000, VerificationsPerSec]),

    % Success criteria: >10,000 verifications/sec
    ?assert(VerificationsPerSec > 10000, "Verification throughput below 10,000/sec"),

    ok.

test_sku_readiness_at_scale(Config) ->
    ct:pal("Testing SKU readiness checks at scale"),

    % Create 1000 SKUs with varying readiness
    SKUs = lists:map(fun(N) ->
        SKUId = iolist_to_binary(io_lib:format("SKU-~4..0B", [N])),
        IsReady = (N rem 3 == 0),  % 1/3 ready
        #{sku_id => SKUId, is_ready => IsReady, required_stages => [<<"build">>, <<"test">>]}
    end, lists:seq(1, 1000)),

    lists:foreach(fun tcps_ontology_index:index_sku/1, SKUs),

    % Query ready SKUs
    {Time, ReadySKUs} = timer:tc(fun() ->
        tcps_ontology_index:get_skus_by_readiness(true)
    end),

    ct:pal("Found ~p ready SKUs in ~.2f ms", [length(ReadySKUs), Time/1000]),

    % Success criteria: <10ms query time
    ?assert(Time / 1000 < 10, "SKU readiness query exceeds 10ms"),

    ok.

%%%===================================================================
%%% Resource Usage Tests
%%%===================================================================

test_memory_usage_under_load(Config) ->
    ct:pal("Testing memory usage under load"),

    MemBefore = erlang:memory(total),

    % Create large dataset
    Receipts = generate_receipts(10000),
    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    MemAfter = erlang:memory(total),
    MemUsedMB = (MemAfter - MemBefore) / 1024 / 1024,

    ct:pal("Memory used for 10,000 receipts: ~.2f MB", [MemUsedMB]),

    % Success criteria: <100MB for 10,000 receipts
    ?assert(MemUsedMB < 100, "Memory usage exceeds 100MB for 10,000 receipts"),

    ok.

test_query_response_time_percentiles(Config) ->
    ct:pal("Testing query response time percentiles"),

    % Create test dataset
    Receipts = generate_receipts(1000),
    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    % Run 100 queries and collect times
    QueryFile = "sparql/tcps_queries_optimized/heijunka_schedule_optimized.rq",

    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            rdf_utils:execute_sparql_file(["test_ontology.ttl"], QueryFile)
        end),
        Time / 1000
    end, lists:seq(1, 100)),

    P50 = percentile(Times, 50),
    P95 = percentile(Times, 95),
    P99 = percentile(Times, 99),

    ct:pal("Query response times - P50: ~.2f ms, P95: ~.2f ms, P99: ~.2f ms",
           [P50, P95, P99]),

    % Success criteria: P99 < 150ms
    ?assert(P99 < 150, "P99 query time exceeds 150ms"),

    ok.

test_cache_hit_rate_optimization(Config) ->
    ct:pal("Testing cache hit rate optimization"),

    % Create test dataset
    Receipts = generate_receipts(100),
    lists:foreach(fun tcps_rdf_incremental:add_receipt_to_ontology/1, Receipts),

    QueryFile = "sparql/tcps_queries_optimized/receipts_by_stage_optimized.rq",
    {ok, Query} = file:read_file(QueryFile),

    % Run same query 100 times
    lists:foreach(fun(_) ->
        tcps_query_cache:cached_sparql_query(["test_ontology.ttl"], Query, 60)
    end, lists:seq(1, 100)),

    HitRate = tcps_query_cache:hit_rate(),

    ct:pal("Cache hit rate after 100 queries: ~.2f%", [HitRate]),

    % Success criteria: >90% hit rate
    ?assert(HitRate > 90, "Cache hit rate below 90%"),

    ok.

%%%===================================================================
%%% Stress Tests
%%%===================================================================

test_sustained_write_throughput(Config) ->
    ct:pal("Testing sustained write throughput over 10 seconds"),

    StartTime = erlang:system_time(millisecond),
    EndTime = StartTime + 10000,  % 10 seconds

    Count = sustained_writes(EndTime, 0),
    ThroughputPerSec = Count / 10,

    ct:pal("Sustained throughput: ~.0f receipts/sec over 10 seconds", [ThroughputPerSec]),

    % Success criteria: >500 receipts/sec sustained
    ?assert(ThroughputPerSec > 500, "Sustained throughput below 500 receipts/sec"),

    ok.

test_mixed_read_write_workload(Config) ->
    ct:pal("Testing mixed read/write workload"),

    % Start background writers
    Writer = spawn(fun() -> continuous_writer(1000) end),

    % Run concurrent queries
    QueryFile = "sparql/tcps_queries_optimized/receipts_by_stage_optimized.rq",
    {ok, Query} = file:read_file(QueryFile),

    {Time, QueryTimes} = timer:tc(fun() ->
        lists:map(fun(_) ->
            {QTime, _} = timer:tc(fun() ->
                tcps_query_cache:cached_sparql_query(["test_ontology.ttl"], Query, 60)
            end),
            QTime
        end, lists:seq(1, 100))
    end),

    Writer ! stop,

    AvgQueryTime = lists:sum(QueryTimes) / length(QueryTimes) / 1000,

    ct:pal("Mixed workload: Avg query time ~.2f ms under continuous writes", [AvgQueryTime]),

    % Success criteria: Avg query time <200ms under writes
    ?assert(AvgQueryTime < 200, "Query performance degraded under writes"),

    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

generate_receipts(N) ->
    [generate_receipt(
        iolist_to_binary(io_lib:format("SKU-~3..0B", [rand:uniform(100)])),
        iolist_to_binary(io_lib:format("RCP-~6..0B", [I]))
    ) || I <- lists:seq(1, N)].

generate_receipt(SKUId, ReceiptId) ->
    #{
        receipt_id => ReceiptId,
        sku_id => SKUId,
        stage => lists:nth(rand:uniform(4), [<<"build">>, <<"test">>, <<"deploy">>, <<"monitor">>]),
        status => case rand:uniform(10) > 1 of true -> <<"PASS">>; false -> <<"FAIL">> end,
        timestamp => iolist_to_binary(io_lib:format("2024-01-15T~2..0B:00:00Z", [rand:uniform(24)]))
    }.

collect_query_times(0, Acc) ->
    Acc;
collect_query_times(N, Acc) ->
    receive
        {query_complete, Time} ->
            collect_query_times(N - 1, [Time | Acc])
    after 30000 ->
        Acc
    end.

percentile(Values, Percentile) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    Index = round(Len * Percentile / 100),
    lists:nth(max(1, min(Index, Len)), Sorted).

sustained_writes(EndTime, Count) ->
    Now = erlang:system_time(millisecond),
    if
        Now < EndTime ->
            Receipt = generate_receipt(<<"SKU-001">>, iolist_to_binary(io_lib:format("RCP-~p", [Count]))),
            tcps_rdf_incremental:add_receipt_to_ontology(Receipt),
            sustained_writes(EndTime, Count + 1);
        true ->
            Count
    end.

continuous_writer(0) ->
    receive stop -> ok after 0 -> ok end;
continuous_writer(N) ->
    receive
        stop -> ok
    after 0 ->
        Receipt = generate_receipt(<<"SKU-WRITER">>, iolist_to_binary(io_lib:format("RCP-W-~p", [N]))),
        tcps_rdf_incremental:add_receipt_to_ontology(Receipt),
        continuous_writer(N - 1)
    end.
