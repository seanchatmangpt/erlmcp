%%%-------------------------------------------------------------------
%%% @doc TCPS Ontology Performance Benchmark Suite
%%%
%%% Benchmarks SPARQL query performance at different dataset sizes
%%% to identify bottlenecks and optimization opportunities.
%%%
%%% Tests:
%%% - Heijunka scheduling queries (10, 100, 1000, 10000 work orders)
%%% - Receipt chain queries (10, 100, 1000 SKUs)
%%% - Andon history queries (10, 100, 1000 Andon events)
%%% - Quality metrics aggregation
%%% - SKU readiness checks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_ontology_benchmark).

-export([
    benchmark_all/0,
    benchmark_heijunka_query/1,
    benchmark_receipt_chain_query/1,
    benchmark_andon_history_query/1,
    benchmark_quality_metrics_query/1,
    benchmark_sku_readiness_query/1,
    generate_report/1
]).

-include_lib("kernel/include/logger.hrl").

-define(BENCHMARK_SIZES, [10, 100, 1000, 10000]).
-define(ONTOLOGY_DIR, "test/fixtures/ontology").
-define(SPARQL_DIR, "sparql/tcps_queries").

%%%===================================================================
%%% API
%%%===================================================================

-spec benchmark_all() -> #{atom() => [benchmark_result()]}.
benchmark_all() ->
    ?LOG_INFO("Starting comprehensive TCPS ontology benchmark suite"),

    Results = #{
        heijunka => benchmark_all_sizes(heijunka),
        receipt_chain => benchmark_all_sizes(receipt_chain),
        andon_history => benchmark_all_sizes(andon_history),
        quality_metrics => benchmark_all_sizes(quality_metrics),
        sku_readiness => benchmark_all_sizes(sku_readiness)
    },

    generate_report(Results),
    Results.

-spec benchmark_heijunka_query(integer()) -> benchmark_result().
benchmark_heijunka_query(NumWorkOrders) ->
    ?LOG_INFO("Benchmarking heijunka query with ~p work orders", [NumWorkOrders]),

    % Create test dataset
    OntologyFile = create_test_work_orders(NumWorkOrders),
    QueryFile = filename:join(?SPARQL_DIR, "heijunka_schedule.rq"),

    % Warm-up run
    rdf_utils:execute_sparql_file([OntologyFile], QueryFile),

    % Benchmark runs (5 iterations)
    Times = lists:map(fun(_) ->
        {Time, _Result} = timer:tc(fun() ->
            rdf_utils:execute_sparql_file([OntologyFile], QueryFile)
        end),
        Time / 1000  % Convert to milliseconds
    end, lists:seq(1, 5)),

    % Calculate statistics
    #{
        query_type => heijunka,
        dataset_size => NumWorkOrders,
        avg_time_ms => lists:sum(Times) / length(Times),
        min_time_ms => lists:min(Times),
        max_time_ms => lists:max(Times),
        p50_time_ms => percentile(Times, 50),
        p95_time_ms => percentile(Times, 95),
        p99_time_ms => percentile(Times, 99),
        times => Times
    }.

-spec benchmark_receipt_chain_query(integer()) -> benchmark_result().
benchmark_receipt_chain_query(NumSKUs) ->
    ?LOG_INFO("Benchmarking receipt chain query with ~p SKUs", [NumSKUs]),

    % Create test dataset (each SKU has 4 stages with receipts)
    OntologyFile = create_test_receipts(NumSKUs),
    QueryFile = filename:join(?SPARQL_DIR, "receipts_by_stage.rq"),

    % Bind SKU ID parameter
    {ok, QueryTemplate} = file:read_file(QueryFile),
    Query = <<QueryTemplate/binary, "\nBIND(\"SKU-001\" AS ?sku_id)\n">>,

    % Warm-up
    rdf_utils:execute_sparql([OntologyFile], Query),

    % Benchmark runs
    Times = lists:map(fun(_) ->
        {Time, _Result} = timer:tc(fun() ->
            rdf_utils:execute_sparql([OntologyFile], Query)
        end),
        Time / 1000
    end, lists:seq(1, 5)),

    #{
        query_type => receipt_chain,
        dataset_size => NumSKUs,
        avg_time_ms => lists:sum(Times) / length(Times),
        min_time_ms => lists:min(Times),
        max_time_ms => lists:max(Times),
        p50_time_ms => percentile(Times, 50),
        p95_time_ms => percentile(Times, 95),
        p99_time_ms => percentile(Times, 99),
        times => Times
    }.

-spec benchmark_andon_history_query(integer()) -> benchmark_result().
benchmark_andon_history_query(NumAndons) ->
    ?LOG_INFO("Benchmarking andon history query with ~p Andon events", [NumAndons]),

    OntologyFile = create_test_andons(NumAndons),
    QueryFile = filename:join(?SPARQL_DIR, "andon_active.rq"),

    % Warm-up
    rdf_utils:execute_sparql_file([OntologyFile], QueryFile),

    Times = lists:map(fun(_) ->
        {Time, _Result} = timer:tc(fun() ->
            rdf_utils:execute_sparql_file([OntologyFile], QueryFile)
        end),
        Time / 1000
    end, lists:seq(1, 5)),

    #{
        query_type => andon_history,
        dataset_size => NumAndons,
        avg_time_ms => lists:sum(Times) / length(Times),
        min_time_ms => lists:min(Times),
        max_time_ms => lists:max(Times),
        p50_time_ms => percentile(Times, 50),
        p95_time_ms => percentile(Times, 95),
        p99_time_ms => percentile(Times, 99),
        times => Times
    }.

-spec benchmark_quality_metrics_query(integer()) -> benchmark_result().
benchmark_quality_metrics_query(NumWorkOrders) ->
    ?LOG_INFO("Benchmarking quality metrics query with ~p work orders", [NumWorkOrders]),

    OntologyFile = create_test_quality_data(NumWorkOrders),
    QueryFile = filename:join(?SPARQL_DIR, "quality_metrics.rq"),

    % Bind date parameters
    {ok, QueryTemplate} = file:read_file(QueryFile),
    Query = <<QueryTemplate/binary,
              "\nBIND(\"2024-01-01T00:00:00Z\"^^xsd:dateTime AS ?start_date)",
              "\nBIND(\"2024-01-31T23:59:59Z\"^^xsd:dateTime AS ?end_date)\n">>,

    % Warm-up
    rdf_utils:execute_sparql([OntologyFile], Query),

    Times = lists:map(fun(_) ->
        {Time, _Result} = timer:tc(fun() ->
            rdf_utils:execute_sparql([OntologyFile], Query)
        end),
        Time / 1000
    end, lists:seq(1, 5)),

    #{
        query_type => quality_metrics,
        dataset_size => NumWorkOrders,
        avg_time_ms => lists:sum(Times) / length(Times),
        min_time_ms => lists:min(Times),
        max_time_ms => lists:max(Times),
        p50_time_ms => percentile(Times, 50),
        p95_time_ms => percentile(Times, 95),
        p99_time_ms => percentile(Times, 99),
        times => Times
    }.

-spec benchmark_sku_readiness_query(integer()) -> benchmark_result().
benchmark_sku_readiness_query(NumSKUs) ->
    ?LOG_INFO("Benchmarking SKU readiness query with ~p SKUs", [NumSKUs]),

    OntologyFile = create_test_sku_data(NumSKUs),
    QueryFile = filename:join(?SPARQL_DIR, "sku_readiness.rq"),

    % Bind SKU parameter
    {ok, QueryTemplate} = file:read_file(QueryFile),
    Query = <<QueryTemplate/binary, "\nBIND(\"SKU-001\" AS ?sku_id)\n">>,

    % Warm-up
    rdf_utils:execute_sparql([OntologyFile], Query),

    Times = lists:map(fun(_) ->
        {Time, _Result} = timer:tc(fun() ->
            rdf_utils:execute_sparql([OntologyFile], Query)
        end),
        Time / 1000
    end, lists:seq(1, 5)),

    #{
        query_type => sku_readiness,
        dataset_size => NumSKUs,
        avg_time_ms => lists:sum(Times) / length(Times),
        min_time_ms => lists:min(Times),
        max_time_ms => lists:max(Times),
        p50_time_ms => percentile(Times, 50),
        p95_time_ms => percentile(Times, 95),
        p99_time_ms => percentile(Times, 99),
        times => Times
    }.

%%%===================================================================
%%% Test Data Generation
%%%===================================================================

create_test_work_orders(NumWorkOrders) ->
    Filename = filename:join(?ONTOLOGY_DIR,
                            io_lib:format("work_orders_~p.ttl", [NumWorkOrders])),
    filelib:ensure_dir(Filename),

    Prefixes = <<"@prefix tcps: <http://example.org/tcps#> .\n",
                 "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
                 "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n">>,

    Triples = lists:map(fun(N) ->
        WOId = io_lib:format("WO-~6..0B", [N]),
        Bucket = lists:nth(rand:uniform(4), ["reliability", "security", "cost", "compliance"]),
        Priority = lists:nth(rand:uniform(4), ["critical", "high", "medium", "low"]),
        Timestamp = io_lib:format("2024-01-~2..0BT~2..0B:00:00Z",
                                  [rand:uniform(31), rand:uniform(24)]),

        io_lib:format(
            "tcps:~s rdf:type tcps:WorkOrder ;\n"
            "  tcps:workOrderId \"~s\" ;\n"
            "  tcps:bucket \"~s\" ;\n"
            "  tcps:priority \"~s\" ;\n"
            "  tcps:status \"pending\" ;\n"
            "  tcps:demandSignal \"test-signal\" ;\n"
            "  tcps:createdTimestamp \"~s\"^^xsd:dateTime .\n\n",
            [WOId, WOId, Bucket, Priority, Timestamp])
    end, lists:seq(1, NumWorkOrders)),

    Content = iolist_to_binary([Prefixes | Triples]),
    file:write_file(Filename, Content),
    Filename.

create_test_receipts(NumSKUs) ->
    Filename = filename:join(?ONTOLOGY_DIR,
                            io_lib:format("receipts_~p.ttl", [NumSKUs])),
    filelib:ensure_dir(Filename),

    Prefixes = <<"@prefix tcps: <http://example.org/tcps#> .\n",
                 "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
                 "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n">>,

    Stages = [
        {<<"build">>, 1},
        {<<"test">>, 2},
        {<<"deploy">>, 3},
        {<<"monitor">>, 4}
    ],

    % Define production stages
    StageTriples = lists:map(fun({StageName, Seq}) ->
        io_lib:format(
            "tcps:Stage~s rdf:type tcps:ProductionStage ;\n"
            "  tcps:stageName \"~s\" ;\n"
            "  tcps:sequenceNumber ~p .\n\n",
            [StageName, StageName, Seq])
    end, Stages),

    % Create receipts for each SKU and stage
    ReceiptTriples = lists:flatmap(fun(N) ->
        SKUId = io_lib:format("SKU-~3..0B", [N]),
        lists:map(fun({StageName, Seq}) ->
            ReceiptId = io_lib:format("RCP-~s-~s", [SKUId, StageName]),
            Status = case rand:uniform(10) > 1 of true -> "PASS"; false -> "FAIL" end,
            Timestamp = io_lib:format("2024-01-15T~2..0B:~2..0B:00Z",
                                      [Seq * 2, rand:uniform(60)]),

            io_lib:format(
                "tcps:~s rdf:type tcps:Receipt ;\n"
                "  tcps:receiptId \"~s\" ;\n"
                "  tcps:skuId \"~s\" ;\n"
                "  tcps:stage \"~s\" ;\n"
                "  tcps:status \"~s\" ;\n"
                "  tcps:receiptTimestamp \"~s\"^^xsd:dateTime .\n\n",
                [ReceiptId, ReceiptId, SKUId, StageName, Status, Timestamp])
        end, Stages)
    end, lists:seq(1, NumSKUs)),

    Content = iolist_to_binary([Prefixes, StageTriples, ReceiptTriples]),
    file:write_file(Filename, Content),
    Filename.

create_test_andons(NumAndons) ->
    Filename = filename:join(?ONTOLOGY_DIR,
                            io_lib:format("andons_~p.ttl", [NumAndons])),
    filelib:ensure_dir(Filename),

    Prefixes = <<"@prefix tcps: <http://example.org/tcps#> .\n",
                 "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
                 "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n">>,

    Triples = lists:map(fun(N) ->
        AndonId = io_lib:format("ANDON-~6..0B", [N]),
        SKUId = io_lib:format("SKU-~3..0B", [rand:uniform(100)]),
        Severity = lists:nth(rand:uniform(4), ["critical", "high", "medium", "low"]),
        Status = case rand:uniform(10) > 3 of true -> "OPEN"; false -> "RESOLVED" end,
        Timestamp = io_lib:format("2024-01-~2..0BT~2..0B:00:00Z",
                                  [rand:uniform(31), rand:uniform(24)]),

        io_lib:format(
            "tcps:~s rdf:type tcps:AndonEvent ;\n"
            "  tcps:andonId \"~s\" ;\n"
            "  tcps:skuId \"~s\" ;\n"
            "  tcps:severity \"~s\" ;\n"
            "  tcps:status \"~s\" ;\n"
            "  tcps:failureReason \"Test failure reason\" ;\n"
            "  tcps:triggeredTimestamp \"~s\"^^xsd:dateTime .\n\n",
            [AndonId, AndonId, SKUId, Severity, Status, Timestamp])
    end, lists:seq(1, NumAndons)),

    Content = iolist_to_binary([Prefixes | Triples]),
    file:write_file(Filename, Content),
    Filename.

create_test_quality_data(NumWorkOrders) ->
    Filename = filename:join(?ONTOLOGY_DIR,
                            io_lib:format("quality_~p.ttl", [NumWorkOrders])),
    filelib:ensure_dir(Filename),

    Prefixes = <<"@prefix tcps: <http://example.org/tcps#> .\n",
                 "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
                 "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n">>,

    Triples = lists:map(fun(N) ->
        WOId = io_lib:format("WO-~6..0B", [N]),
        SKUId = io_lib:format("SKU-~3..0B", [rand:uniform(100)]),
        CreatedDay = rand:uniform(31),
        CompletedDay = CreatedDay + rand:uniform(5),
        Created = io_lib:format("2024-01-~2..0BT08:00:00Z", [CreatedDay]),
        Completed = io_lib:format("2024-01-~2..0BT17:00:00Z", [CompletedDay]),
        ReworkCount = rand:uniform(3) - 1,

        io_lib:format(
            "tcps:~s rdf:type tcps:WorkOrder ;\n"
            "  tcps:workOrderId \"~s\" ;\n"
            "  tcps:skuId \"~s\" ;\n"
            "  tcps:status \"completed\" ;\n"
            "  tcps:createdTimestamp \"~s\"^^xsd:dateTime ;\n"
            "  tcps:completedTimestamp \"~s\"^^xsd:dateTime ;\n"
            "  tcps:reworkCount ~p .\n\n",
            [WOId, WOId, SKUId, Created, Completed, ReworkCount])
    end, lists:seq(1, NumWorkOrders)),

    Content = iolist_to_binary([Prefixes | Triples]),
    file:write_file(Filename, Content),
    Filename.

create_test_sku_data(NumSKUs) ->
    Filename = filename:join(?ONTOLOGY_DIR,
                            io_lib:format("skus_~p.ttl", [NumSKUs])),
    filelib:ensure_dir(Filename),

    Prefixes = <<"@prefix tcps: <http://example.org/tcps#> .\n",
                 "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
                 "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n">>,

    Stages = ["build", "test", "deploy", "monitor"],

    Triples = lists:flatmap(fun(N) ->
        SKUId = io_lib:format("SKU-~3..0B", [N]),

        % SKU definition
        SKUTriple = io_lib:format(
            "tcps:~s rdf:type tcps:SKU ;\n"
            "  tcps:skuId \"~s\" ;\n"
            "  tcps:requiresStage \"build\" ;\n"
            "  tcps:requiresStage \"test\" ;\n"
            "  tcps:requiresStage \"deploy\" ;\n"
            "  tcps:requiresStage \"monitor\" .\n\n",
            [SKUId, SKUId]),

        % Receipts for this SKU
        ReceiptTriples = lists:map(fun(Stage) ->
            ReceiptId = io_lib:format("RCP-~s-~s", [SKUId, Stage]),
            Status = case rand:uniform(10) > 1 of true -> "PASS"; false -> "FAIL" end,

            io_lib:format(
                "tcps:~s rdf:type tcps:Receipt ;\n"
                "  tcps:receiptId \"~s\" ;\n"
                "  tcps:skuId \"~s\" ;\n"
                "  tcps:stage \"~s\" ;\n"
                "  tcps:status \"~s\" .\n\n",
                [ReceiptId, ReceiptId, SKUId, Stage, Status])
        end, Stages),

        [SKUTriple | ReceiptTriples]
    end, lists:seq(1, NumSKUs)),

    Content = iolist_to_binary([Prefixes | Triples]),
    file:write_file(Filename, Content),
    Filename.

%%%===================================================================
%%% Utilities
%%%===================================================================

benchmark_all_sizes(QueryType) ->
    lists:map(fun(Size) ->
        case QueryType of
            heijunka -> benchmark_heijunka_query(Size);
            receipt_chain -> benchmark_receipt_chain_query(Size);
            andon_history -> benchmark_andon_history_query(Size);
            quality_metrics -> benchmark_quality_metrics_query(Size);
            sku_readiness -> benchmark_sku_readiness_query(Size)
        end
    end, ?BENCHMARK_SIZES).

percentile(Values, Percentile) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    Index = round(Len * Percentile / 100),
    lists:nth(max(1, min(Index, Len)), Sorted).

-spec generate_report(#{atom() => [benchmark_result()]}) -> ok.
generate_report(Results) ->
    ReportFile = "docs/SPARQL_BENCHMARK_REPORT.md",
    filelib:ensure_dir(ReportFile),

    Content = [
        "# TCPS SPARQL Query Performance Benchmark Report\n\n",
        "Generated: ", format_timestamp(erlang:system_time(second)), "\n\n",
        "## Summary\n\n",
        format_summary_table(Results),
        "\n\n## Detailed Results\n\n",
        format_detailed_results(Results)
    ],

    file:write_file(ReportFile, iolist_to_binary(Content)),
    ?LOG_INFO("Benchmark report generated: ~s", [ReportFile]),
    ok.

format_summary_table(Results) ->
    [
        "| Query Type | Dataset Size | Avg Time (ms) | P95 Time (ms) | P99 Time (ms) |\n",
        "|------------|--------------|---------------|---------------|---------------|\n",
        maps:fold(fun(QueryType, Benchmarks, Acc) ->
            [Acc | lists:map(fun(#{dataset_size := Size, avg_time_ms := Avg,
                                    p95_time_ms := P95, p99_time_ms := P99}) ->
                io_lib:format("| ~s | ~p | ~.2f | ~.2f | ~.2f |\n",
                              [QueryType, Size, Avg, P95, P99])
            end, Benchmarks)]
        end, [], Results)
    ].

format_detailed_results(Results) ->
    maps:fold(fun(QueryType, Benchmarks, Acc) ->
        [Acc, io_lib:format("### ~s\n\n", [QueryType]),
         lists:map(fun(Benchmark) ->
             format_benchmark_detail(Benchmark)
         end, Benchmarks)]
    end, [], Results).

format_benchmark_detail(#{query_type := Type, dataset_size := Size,
                          avg_time_ms := Avg, min_time_ms := Min,
                          max_time_ms := Max, times := Times}) ->
    io_lib:format(
        "**Dataset Size: ~p**\n"
        "- Average: ~.2f ms\n"
        "- Min: ~.2f ms\n"
        "- Max: ~.2f ms\n"
        "- All runs: ~s\n\n",
        [Size, Avg, Min, Max,
         string:join([io_lib:format("~.2f", [T]) || T <- Times], ", ")]).

format_timestamp(UnixTime) ->
    {{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(
        UnixTime + 62167219200),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Y, M, D, H, Min, S]).

%%%===================================================================
%%% Types
%%%===================================================================

-type benchmark_result() :: #{
    query_type := atom(),
    dataset_size := integer(),
    avg_time_ms := float(),
    min_time_ms := float(),
    max_time_ms := float(),
    p50_time_ms := float(),
    p95_time_ms := float(),
    p99_time_ms := float(),
    times := [float()]
}.
