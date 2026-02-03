%% @doc Database Performance Test for erlmcp v3
%% Comprehensive testing of database performance under load
%%
%% Features:
%% - ETS performance testing
%% - DETS performance testing
%% - Mnesia performance testing
%% - Query optimization analysis
%%- Transaction throughput measurement
%%- Connection pool testing
%%- Index performance testing
%%- Data consistency validation

-module(erlmcp_database_performance_test).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, run_database_test/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.database_scenario, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    database_type :: ets | dets | mnesia,
    operation_pattern :: read_heavy | write_heavy | mixed | transactional,
    data_size :: integer(), % bytes per record
    record_count :: integer(),
    concurrency_level :: integer(),
    query_types :: list(),
    index_config :: list()
}.

-record.database_test, {
    id :: binary(),
    scenario :: #database_scenario{},
    start_time :: integer(),
    end_time :: integer(),
    status :: planning | running | completed | failed,
    metrics :: map(),
    throughput :: map(),
    latency :: map(),
    error_rate :: float(),
    resource_usage :: map(),
    query_analysis :: map()
}.

-record.query_metrics, {
    query_type :: binary(),
    execution_count :: integer(),
    average_latency :: float(),
    p95_latency :: float(),
    p99_latency :: float(),
    min_latency :: float(),
    max_latency :: float(),
    success_rate :: float(),
    error_count :: integer()
}.

record.database_metrics, {
    timestamp :: integer(),
    db_type :: binary(),
    operation :: binary(),
    records_processed :: integer(),
    throughput :: float(), % operations/sec
    latency :: float(), % ms
    memory_usage :: integer(),
    disk_io :: integer(),
    cpu_usage :: float(),
    concurrent_operations :: integer(),
    table_size :: integer(),
    cache_hit_ratio :: float()
}.

-record.test_config, {
    duration :: integer(),
    warmup_duration :: integer(),
    ramp_up_duration :: integer(),
    think_time :: integer(),
    retry_policy :: {integer(), integer()},
    measurement_interval :: integer(),
    consistency_check_interval :: integer(),
    cleanup_after_test :: boolean()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_database_test(TestId, Scenario, Config) ->
    gen_server:call(?MODULE, {run_database_test, TestId, Scenario, Config}, infinity).

stop() ->
    gen_server:call(?MODULE, stop).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Database performance test framework initialized"),

    %% Initialize state
    State = #{
        active_tests => [],
        database_metrics => initialize_database_metrics(),
        performance_baselines => initialize_performance_baselines(),
        test_history => [],
        database_connections => initialize_database_connections()
    },

    %% Start monitoring
    erlang:send_after(5000, self(), monitor_database),

    {ok, State}.

handle_call({run_database_test, TestId, Scenario, Config}, _From, State) ->
    %% Validate scenario
    case validate_database_scenario(Scenario) of
        {ok, ValidScenario} ->
            %% Create and start database test
            Test = create_database_test(TestId, ValidScenario, Config),
            NewState = start_database_test(Test, State),
            {reply, {ok, test_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_active_tests, _From, State) ->
    {reply, {ok, maps:get(active_tests, State, [])}, State};

handle_call(get_test_history, _From, State) ->
    {reply, {ok, maps:get(test_history, State, [])}, State};

handle_call(get_database_metrics, _From, State) ->
    {reply, {ok, maps:get(database_metrics, State, [])}, State};

handle_call(get_performance_baselines, _From, State) ->
    {reply, {ok, maps:get(performance_baselines, State, [])}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_database, State) ->
    %% Monitor database performance
    DatabaseMetrics = monitor_database_performance(State);
    UpdatedState = State#{
        database_metrics => DatabaseMetrics
    },

    %% Schedule next monitoring
    erlang:send_after(5000, self(), monitor_database),
    {noreply, UpdatedState};

handle_info({test_phase, TestId, Phase, Metrics}, State) ->
    %% Handle test phase transitions
    case lists:keyfind(TestId, #database_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            UpdatedTest = Test#database_test{
                status = Phase,
                metrics = merge_metrics(Test#database_test.metrics, Metrics)
            },

            NewState = State#{
                active_tests => lists:keyreplace(TestId, #database_test.id,
                                              maps:get(active_tests, State), UpdatedTest)
            },

            case Phase of
                running ->
                    %% Start database operations
                    erlang:send_after(0, self(), {start_database_operations, TestId});
                completed ->
                    %% Generate final report
                    Report = generate_database_test_report(Test, State);
                    erlang:send_after(0, self(), {test_complete, TestId, Report});
                _ ->
                    ok
            end,

            {noreply, NewState}
    end;

handle_info({start_database_operations, TestId}, State) ->
    %% Start database operations for test
    case lists:keyfind(TestId, #database_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Create test data
            case create_test_data(Test) of
                {ok, DataCount} ->
                    %% Start load testing
                    start_load_test(Test, DataCount);
                {error, Reason} ->
                    ?LOG_ERROR("Failed to create test data: ~p", [Reason]),
                    UpdatedTest = Test#database_test{
                        status = failed,
                        end_time => erlang:system_time(millisecond)
                    },
                    {noreply, update_test(TestId, UpdatedTest, State)}
            end
    end;

handle_info({database_operation, TestId, OpType, Metrics}, State) ->
    %% Handle database operation metrics
    case lists:keyfind(TestId, #database_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            UpdatedTest = update_test_metrics(TestId, OpType, Metrics, Test, State),
            {noreply, update_test(TestId, UpdatedTest, State)}
    end;

handle_info({consistency_check, TestId, CheckId, Result}, State) ->
    %% Handle consistency check results
    ?LOG_DEBUG("Consistency check ~p for test ~p: ~p", [CheckId, TestId, Result]),

    UpdatedState = lists:map(fun(Test) ->
        case Test#database_test.id =:= TestId of
            true ->
                UpdatedTest = Test#database_test{
                    query_analysis = maps:put(CheckId, Result, Test#database_test.query_analysis)
                };
            false ->
                Test
        end
    end, maps:get(active_tests, State)),

    State#{
        active_tests => UpdatedState
    };

handle_info({test_complete, TestId, Report}, State) ->
    %% Handle test completion
    ?LOG_INFO("Database test ~p completed", [TestId]),

    case lists:keyfind(TestId, #database_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Move to history
            NewHistory = [Test | maps:get(test_history, State)],
            NewActive = lists:keydelete(TestId, #database_test.id, maps:get(active_tests, State)),

            %% Save report
            ReportFile = "/Users/sac/erlmcp/load-testing/database_test_" ++
                         binary_to_list(TestId) ++ "_report.json",
            file:write_file(ReportFile, jsx:encode(Report)),

            ?LOG_INFO("Database test report saved to: ~p", [ReportFile]),

            State#{
                active_tests => NewActive,
                test_history => NewHistory
            }
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup database tests
    lists:foreach(fun(Test) ->
        cleanup_test(Test)
    end, maps:get(active_tests, State)),

    ?LOG_INFO("Database performance test framework terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_database_scenario(Scenario) ->
    %% Validate database scenario
    case Scenario#database_scenario.database_type of
        ets ->
            validate_ets_scenario(Scenario);
        dets ->
            validate_dets_scenario(Scenario);
        mnesia ->
            validate_mnesia_scenario(Scenario);
        _ ->
            {error, unsupported_database_type}
    end.

validate_ets_scenario(Scenario) ->
    %% Validate ETS scenario
    case Scenario#database_scenario.operation_pattern of
        read_heavy ->
            {ok, Scenario};
        write_heavy ->
            {ok, Scenario};
        mixed ->
            {ok, Scenario};
        transactional ->
            {error, ets_not_transactional};
        _ ->
            {error, invalid_operation_pattern}
    end.

validate_dets_scenario(Scenario) ->
    %% Validate DETS scenario
    case Scenario#database_scenario.operation_pattern of
        read_heavy ->
            {ok, Scenario};
        write_heavy ->
            {ok, Scenario};
        mixed ->
            {ok, Scenario};
        transactional ->
            {error, dets_not_transactional};
        _ ->
            {error, invalid_operation_pattern}
    end.

validate_mnesia_scenario(Scenario) ->
    %% Validate Mnesia scenario
    case Scenario#database_scenario.operation_pattern of
        read_heavy ->
            {ok, Scenario};
        write_heavy ->
            {ok, Scenario};
        mixed ->
            {ok, Scenario};
        transactional ->
            {ok, Scenario};
        _ ->
            {error, invalid_operation_pattern}
    end.

create_database_test(TestId, Scenario, Config) ->
    %% Create database test
    #database_test{
        id = TestId,
        scenario = Scenario,
        start_time = erlang:system_time(millisecond),
        status = planning,
        metrics = #{},
        throughput => #{},
        latency => #{},
        error_rate => 0.0,
        resource_usage => #{},
        query_analysis => #{}
    }.

start_database_test(Test, State) ->
    %% Start database test
    ?LOG_INFO("Starting database test: ~p", [Test#database_test.id]),

    %% Create database tables
    create_database_tables(Test),

    %% Add to active tests
    NewActive = [Test | maps:get(active_tests, State)],

    %% Send phase transition
    erlang:send_after(0, self(), {test_phase, Test#database_test.id, running, #{}}),

    State#{
        active_tests => NewActive
    }.

create_database_tables(Test) ->
    %% Create database tables based on scenario
    Scenario = Test#database_test.scenario;

    case Scenario#database_scenario.database_type of
        ets ->
            create_ets_tables(Scenario);
        dets ->
            create_dets_tables(Scenario);
        mnesia ->
            create_mnesia_tables(Scenario)
    end.

create_ets_tables(Scenario) ->
    %% Create ETS tables
    TableName = list_to_atom("test_table_" ++ binary_to_list(Scenario#database_scenario.id)),
    Options = [public, {read_concurrency, true}, {write_concurrency, true}],

    ets:new(TableName, Options),
    ?LOG_INFO("Created ETS table: ~p", [TableName]).

create_dets_tables(Scenario) ->
    %% Create DETS tables
    FileName = "/tmp/test_db_" ++ binary_to_list(Scenario#database_scenario.id) ++ ".dets",
    Options = [auto_save, {min_no_slots, 1000}],

    case dets:open_file(FileName, Options) of
        {ok, Table} ->
            ?LOG_INFO("Created DETS table: ~p", [Table]);
        {error, Reason} ->
            ?LOG_ERROR("Failed to create DETS table: ~p", [Reason])
    end.

create_mnesia_tables(Scenario) ->
    %% Create Mnesia tables
    TableName = list_to_atom("test_table_" ++ binary_to_list(Scenario#database_scenario.id)),

    case mnesia:create_table(TableName, [
        {attributes, record_info(fields, test_record)},
        {ram_copies, [node()]},
        {type, set},
        {index, Scenario#database_scenario.index_config}
    ]) of
        {atomic, ok} ->
            ?LOG_INFO("Created Mnesia table: ~p", [TableName]);
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to create Mnesia table: ~p", [Reason])
    end.

create_test_data(Test) ->
    %% Create test data
    Scenario = Test#database_test.scenario;
    RecordCount = Scenario#database_scenario.record_count;
    DataSize = Scenario#database_scenario.data_size;

    case Scenario#database_scenario.database_type of
        ets ->
            create_ets_test_data(Scenario, RecordCount, DataSize);
        dets ->
            create_dets_test_data(Scenario, RecordCount, DataSize);
        mnesia ->
            create_mnesia_test_data(Scenario, RecordCount, DataSize)
    end.

create_ets_test_data(Scenario, RecordCount, DataSize) ->
    %% Create test data for ETS
    TableName = list_to_atom("test_table_" ++ binary_to_list(Scenario#database_scenario.id));

    lists:foreach(fun(I) ->
        Record = generate_test_record(I, DataSize),
        ets:insert(TableName, {I, Record})
    end, lists:seq(1, RecordCount)),

    {ok, RecordCount}.

create_dets_test_data(Scenario, RecordCount, DataSize) ->
    %% Create test data for DETS
    FileName = "/tmp/test_db_" ++ binary_to_list(Scenario#database_scenario.id) ++ ".dets";

    case dets:open_file(FileName, []) of
        {ok, Table} ->
            lists:foreach(fun(I) ->
                Record = generate_test_record(I, DataSize),
                dets:insert(Table, {I, Record})
            end, lists:seq(1, RecordCount)),
            dets:close(Table),
            {ok, RecordCount};
        {error, Reason} ->
            {error, Reason}
    end.

create_mnesia_test_data(Scenario, RecordCount, DataSize) ->
    %% Create test data for Mnesia
    TableName = list_to_atom("test_table_" ++ binary_to_list(Scenario#database_scenario.id));

    F = fun() ->
        lists:foreach(fun(I) ->
            Record = generate_test_record(I, DataSize),
            mnesia:write(TableName, {I, Record}, write)
        end, lists:seq(1, RecordCount))
    end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            {ok, RecordCount};
        {aborted, Reason} ->
            {error, Reason}
    end.

generate_test_record(Index, DataSize) ->
    %% Generate test record
    Data = generate_random_data(DataSize),
    #{
        id => Index,
        data => Data,
        timestamp => erlang:system_time(millisecond),
        metadata => generate_metadata()
    }.

generate_random_data(Size) ->
    %% Generate random test data
    case Size > 0 of
        true ->
            crypto:strong_rand_bytes(Size);
        false ->
            <<>>
    end.

generate_metadata() ->
    %% Generate metadata
    #{
        created_by => test_framework,
        version => "1.0",
        tags => generate_tags()
    }.

generate_tags() ->
    %% Generate random tags
    lists:map(fun(_) ->
        "tag_" ++ integer_to_list(crypto:rand_uniform(1, 1000))
    end, lists:seq(1, 5)).

start_load_test(Test, DataCount) ->
    %% Start load testing
    Scenario = Test#database_test.scenario;
    Config = Test#database_test.config;

    %% Configure load generator
    LoadConfig = #{
        test_id => Test#database_test.id,
        database_type => Scenario#database_scenario.database_type,
        operation_pattern => Scenario#database_scenario.operation_pattern,
        record_count => DataCount,
        concurrency_level => Scenario#database_scenario.concurrency_level,
        query_types => Scenario#database_scenario.query_types,
        duration => Config#duration,
        think_time => Config#think_time,
        retry_policy => Config#retry_policy
    },

    %% Start load generator
    erlmcp_load_generator:start(LoadConfig).

monitor_database_performance(State) ->
    %% Monitor database performance
    case get_active_database_tests() of
        [] ->
            #{
                timestamp => erlang:system_time(millisecond),
                active_tests => 0,
                total_throughput => 0.0,
                avg_latency => 0.0,
                memory_usage => 0.0,
                disk_io => 0.0
            };
        _ ->
            %% Monitor all active database tests
            AggregateMetrics = aggregate_database_metrics(State);

            #{
                timestamp => erlang:system_time(millisecond),
                active_tests => length(get_active_database_tests()),
                total_throughput => AggregateMetrics#throughput,
                avg_latency => AggregateMetrics#latency,
                memory_usage => AggregateMetrics#memory_usage,
                disk_io => AggregateMetrics#disk_io
            }
    end.

aggregate_database_metrics(State) ->
    %% Aggregate metrics from all active tests
    Tests = maps:get(active_tests, State);

    lists:foldl(fun(Test, Acc) ->
        TestMetrics = Test#database_test.metrics;
        Acc#{
            throughput => Acc#throughput + TestMetrics#throughput,
            latency => Acc#latency + TestMetrics#latency,
            memory_usage => Acc#memory_usage + TestMetrics#memory_usage,
            disk_io => Acc#disk_io + TestMetrics#disk_io
        }
    end, #{throughput => 0.0, latency => 0.0, memory_usage => 0.0, disk_io => 0.0}, Tests).

get_active_database_tests() ->
    %% Get all active database tests
    case erlmcp_test_manager:get_active_tests() of
        {ok, Tests} ->
            [T || T <- Tests, is_database_test(T)];
        _ ->
            []
    end.

is_database_test(Test) ->
    %% Check if test is database-related
    case maps:find(database_type, Test#metadata) of
        {ok, _} ->
            true;
        error ->
            false
    end.

update_test_metrics(TestId, OpType, Metrics, Test, State) ->
    %% Update test metrics
    NewMetrics = Metrics#{
        timestamp => erlang:system_time(millisecond),
        operation => OpType
    };

    UpdatedThroughput = case maps:find(OpType, Test#database_test.throughput) of
        {ok, Current} ->
            Test#database_test.throughput#{OpType => Current + Metrics#throughput};
        error ->
            Test#database_test.throughput#{OpType => Metrics#throughput}
    end;

    UpdatedLatency = case maps:find(OpType, Test#database_test.latency) of
        {ok, Current} ->
            Test#database_test.latency#{OpType => merge_latencies(Current, Metrics#latency)};
        error ->
            Test#database_test.latency#{OpType => Metrics#latency}
    end;

    UpdatedErrorRate = calculate_error_rate(Test#database_test.metrics, Metrics);

    Test#database_test{
        metrics => NewMetrics,
        throughput => UpdatedThroughput,
        latency => UpdatedLatency,
        error_rate => UpdatedErrorRate
    }.

merge_latencies(CurrentLatencies, NewLatencies) ->
    %% Merge latency metrics
    #{
        average => (CurrentLatencies#average * 10 + NewLatencies#average) / 11,
        min => min(CurrentLatencies#min, NewLatencies#min),
        max => max(CurrentLatencies#max, NewLatencies#max),
        p95 => calculate_percentile(CurrentLatencies ++ [NewLatencies], 95),
        p99 => calculate_percentile(CurrentLatencies ++ [NewLatencies], 99)
    }.

calculate_percentile(Values, Percentile) ->
    %% Calculate percentile value
    Sorted = lists:sort(Values),
    Index = trunc((Percentile / 100) * length(Sorted)),
    lists:nth(Index, Sorted).

calculate_error_rate(CurrentMetrics, NewMetrics) ->
    %% Calculate error rate
    TotalOps = CurrentMetrics#operations_processed + NewMetrics#operations_processed;
    TotalErrors = CurrentMetrics#error_count + NewMetrics#error_count;

    case TotalOps > 0 of
        true ->
            TotalErrors / TotalOps * 100;
        false ->
            0.0
    end.

initialize_database_metrics() ->
    %% Initialize database metrics
    #{
        timestamp => erlang:system_time(millisecond),
        active_tests => 0,
        total_throughput => 0.0,
        avg_latency => 0.0,
        memory_usage => 0.0,
        disk_io => 0.0,
        query_analysis => #{}
    }.

initialize_performance_baselines() ->
    %% Initialize performance baselines
    #{
        ets => #{
            read_throughput => 10000,
            write_throughput => 5000,
            read_latency => 1.0,
            write_latency => 2.0,
            memory_usage => 100
        },
        dets => #{
            read_throughput => 5000,
            write_throughput => 2000,
            read_latency => 5.0,
            write_latency => 10.0,
            memory_usage => 200
        },
        mnesia => #{
            read_throughput => 3000,
            write_throughput => 1500,
            read_latency => 10.0,
            write_latency => 20.0,
            memory_usage => 300
        }
    }.

initialize_database_connections() ->
    %% Initialize database connections
    #{
        ets => #{},
        dets => #{},
        mnesia => #{}
    }.

generate_database_test_report(Test, State) ->
    %% Generate database test report
    Scenario = Test#database_test.scenario;
    Metrics = Test#database_test.metrics;
    Throughput = Test#database_test.throughput;
    Latency = Test#database_test.latency;

    Report = #{
        test_id => Test#database_test.id,
        scenario_name => Scenario#database_scenario.name,
        database_type => Scenario#database_scenario.database_type,
        operation_pattern => Scenario#database_scenario.operation_pattern,
        start_time => Test#database_test.start_time,
        end_time => Test#database_test.end_time,
        duration => Test#database_test.end_time - Test#database_test.start_time,
        metrics => Metrics,
        throughput => Throughput,
        latency => Latency,
        error_rate => Test#database_test.error_rate,
        query_analysis => Test#database_test.query_analysis,
        recommendations => generate_recommendations(Test, State),
        baseline_comparison => compare_with_baseline(Scenario, Metrics)
    },

    Report.

generate_recommendations(Test, State) ->
    %% Generate test recommendations
    Scenario = Test#database_scenario;
    Metrics = Test#database_test.metrics;

    Baselines = maps:get(performance_baselines, State);
    CurrentBaseline = maps:get(Scenario#database_scenario.database_type, Baselines);

    Recommendations = case Metrics#throughput < CurrentBaseline#read_throughput of
        true ->
            [#{recommendation => "Consider increasing read_concurrency for ETS tables",
               priority => high}];
        false ->
            [#{recommendation => "Read performance is acceptable",
               priority => low}]
    end,

    case Metrics#latency > CurrentBaseline#read_latency * 2 of
        true ->
            [#{recommendation => "High latency detected. Consider optimizing queries",
               priority => high} | Recommendations];
        false ->
            [#{recommendation => "Latency is within acceptable range",
               priority => low} | Recommendations]
    end.

compare_with_baseline(Scenario, Metrics) ->
    %% Compare metrics with baselines
    Baseline = get_performance_baseline(Scenario#database_scenario.database_type);

    #{
        read_throughput => #{
            baseline => Baseline#read_throughput,
            actual => Metrics#throughput,
            improvement => ((Metrics#throughput - Baseline#read_throughput) / Baseline#read_throughput) * 100
        },
        write_throughput => #{
            baseline => Baseline#write_throughput,
            actual => Metrics#throughput,
            improvement => ((Metrics#throughput - Baseline#write_throughput) / Baseline#write_throughput) * 100
        },
        read_latency => #{
            baseline => Baseline#read_latency,
            actual => Metrics#latency,
            improvement => ((Baseline#read_latency - Metrics#latency) / Baseline#read_latency) * 100
        }
    }.

get_performance_baseline(DatabaseType) ->
    %% Get performance baseline for database type
    Baselines = #{
        ets => #{read_throughput => 10000, write_throughput => 5000, read_latency => 1.0, write_latency => 2.0},
        dets => #{read_throughput => 5000, write_throughput => 2000, read_latency => 5.0, write_latency => 10.0},
        mnesia => #{read_throughput => 3000, write_throughput => 1500, read_latency => 10.0, write_latency => 20.0}
    },

    maps:get(DatabaseType, Baselines).

cleanup_test(Test) ->
    %% Cleanup test resources
    Scenario = Test#database_test.scenario;

    case Scenario#database_scenario.database_type of
        ets ->
            cleanup_ets_test(Scenario);
        dets ->
            cleanup_dets_test(Scenario);
        mnesia ->
            cleanup_mnesia_test(Scenario)
    end.

cleanup_ets_test(Scenario) ->
    %% Cleanup ETS test
    TableName = list_to_atom("test_table_" ++ binary_to_list(Scenario#database_scenario.id)),
    ets:delete(TableName),
    ?LOG_INFO("Cleaned up ETS table: ~p", [TableName]).

cleanup_dets_test(Scenario) ->
    %% Cleanup DETS test
    FileName = "/tmp/test_db_" ++ binary_to_list(Scenario#database_scenario.id) ++ ".dets",
    file:delete(FileName),
    ?LOG_INFO("Cleaned up DETS file: ~p", [FileName]).

cleanup_mnesia_test(Scenario) ->
    %% Cleanup Mnesia test
    TableName = list_to_atom("test_table_" ++ binary_to_list(Scenario#database_scenario.id)),
    mnesia:delete_table(TableName),
    ?LOG_INFO("Cleaned up Mnesia table: ~p", [TableName]).