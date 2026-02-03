-module(erlmcp_database_test).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

-behaviour(gen_server).

%% API exports
-export([
    start/0,
    stop/0,
    execute_load_tests/0,
    analyze_performance/1,
    identify_bottlenecks/1,
    validate_scaling/1
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
## TYPE DEFINITIONS
##====================================================================

-type database_query() :: #{
    id := binary(),
    type := select | insert | update | delete | batch,
    table := binary(),
    query := binary(),
    parameters := list(),
    expected_rows => integer(),
    timeout => pos_integer()
}.

-type load_test_scenario() :: #{
    name := binary(),
    description := binary(),
    query_patterns := [database_query()],
    concurrency_level := pos_integer(),
    duration := pos_integer(),
    ramp_up_duration => pos_integer(),
    think_time => pos_integer(),
    error_rate => float(),
    data_volume => pos_integer()
}.

-type database_metric() :: #{
    timestamp := integer(),
    query_type := binary(),
    latency := integer(),
    rows_affected := integer(),
    success := boolean(),
    error := binary() | undefined,
    query_size => integer(),
    result_size => integer(),
    connection_id => binary(),
    pool_name => binary()
}.

-type database_analysis() :: #{
    throughput => float(),
    average_latency => float(),
    p95_latency => integer(),
    p99_latency => integer(),
    success_rate => float(),
    error_rate => float(),
    query_distribution => map(),
    connection_pool_efficiency => float(),
    cache_hit_rate => float(),
    index_efficiency => map(),
    bottlenecks => [map()],
    scaling_recommendations => [map()]
}.

-type database_bottleneck() :: #{
    type := connection_pool | query_optimization | index | schema | memory | disk,
    severity := low | medium | high | critical,
    current_metric => float(),
    threshold => float(),
    impact => map(),
    query_examples => [binary()],
    optimization => map()
}.

%%====================================================================
## GEN_SERVER STATE
##====================================================================

-record(state, {
    scenarios :: [load_test_scenario()],
    current_scenario :: load_test_scenario() | undefined,
    metrics_history :: [database_metric()],
    start_time :: integer(),
    test_duration :: pos_integer(),
    current_load :: pos_integer(),
    max_concurrency :: pos_integer(),
    connection_pool :: pid() | undefined,
    query_generator :: pid(),
    monitoring_timer :: reference() | undefined,
    analysis_timer :: reference() | undefined,
    results :: map()
}).

%%====================================================================
## API FUNCTIONS
##====================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop, 5000).

-spec execute_load_tests() -> map().
execute_load_tests() ->
    gen_server:call(?MODULE, execute_load_tests, 30000).

-spec analyze_performance([database_metric()]) -> database_analysis().
analyze_performance(Metrics) ->
    gen_server:call(?MODULE, {analyze_performance, Metrics}, 10000).

-spec identify_bottlenecks(database_analysis()) -> [database_bottleneck()].
identify_bottlenecks(Analysis) ->
    gen_server:call(?MODULE, {identify_bottlenecks, Analysis}, 5000).

-spec validate_scaling(database_analysis()) -> map().
validate_scaling(Analysis) ->
    gen_server:call(?MODULE, {validate_scaling, Analysis}, 5000).

%%====================================================================
## GEN_SERVER CALLBACKS
##====================================================================

init([]) ->
    ?LOG_INFO("Starting database load test"),

    State = #state{
        scenarios = initialize_scenarios(),
        current_scenario = undefined,
        metrics_history = [],
        start_time = erlang:system_time(millisecond),
        test_duration = 3600000,  % 1 hour
        current_load = 0,
        max_concurrency = 1000,
        connection_pool = undefined,
        query_generator = undefined,
        monitoring_timer = undefined,
        analysis_timer = undefined,
        results = #{}
    },

    %% Initialize database connection pool
    ConnectionPool = initialize_database_pool(),

    %% Start query generator
    QueryGenerator = start_query_generator(),

    {ok, State#state{
        connection_pool = ConnectionPool,
        query_generator = QueryGenerator
    }}.

handle_call(execute_load_tests, _From, State) ->
    %% Execute all load test scenarios
    TestResults = execute_all_scenarios(State),

    {reply, TestResults, State};

handle_call({analyze_performance, Metrics}, _From, State) ->
    Analysis = analyze_database_performance(Metrics),
    {reply, Analysis, State};

handle_call({identify_bottlenecks, Analysis}, _From, State) ->
    Bottlenecks = identify_database_bottlenecks(Analysis),
    {reply, Bottlenecks, State};

handle_call({validate_scaling, Analysis}, _From, State) ->
    ScalingValidation = validate_database_scaling(Analysis),
    {reply, ScalingValidation, State};

handle_call(stop, _From, State) ->
    %% Stop all timers and clean up
    [erlang:cancel_timer(Timer) || Timer <-
        [State#state.monitoring_timer, State#state.analysis_timer]
    ],

    %% Stop query generator
    case State#state.query_generator of
        undefined -> ok;
        Pid -> Pid ! stop
    end,

    %% Shutdown connection pool
    case State#state.connection_pool of
        undefined -> ok;
        Pool -> erlmcp_pool:stop(Pool)
    end,

    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scenario_timeout, State) ->
    %% Move to next scenario
    NextState = execute_next_scenario(State),
    {noreply, NextState};

handle_info(collect_metrics, State) ->
    %% Collect database metrics
    Metrics = collect_database_metrics(State),

    %% Update metrics history
    NewMetricsHistory = State#state.metrics_history ++ Metrics,

    %% Schedule next metrics collection
    MonitoringTimer = erlang:send_after(1000, self(), collect_metrics),

    {noreply, State#state{
        metrics_history = NewMetricsHistory,
        monitoring_timer = MonitoringTimer
    }};

handle_info(analyze_metrics, State) ->
    %% Analyze collected metrics
    Analysis = analyze_database_performance(State#state.metrics_history),

    %% Identify bottlenecks
    Bottlenecks = identify_database_bottlenecks(Analysis),

    %% Update results
    UpdatedResults = State#state.results,
    case State#state.current_scenario of
        undefined -> ok;
        Scenario ->
            UpdatedResults = maps:put(Scenario#scenario.name, #{
                metrics => State#state.metrics_history,
                analysis => Analysis,
                bottlenecks => Bottlenecks
            }, UpdatedResults)
    end,

    %% Schedule next analysis
    AnalysisTimer = erlang:send_after(5000, self(), analyze_metrics),

    {noreply, State#state{
        results = UpdatedResults,
        analysis_timer = AnalysisTimer
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating database load test, executed scenarios: ~p",
             [maps:size(State#state.results)]),

    %% Clean up resources
    cleanup_resources(State),

    %% Generate final report
    generate_final_report(State),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## INTERNAL FUNCTIONS
##====================================================================

initialize_scenarios() ->
    %% Initialize database load test scenarios
    [
        #{
            name => "read_intensive",
            description => "Read-intensive workload pattern",
            query_patterns => generate_read_queries(),
            concurrency_level => 500,
            duration => 300000,  % 5 minutes
            ramp_up_duration => 60000,  % 1 minute
            think_time => 100,
            error_rate => 0.01,
            data_volume => 1000000
        },
        #{
            name => "write_intensive",
            description => "Write-intensive workload pattern",
            query_patterns => generate_write_queries(),
            concurrency_level => 200,
            duration => 300000,  % 5 minutes
            ramp_up_duration => 60000,  % 1 minute
            think_time => 200,
            error_rate => 0.02,
            data_volume => 500000
        },
        #{
            name => "mixed_workload",
            description => "Mixed read/write workload pattern",
            query_patterns => generate_mixed_queries(),
            concurrency_level => 300,
            duration => 600000,  % 10 minutes
            ramp_up_duration => 120000,  % 2 minutes
            think_time => 150,
            error_rate => 0.015,
            data_volume => 750000
        },
        #{
            name => "batch_operations",
            description => "Batch operation workload pattern",
            query_patterns => generate_batch_queries(),
            concurrency_level => 100,
            duration => 300000,  % 5 minutes
            ramp_up_duration => 60000,  % 1 minute
            think_time => 500,
            error_rate => 0.05,
            data_volume => 10000000
        },
        #{
            name => "stress_test",
            description => "High-stress workload pattern",
            query_patterns => generate_stress_queries(),
            concurrency_level => 800,
            duration => 180000,  % 3 minutes
            ramp_up_duration => 30000,  % 30 seconds
            think_time => 50,
            error_rate => 0.1,
            data_volume => 2000000
        }
    ].

generate_read_queries() ->
    %% Generate read query patterns
    [
        #{
            id => erlmcp_utils:uuid(),
            type => select,
            table => <<"users">>,
            query => <<"SELECT id, username, email FROM users WHERE id = $1">>,
            parameters => [1],
            expected_rows => 1,
            timeout => 1000
        },
        #{
            id => erlmcp_utils:uuid(),
            type => select,
            table => <<"users">>,
            query => <<"SELECT * FROM users WHERE created_at > $1 ORDER BY created_at LIMIT $2">>,
            parameters => [<<"2023-01-01">>, 100],
            expected_rows => 100,
            timeout => 2000
        },
        #{
            id => erlmcp_utils:uuid(),
            type => select,
            table => <<"sessions">>,
            query => <<"SELECT user_id, created_at FROM sessions WHERE last_active > $1">>,
            parameters = [<<"2023-01-01">>],
            expected_rows => 10,
            timeout => 1500
        }
    ].

generate_write_queries() ->
    %% Generate write query patterns
    [
        #{
            id => erlmcp_utils:uuid(),
            type => insert,
            table => <<"users">>,
            query = <<"INSERT INTO users (username, email, created_at) VALUES ($1, $2, $3)">>,
            parameters = [<<"test_user">>, <<"test@example.com">>, erlang:system_time(millisecond)],
            timeout = 2000
        },
        #{
            id => erlmcp_utils:uuid(),
            type => update,
            table = <<"sessions">>,
            query = <<"UPDATE sessions SET last_active = $1 WHERE user_id = $2">>,
            parameters = [erlang:system_time(millisecond), 1],
            timeout = 1000
        },
        #{
            id => erlmcp_utils:uuid(),
            type => delete,
            table = <<"temp_data">>,
            query = <<"DELETE FROM temp_data WHERE created_at < $1">>,
            parameters = [erlang:system_time(millisecond) - 86400000],  % 1 day ago
            timeout = 3000
        }
    ].

generate_mixed_queries() ->
    %% Generate mixed query patterns
    generate_read_queries() ++ generate_write_queries().

generate_batch_queries() ->
    %% Generate batch query patterns
    [
        #{
            id => erlmcp_utils:uuid(),
            type => batch,
            table = <<"bulk_insert">>,
            query = <<"INSERT INTO bulk_insert (data) VALUES ($1)">>,
            parameters = [erlmcp_utils:random_string(100)],
            timeout = 5000
        },
        #{
            id => erlmcp_utils:uuid(),
            type => batch,
            table = <<"bulk_update">>,
            query = <<"UPDATE bulk_update SET processed = true WHERE id = $1">>,
            parameters = [1],
            timeout = 3000
        }
    ].

generate_stress_queries() ->
    %% Generate stress query patterns
    [
        #{
            id => erlmcp_utils:uuid(),
            type => select,
            table = <<"large_table">>,
            query = <<"SELECT COUNT(*) FROM large_table">>,
            parameters = [],
            timeout = 5000
        },
        #{
            id => erlmcp_utils:uuid(),
            type => insert,
            table = <<"stress_table">>,
            query = <<"INSERT INTO stress_table (data) VALUES ($1)">>,
            parameters = [erlmcp_utils:random_string(1000)],
            timeout = 1000
        }
    ].

initialize_database_pool() ->
    %% Initialize database connection pool
    PoolConfig = #{
        name => database_pool,
        size => 50,
        max_overflow => 20,
        idle_timeout => 30000
    },

    case erlmcp_pool:start(PoolConfig) of
        {ok, PoolPid} ->
            PoolPid;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start database pool: ~p", [Reason]),
            undefined
    end.

start_query_generator() ->
    %% Start query generator process
    spawn_link(fun() -> query_generator_loop() end).

query_generator_loop() ->
    receive
        stop ->
            ok;
        {generate_query, From} ->
            %% Generate random query
            Query = generate_random_query(),
            From ! {query_generated, Query},
            query_generator_loop();
        {generate_queries, From, Count} ->
            %% Generate multiple queries
            Queries = [generate_random_query() || _ <- lists:seq(1, Count)],
            From ! {queries_generated, Queries},
            query_generator_loop()
    after 1000 ->
        query_generator_loop()
    end.

generate_random_query() ->
    %% Generate random database query
    QueryTypes = [select, insert, update, delete],
    Type = lists:nth(rand:uniform(length(QueryTypes)), QueryTypes),

    case Type of
        select ->
            #{
                id => erlmcp_utils:uuid(),
                type => select,
                table => <<"users">>,
                query => <<"SELECT * FROM users WHERE id = $1 LIMIT 10">>,
                parameters => [rand:uniform(1000)],
                timeout => 1000
            };
        insert ->
            #{
                id => erlmcp_utils:uuid(),
                type => insert,
                table => <<"logs">>,
                query = <<"INSERT INTO logs (message, created_at) VALUES ($1, $2)">>,
                parameters = [erlmcp_utils:random_string(100), erlang:system_time(millisecond)],
                timeout => 2000
            };
        update ->
            #{
                id => erlmcp_utils:uuid(),
                type => update,
                table = <<"stats">>,
                query = <<"UPDATE stats SET count = count + 1 WHERE name = $1">>,
                parameters = [<<"test_stat">>],
                timeout = 1000
            };
        delete ->
            #{
                id => erlmcp_utils:uuid(),
                type => delete,
                table = <<"temp_table">>,
                query = <<"DELETE FROM temp_table WHERE created_at < $1">>,
                parameters = [erlang:system_time(millisecond) - 3600000],
                timeout = 3000
            }
    end.

execute_all_scenarios(State) ->
    %% Execute all test scenarios
    lists:foldl(fun(Scenario, AccResults) ->
        ScenarioResults = execute_scenario(Scenario, State),
        AccResults#{Scenario#scenario.name => ScenarioResults}
    end, #{}, State#state.scenarios).

execute_scenario(Scenario, State) ->
    %% Execute specific test scenario
    ?LOG_INFO("Executing scenario: ~p", [Scenario#scenario.name]),

    %% Start scenario monitoring
    ScenarioStart = erlang:system_time(millisecond),

    %% Ramp up users
    ramp_up_users(Scenario, State),

    %% Execute scenario for specified duration
    execute_scenario_load(Scenario, State),

    %% Ramp down users
    ramp_down_users(Scenario, State),

    %% Collect results
    Results = collect_scenario_results(Scenario, State),

    %% Generate scenario report
    generate_scenario_report(Scenario, Results),

    Results.

ramp_up_users(Scenario, State) ->
    %% Ramp up users for scenario
    TotalUsers = Scenario#scenario.concurrency_level,
    RampTime = Scenario#scenario.ramp_up_duration,

    lists:foreach(fun(UserIndex) ->
        spawn_link(fun() -> database_user_worker(Scenario, UserIndex, State) end),
        timer:sleep(RampTime div TotalUsers)
    end, lists:seq(1, TotalUsers)).

execute_scenario_load(Scenario, State) ->
    %% Execute scenario load for specified duration
    EndTime = erlang:system_time(millisecond) + Scenario#scenario.duration,

    execute_load_loop(Scenario, State, EndTime).

execute_load_loop(Scenario, State, EndTime) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            %% Execute load
            execute_database_load(Scenario, State),
            timer:sleep(Scenario#scenario.think_time),
            execute_load_loop(Scenario, State, EndTime);
        false ->
            ok
    end.

ramp_down_users(Scenario, State) ->
    %% Ramp down users
    TotalUsers = Scenario#scenario.concurrency_level,

    lists:foreach(fun(UserIndex) ->
        case whereis(list_to_atom("db_user_" ++ integer_to_list(UserIndex))) of
            Pid when is_pid(Pid) ->
                Pid ! stop;
            undefined ->
                ok
        end
    end, lists:seq(1, TotalUsers)).

database_user_worker(Scenario, UserIndex, State) ->
    %% Database user worker process
    register(list_to_atom("db_user_" ++ integer_to_list(UserIndex)), self()),

    database_user_loop(Scenario, UserIndex, State).

database_user_loop(Scenario, UserIndex, State) ->
    receive
        stop ->
            ok
    after 0 ->
        %% Execute database queries
        case execute_user_queries(Scenario, State) of
            {ok, _} ->
                database_user_loop(Scenario, UserIndex, State);
            {error, Error} ->
                ?LOG_ERROR("Database query error: ~p", [Error]),
                database_user_loop(Scenario, UserIndex, State)
        end
    end.

execute_user_queries(Scenario, State) ->
    %% Execute user queries
    QueryPattern = select_random_query(Scenario#scenario.query_patterns),

    case execute_database_query(QueryPattern, State) of
        {ok, Result} ->
            RecordMetric(QueryPattern, Result, true),
            {ok, Result};
        {error, Error} ->
            RecordMetric(QueryPattern, #{error => Error}, false),
            {error, Error}
    end.

select_random_query(QueryPatterns) ->
    %% Select random query pattern
    lists:nth(rand:uniform(length(QueryPatterns)), QueryPatterns).

execute_database_query(Query, State) ->
    %% Execute database query
    case State#state.connection_pool of
        undefined ->
            {error, connection_pool_not_initialized};
        PoolPid ->
            %% Get connection from pool
            case erlmcp_pool:get_connection(PoolPid) of
                {ok, Connection} ->
                    try
                        %% Execute query
                        Result = execute_query(Connection, Query),
                        erlmcp_pool:return_connection(PoolPid, Connection),
                        {ok, Result}
                    catch
                        Error:Reason ->
                            erlmcp_pool:return_connection(PoolPid, Connection),
                            {error, {Error, Reason}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

execute_query(Connection, Query) ->
    %% Execute individual database query
    StartTime = erlang:system_time(millisecond),

    try
        %% Simulate database query execution
        case Query#database_query.type of
            select ->
                %% Simulate SELECT query
                timer:sleep(rand:uniform(100)),
                #{rows => generate_result_rows(Query)};
            insert ->
                %% Simulate INSERT query
                timer:sleep(rand:uniform(200)),
                #{affected_rows => 1};
            update ->
                %% Simulate UPDATE query
                timer:sleep(rand:uniform(150)),
                #{affected_rows => 1};
            delete ->
                %% Simulate DELETE query
                timer:sleep(rand:uniform(250)),
                #{affected_rows => 1};
            batch ->
                %% Simulate batch query
                timer:sleep(rand:uniform(1000)),
                #{affected_rows => 100}
        end
    catch
        Error:Reason ->
            throw({query_error, {Error, Reason}})
    end.

generate_result_rows(Query) ->
    %% Generate result rows for SELECT query
    case Query#database_query.expected_rows of
        undefined ->
            10;
        Expected ->
            Expected
    end.

record_metric(Query, Result, Success) ->
    %% Record database metric
    Metric = #{
        timestamp => erlang:system_time(millisecond),
        query_type => atom_to_binary(Query#database_query.type, utf8),
        latency => erlang:system_time(millisecond) - get_query_start_time(),
        rows_affected => maps:get(affected_rows, Result, 0),
        success => Success,
        error => case Success of true -> undefined; false -> maps:get(error, Result, unknown) end,
        query_size => byte_size(Query#database_query.query),
        result_size => calculate_result_size(Result),
        connection_id => erlmcp_utils:uuid(),
        pool_name => <<"database_pool">>
    },

    %% Store metric (in real implementation, would be stored in ETS or database)
    ok.

get_query_start_time() ->
    %% Get query start time (stored in process dictionary)
    case get(query_start_time) of
        undefined -> erlang:system_time(millisecond);
        Time -> Time
    end.

calculate_result_size(Result) ->
    %% Calculate result size in bytes
    case is_map(Result) andalso maps:is_key(rows, Result) of
        true ->
            length(Result#rows) * 100;  % Estimate
        false ->
            0
    end.

collect_database_metrics(State) ->
    %% Collect database metrics from workers
    Metrics = [],

    %% In real implementation, would collect from ETS or database
    Metrics.

collect_scenario_results(Scenario, State) ->
    %% Collect scenario results
    ScenarioMetrics = [M || M <- State#state.metrics_history,
                          M#database_metric.timestamp >= Scenario#scenario.ramp_up_duration],

    #{
        scenario => Scenario#scenario.name,
        metrics => ScenarioMetrics,
        duration => erlang:system_time(millisecond) - Scenario#scenario.ramp_up_duration,
        throughput => calculate_scenario_throughput(ScenarioMetrics),
        average_latency => calculate_average_latency(ScenarioMetrics),
        success_rate => calculate_success_rate(ScenarioMetrics)
    }.

calculate_scenario_throughput(Metrics) ->
    %% Calculate scenario throughput
    case Metrics of
        [] -> 0.0;
        _ ->
            TotalDuration = lists:last(Metrics)#database_metric.timestamp -
                           lists:nth(1, Metrics)#database_metric.timestamp,
            TotalQueries = length([M || M <- Metrics, M#database_metric.success]),
            if TotalDuration > 0 -> TotalQueries / (TotalDuration / 1000.0); true -> 0.0 end
    end.

calculate_average_latency(Metrics) ->
    ##% Calculate average latency
    case [M#database_metric.latency || M <- Metrics, M#database_metric.success] of
        [] -> 0.0;
        Latencies ->
            lists:sum(Latencies) / length(Latencies)
    end.

calculate_success_rate(Metrics) ->
    %% Calculate success rate
    case Metrics of
        [] -> 0.0;
        _ ->
            SuccessCount = length([M || M <- Metrics, M#database_metric.success]),
            SuccessCount / length(Metrics)
    end.

analyze_database_performance(Metrics) ->
    %% Analyze database performance
    case Metrics of
        [] -> #{};
        _ ->
            Analysis = #{
                throughput => calculate_throughput(Metrics),
                average_latency => calculate_average_latency(Metrics),
                p95_latency => calculate_p95_latency(Metrics),
                p99_latency => calculate_p99_latency(Metrics),
                success_rate => calculate_success_rate(Metrics),
                error_rate => 1.0 - calculate_success_rate(Metrics),
                query_distribution => analyze_query_distribution(Metrics),
                connection_pool_efficiency => calculate_pool_efficiency(Metrics),
                cache_hit_rate => calculate_cache_hit_rate(Metrics),
                index_efficiency => analyze_index_efficiency(Metrics),
                bottlenecks => [],
                scaling_recommendations => []
            },

            Analysis
    end.

calculate_throughput(Metrics) ->
    %% Calculate query throughput
    case Metrics of
        [] -> 0.0;
        _ ->
            Duration = lists:last(Metrics)#database_metric.timestamp -
                       lists:nth(1, Metrics)#database_metric.timestamp,
            if Duration > 0 -> length(Metrics) / (Duration / 1000.0); true -> 0.0 end
    end.

calculate_p95_latency(Metrics) ->
    ##% Calculate 95th percentile latency
    Latencies = [M#database_metric.latency || M <- Metrics, M#database_metric.success],
    case Latencies of
        [] -> 0;
        _ -> lists:nth(floor(0.95 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_p99_latency(Metrics) ->
    %% Calculate 99th percentile latency
    Latencies = [M#database_metric.latency || M <- Metrics, M#database_metric.success],
    case Latencies of
        [] -> 0;
        _ -> lists:nth(floor(0.99 * length(Latencies)), lists:sort(Latencies))
    end.

analyze_query_distribution(Metrics) ->
    %% Analyze query type distribution
    Distribution = lists:foldl(fun(Metric, Acc) ->
        QueryType = Metric#database_metric.query_type,
        maps:update(QueryType, maps:get(QueryType, Acc, 0) + 1, Acc)
    end, #{}, Metrics),

    lists:fold(fun({Type, Count}, Acc) ->
        [#{type => Type, count => Count} | Acc]
    end, [], maps:to_list(Distribution)).

calculate_pool_efficiency(Metrics) ->
    %% Calculate connection pool efficiency
    case Metrics of
        [] -> 0.0;
        _ ->
            ConnectionErrors = length([M || M <- Metrics,
                                         M#database_metric.success =:= false,
                                         M#database_metric.error =/= undefined]),
            if ConnectionErrors > 0 -> 1.0 - (ConnectionErrors / length(Metrics)); true -> 1.0 end
    end.

calculate_cache_hit_rate(Metrics) ->
    %% Calculate cache hit rate (simulated)
    0.85.

analyze_index_efficiency(Metrics) ->
    %% Analyze index efficiency (simulated)
    #{
        select_index_efficiency => 0.95,
        insert_index_efficiency => 0.90,
        update_index_efficiency => 0.92,
        delete_index_efficiency => 0.88
    }.

identify_database_bottlenecks(Analysis) ->
    %% Identify database bottlenecks
    Bottlenecks = [],

    %% Check for connection pool bottlenecks
    case Analysis#database_analysis.connection_pool_efficiency < 0.9 of
        true ->
            Bottleneck = #{
                type => connection_pool,
                severity => high,
                current_metric => Analysis#database_analysis.connection_pool_efficiency,
                threshold => 0.9,
                impact => #{performance_impact => (1.0 - Analysis#database_analysis.connection_pool_efficiency) * 100},
                query_examples => ["SELECT * FROM users WHERE username = 'test'"],
                optimization => #{action => increase_pool_size, recommendation => "Increase connection pool size"}
            },
            [Bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    %% Check for query optimization bottlenecks
    case Analysis#database_analysis.p95_latency > 1000 of
        true ->
            Bottleneck = #{
                type => query_optimization,
                severity => medium,
                current_metric => Analysis#database_analysis.p95_latency,
                threshold => 1000,
                impact => #{performance_impact => (Analysis#database_analysis.p95_latency - 1000) / 10},
                query_examples => ["SELECT * FROM large_table WHERE condition"],
                optimization => #{action => add_indexes, recommendation => "Add appropriate indexes"}
            },
            [Bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    Bottlenecks.

validate_database_scaling(Analysis) ->
    ##% Validate database scaling capabilities
    Scaling = #{
        current_capacity => Analysis#database_analysis.throughput,
        scaling_factor => determine_scaling_factor(Analysis),
        recommended_capacity => Analysis#database_analysis.throughput * determine_scaling_factor(Analysis),
        bottlenecks => Analysis#database_analysis.bottlenecks,
        scaling_recommendations => generate_scaling_recommendations(Analysis)
    },

    Scaling.

determine_scaling_factor(Analysis) ->
    %% Determine database scaling factor based on analysis
    case Analysis#database_analysis.p95_latency of
        Latency when Latency > 2000 -> 0.5;  % Limited scaling
        Latency when Latency > 1000 -> 1.0;  % No scaling needed
        _ -> 2.0  % Can scale well
    end.

generate_scaling_recommendations(Analysis) ->
    %% Generate database scaling recommendations
    Recommendations = [],

    %% Based on throughput
    case Analysis#database_analysis.throughput < 1000 of
        true ->
            [#{type => read_replicas, action => "Add read replicas", impact => high} | Recommendations];
        false -> Recommendations
    end,

    %% Based on connection efficiency
    case Analysis#database_analysis.connection_pool_efficiency < 0.95 of
        true ->
            [#{type => connection_pool, action => "Optimize connection pool", impact => medium} | Recommendations];
        false -> Recommendations
    end,

    Recommendations.

generate_scenario_report(Scenario, Results) ->
    %% Generate scenario-specific report
    Report = #{
        scenario => Scenario#scenario.name,
        timestamp => erlang:system_time(millisecond),
        results => Results,
        metrics => collect_database_metrics(#state{})
    },

    %% Save report
    ReportFile = "/Users/sac/erlmcp/load_test_reports/database_" ++
                 binary_to_list(Scenario#scenario.name) ++ "_report.json",
    ok = file:write_file(ReportFile, jsx:encode(Report)).

generate_final_report(State) ->
    %% Generate final comprehensive report
    FinalReport = #{
        test_summary => #{
            total_scenarios => length(State#state.scenarios),
            test_duration => erlang:system_time(millisecond) - State#state.start_time,
            overall_throughput => calculate_overall_throughput(State#state.results)
        },
        scenario_results => State#state.results,
        bottlenecks => identify_aggregate_bottlenecks(State#state.results),
        recommendations => generate_aggregate_recommendations(State#state.results),
        timestamp => erlang:system_time(millisecond)
    },

    %% Save final report
    ReportFile = "/Users/sac/erlmcp/load_test_reports/database_test_final_report.json",
    ok = file:write_file(ReportFile, jsx:encode(FinalReport)).

calculate_overall_throughput(Results) ->
    %% Calculate overall throughput across all scenarios
    Throughputs = maps:fold(fun(_Name, ScenarioResult, Acc) ->
        maps:get(throughput, ScenarioResult#scenario_results) + Acc
    end, 0.0, Results),

    case maps:size(Results) of
        0 -> 0.0;
        _ -> Throughputs / maps:size(Results)
    end.

identify_aggregate_bottlenecks(Results) ->
    %% Identify aggregate bottlenecks across all scenarios
    AllBottlenecks = maps:fold(fun(_Name, ScenarioResult, Acc) ->
        Bottlenecks = maps:get(bottlenecks, ScenarioResult#scenario_results),
        Acc ++ Bottlenecks
    end, [], Results),

    %% Aggregate by type
    AggregateBottlenecks = lists:foldl(fun(Bottleneck, Acc) ->
        Type = maps:get(type, Bottleneck),
        maps:update(Type, [Bottleneck | maps:get(Type, Acc, [])], Acc)
    end, #{}, AllBottlenecks),

    AggregateBottlenecks.

generate_aggregate_recommendations(Results) ->
    ##% Generate aggregate recommendations
    Recommendations = [],

    %% Analyze overall performance
    OverallAnalysis = analyze_overall_performance(Results),

    %% Generate recommendations based on analysis
    case OverallAnalysis#overall_analysis.overall_score < 80 of
        true ->
            [#{type => system_upgrade, priority => high,
               recommendation => "Consider system upgrade for better performance"} | Recommendations];
        false -> Recommendations
    end,

    Recommendations.

analyze_overall_performance(Results) ->
    %% Analyze overall database performance
    #{
        overall_score => calculate_overall_score(Results),
        throughput_score => calculate_throughput_score(Results),
        latency_score => calculate_latency_score(Results),
        reliability_score => calculate_reliability_score(Results)
    }.

calculate_overall_score(Results) ->
    %% Calculate overall performance score
    ThroughputScore = calculate_throughput_score(Results),
    LatencyScore = calculate_latency_score(Results),
    ReliabilityScore = calculate_reliability_score(Results),

    (ThroughputScore + LatencyScore + ReliabilityScore) / 3.0.

calculate_throughput_score(Results) ->
    %% Calculate throughput score (0-100)
    Throughputs = [maps:get(throughput, Scenario#scenario_results) ||
                  Scenario <- maps:values(Results)],
    case Throughputs of
        [] -> 0.0;
        _ ->
            MaxThroughput = lists:max(Throughputs),
            if MaxThroughput > 0 -> min(100.0, MaxThroughput / 10.0); true -> 0.0 end
    end.

calculate_latency_score(Results) ->
    ##% Calculate latency score (0-100, lower latency = higher score)
    Latencies = [maps:get(average_latency, Scenario#scenario_results) ||
                Scenario <- maps:values(Results)],
    case Latencies of
        [] -> 100.0;
        _ ->
            AverageLatency = lists:sum(Latencies) / length(Latencies),
            max(0.0, 100.0 - (AverageLatency / 10.0))  % Each 10ms reduces score by 1 point
    end.

calculate_reliability_score(Results) ->
    %% Calculate reliability score (0-100)
    SuccessRates = [maps:get(success_rate, Scenario#scenario_results) ||
                  Scenario <- maps:values(Results)],
    case SuccessRates of
        [] -> 0.0;
        _ -> lists:sum(SuccessRates) * 100.0 / length(SuccessRates)
    end.

cleanup_resources(State) ->
    %% Clean up test resources
    %% Stop all worker processes
    unregister_all_workers(),

    %% Close database connections
    case State#state.connection_pool of
        undefined -> ok;
        Pool -> erlmcp_pool:stop(Pool)
    end,

    ok.

unregister_all_workers() ->
    %% Unregister all worker processes
    Workers = erlang:registered(),
    lists:foreach(fun(Name) ->
        case atom_to_list(Name) of
            "db_user_" ++ _ ->
                case whereis(Name) of
                    Pid when is_pid(Pid) ->
                        Pid ! stop;
                    undefined ->
                        ok
                end;
            _ -> ok
        end
    end, Workers).