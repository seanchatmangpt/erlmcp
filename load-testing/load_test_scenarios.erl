%% @doc Comprehensive Load Testing Scenarios for erlmcp v3 Fortune 500 Deployment
%% Designed for peak traffic patterns, stress testing, and performance validation
%%
%% Generated: 2026-02-02
%% Purpose: Enterprise-grade load testing with realistic Fortune 500 workloads

-module(erlmcp_load_test_scenarios).
-author("erlmcp-load-test-team").
-behaviour(application).

%% API exports
-export([start/0, start_link/0, run_scenarios/1, stop/0]).
-export([get_test_scenarios/0, get_workload_patterns/0, get_performance_thresholds/0]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record(scenario_config, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    duration :: integer(), % milliseconds
    max_connections :: integer(),
    request_rate :: integer(), % requests per second
    pattern :: constant | ramp_up | spike | sustained | burst,
    timeout :: integer(), % milliseconds
    retry_policy :: {integer(), integer()}, % max_retries, backoff_ms
    expected_results :: map()
}).

-record.performance_threshold, {
    min_throughput :: integer(), % requests/sec
    max_latency :: float(), % milliseconds
    p99_latency :: float(), % milliseconds
    p95_latency :: float(), % milliseconds
    error_rate :: float(), % percentage
    cpu_usage :: float(), % percentage
    memory_usage :: float(), % percentage
    connection_count :: integer()
}).

-record.workload_pattern, {
    name :: binary(),
    scale_factor :: float(),
    arrival_rate :: float(),
    think_time :: float(), % milliseconds
    mix :: map(), % tool_type -> percentage
    duration :: integer()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start() ->
    application:ensure_all_started(?MODULE),
    ok.

start_link() ->
    application:ensure_all_started(?MODULE),
    ok.

stop() ->
    application:stop(?MODULE),
    ok.

run_scenarios(Config) ->
    Scenarios = get_test_scenarios(),
    Results = lists:map(fun(Scenario) ->
        run_scenario(Scenario, Config)
    end, Scenarios),
    generate_test_report(Results).

%% ============================================================================
%% SCENARIO DEFINITIONS
%% ============================================================================

get_test_scenarios() ->
    [
        %% Peak Traffic Scenarios
        #scenario_config{
            id = <<"peak_daily_trading">>,
            name = "Peak Daily Trading",
            description = "Simulate end-of-day financial trading surge (Fortune 500)",
            duration = 300000, % 5 minutes
            max_connections = 25000,
            request_rate = 50000, % 50K requests/sec
            pattern = spike,
            timeout = 5000,
            retry_policy = {3, 1000},
            expected_results = #{
                throughput => 45000,
                max_latency => 200,
                p99_latency => 150,
                error_rate => 0.01,
                cpu_usage => 75,
                memory_usage => 65
            }
        },

        #scenario_config{
            id = <<"monthly_reporting_surge">>,
            name = "Monthly Reporting Surge",
            description = "End-of-month reporting consolidation (ERP systems)",
            duration = 900000, % 15 minutes
            max_connections = 50000,
            request_rate = 100000, % 100K requests/sec
            pattern = sustained,
            timeout = 10000,
            retry_policy = {5, 2000},
            expected_results = #{
                throughput => 95000,
                max_latency => 500,
                p99_latency => 300,
                error_rate => 0.02,
                cpu_usage => 85,
                memory_usage => 80
            }
        },

        %% Stress Testing Scenarios
        #scenario_config{
            id = <<"connection_stress_test">>,
            name = "Connection Stress Test",
            description = "Maximum connection capacity (Horizontal scaling validation)",
            duration = 600000, % 10 minutes
            max_connections = 100000,
            request_rate = 200000, % 200K requests/sec
            pattern = constant,
            timeout = 3000,
            retry_policy = {2, 500},
            expected_results = #{
                throughput => 180000,
                max_latency => 1000,
                p99_latency => 800,
                error_rate => 0.05,
                cpu_usage => 95,
                memory_usage => 90
            }
        },

        #scenario_config{
            id = <<"burst_traffic_pattern">>,
            name = "Burst Traffic Pattern",
            description = "Sudden traffic spikes (e.g., breaking news, flash sales)",
            duration = 180000, % 3 minutes
            max_connections = 75000,
            request_rate = 150000, % 150K requests/sec
            pattern = spike,
            timeout = 15000,
            retry_policy = {3, 1500},
            expected_results = #{
                throughput => 135000,
                max_latency => 750,
                p99_latency => 600,
                error_rate => 0.03,
                cpu_usage => 90,
                memory_usage => 75
            }
        },

        %% Failover Testing
        #scenario_config{
            id = <<"_node_failure_scenario">>,
            name = "Node Failure Scenario",
            description = "Test failover during node failures (Chaos engineering)",
            duration = 600000, % 10 minutes
            max_connections = 30000,
            request_rate = 60000, % 60K requests/sec
            pattern = sustained,
            timeout = 8000,
            retry_policy = {5, 3000},
            expected_results = #{
                throughput => 55000,
                max_latency => 1200,
                p99_latency => 1000,
                error_rate => 0.1,
                cpu_usage => 88,
                memory_usage => 82
            }
        },

        %% Horizontal Scaling
        #scenario_config{
            id = <<"horizontal_scaling_test">>,
            name = "Horizontal Scaling Test",
            description = "Validate scaling performance across cluster nodes",
            duration = 1200000, % 20 minutes
            max_connections = 150000,
            request_rate = 300000, % 300K requests/sec
            pattern = ramp_up,
            timeout = 6000,
            retry_policy = {3, 1000},
            expected_results = #{
                throughput => 280000,
                max_latency => 400,
                p99_latency => 350,
                error_rate => 0.02,
                cpu_usage => 78,
                memory_usage => 70
            }
        },

        %% Memory Leak Detection
        #scenario_config{
            id = <<"memory_longevity_test">>,
            name = "Memory Longevity Test",
            description = "24-hour continuous operation with memory leak detection",
            duration = 86400000, % 24 hours
            max_connections = 10000,
            request_rate = 20000, % 20K requests/sec
            pattern = sustained,
            timeout = 5000,
            retry_policy = {2, 1000},
            expected_results = #{
                throughput => 19500,
                max_latency => 300,
                p99_latency => 250,
                error_rate => 0.005,
                cpu_usage => 45,
                memory_growth => 0.0 % 0% growth over 24 hours
            }
        },

        %% Network Partition Tolerance
        #scenario_config{
            id = <<"network_partition_test">>,
            name = "Network Partition Test",
            description = "Test system behavior during network partitions",
            duration = 1800000, % 30 minutes
            max_connections = 20000,
            request_rate = 40000, % 40K requests/sec
            pattern = sustained,
            timeout = 12000,
            retry_policy = {4, 2000},
            expected_results = #{
                throughput => 38000,
                max_latency => 2000,
                p99_latency => 1800,
                error_rate => 0.15,
                cpu_usage => 82,
                memory_usage => 78
            }
        },

        %% Database Performance
        #scenario_config{
            id = <<"database_performance_test">>,
            name = "Database Performance Test",
            description = "Erlang/OTP database performance with concurrent operations",
            duration = 900000, % 15 minutes
            max_connections = 25000,
            request_rate = 50000, % 50K requests/sec
            pattern = constant,
            timeout = 7000,
            retry_policy = {3, 1500},
            expected_results = #{
                throughput => 47500,
                max_latency => 600,
                p99_latency => 450,
                error_rate => 0.01,
                cpu_usage => 70,
                memory_usage => 68
            }
        },

        %% Regression Testing
        #scenario_config{
            id = <<"regression_suite">>,
            name = "Regression Testing Suite",
            description = "Comprehensive regression testing with all supported tools",
            duration = 3600000, % 60 minutes
            max_connections = 30000,
            request_rate = 60000, % 60K requests/sec
            pattern = ramp_up,
            timeout = 8000,
            retry_policy = {4, 2000},
            expected_results = #{
                throughput => 57000,
                max_latency => 550,
                p99_latency => 450,
                error_rate => 0.008,
                cpu_usage => 75,
                memory_usage => 72
            }
        }
    ].

get_workload_patterns() ->
    [
        #workload_pattern{
            name = "Financial Trading",
            scale_factor = 1.0,
            arrival_rate = 0.1, % Poisson process
            think_time = 50,
            mix = #{
                file_system => 15,
                file_search => 25,
                shell_command => 30,
                git => 20,
                mermaid => 10
            },
            duration = 300000
        },

        #workload_pattern{
            name = "Enterprise Reporting",
            scale_factor = 2.0,
            arrival_rate = 0.05,
            think_time = 200,
            mix = #{
                file_system => 35,
                shell_command => 25,
                git => 15,
                file_search => 15,
                mermaid => 10
            },
            duration = 900000
        },

        #workload_pattern{
            name = "Content Management",
            scale_factor = 1.5,
            arrival_rate = 0.08,
            think_time = 100,
            mix = #{
                file_system => 40,
                file_search => 20,
                shell_command => 20,
                git => 15,
                mermaid => 5
            },
            duration = 600000
        },

        #workload_pattern{
            name = "Development Pipeline",
            scale_factor = 1.2,
            arrival_rate = 0.12,
            think_time = 80,
            mix = #{
                git => 35,
                shell_command => 30,
                file_system => 20,
                file_search => 10,
                mermaid => 5
            },
            duration = 450000
        }
    ].

get_performance_thresholds() ->
    [
        #performance_threshold{
            min_throughput = 10000,
            max_latency = 1000,
            p99_latency = 800,
            p95_latency = 500,
            error_rate = 0.05,
            cpu_usage = 90,
            memory_usage = 85,
            connection_count = 50000
        },

        #performance_threshold{
            min_throughput = 50000,
            max_latency = 500,
            p99_latency = 400,
            p95_latency = 300,
            error_rate = 0.02,
            cpu_usage = 80,
            memory_usage = 75,
            connection_count = 100000
        },

        #performance_threshold{
            min_throughput = 100000,
            max_latency = 1000,
            p99_latency = 900,
            p95_latency = 700,
            error_rate = 0.05,
            cpu_usage = 95,
            memory_usage = 90,
            connection_count = 150000
        }
    ].

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

run_scenario(ScenarioConfig, TestConfig) ->
    ?LOG_INFO("Starting load test scenario: ~p", [ScenarioConfig#scenario_config.name]),

    try
        %% Initialize test environment
        TestEnv = setup_test_environment(ScenarioConfig, TestConfig),

        %% Spawn test clients
        TestClients = spawn_test_clients(ScenarioConfig, TestEnv),

        %% Run test duration
        test_duration(ScenarioConfig#scenario_config.duration, TestClients, TestEnv),

        %% Collect metrics
        Metrics = collect_test_metrics(TestClients, TestEnv),

        %% Validate results
        Validation = validate_results(Metrics, ScenarioConfig#scenario_config.expected_results),

        %% Cleanup
        cleanup_test_environment(TestEnv),

        #{
            scenario => ScenarioConfig#scenario_config.name,
            metrics => Metrics,
            validation => Validation,
            status => case Validation#validation.passed of
                true -> passed;
                false -> failed
            end
        }

    catch
        Error:Reason ->
            ?LOG_ERROR("Scenario ~p failed: ~p:~p", [
                ScenarioConfig#scenario_config.name, Error, Reason
            ]),
            #{
                scenario => ScenarioConfig#scenario_config.name,
                error => {Error, Reason},
                status => error
            }
    end.

setup_test_environment(ScenarioConfig, TestConfig) ->
    #{
        id => generate_test_id(),
        start_time => erlang:system_time(millisecond),
        config => ScenarioConfig,
        test_config => TestConfig,
        metrics => erlmcp_metrics:new(),
        connections => #{},
        errors => [],
        supervisors => []
    }.

spawn_test_clients(ScenarioConfig, TestEnv) ->
    MaxConnections = ScenarioConfig#scenario_config.max_connections,
    ClientConfig = #{
        rate => ScenarioConfig#scenario_config.request_rate,
        pattern => ScenarioConfig#scenario_config.pattern,
        timeout => ScenarioConfig#scenario_config.timeout,
        retry_policy => ScenarioConfig#scenario_config.retry_policy,
        workload => get_workload_patterns()
    },

    spawn_clients(MaxConnections, ClientConfig, TestEnv, []).

spawn_clients(0, _ClientConfig, TestEnv, Clients) ->
    Clients;

spawn_clients(N, ClientConfig, TestEnv, Clients) ->
    ClientId = generate_client_id(),

    %% Create client process
    Client = spawn_link(fun() ->
        load_test_client(ClientId, ClientConfig, TestEnv)
    end),

    %% Track connection
    ConnectionRef = make_ref(),
    Connections = TestEnv#connections ++ [{ClientId, Client, ConnectionRef}],

    spawn_clients(N-1, ClientConfig, TestEnv#connections{Connections}, [Client | Clients]).

load_test_client(ClientId, ClientConfig, TestEnv) ->
    ?LOG_DEBUG("Load test client ~p starting", [ClientId]),

    try
        load_test_client_loop(ClientId, ClientConfig, TestEnv, 0, [])
    catch
        Error:Reason ->
            ?LOG_ERROR("Client ~p crashed: ~p:~p", [ClientId, Error, Reason]),
            erlang:error(Error, Reason)
    end.

load_test_client_loop(ClientId, ClientConfig, TestEnv, RequestCount, Latencies) ->
    case should_continue_client(ClientId, TestEnv) of
        true ->
            %% Generate request based on pattern
            Request = generate_test_request(ClientConfig, TestEnv),

            %% Send request and measure latency
            {Latency, Result} = send_test_request(Request, ClientConfig#timeout, TestEnv),

            %% Update metrics
            UpdatedLatencies = [Latency | Latencies],
            UpdatedEnv = update_client_metrics(ClientId, TestEnv, Latency, Result),

            %% Calculate next request time based on pattern
            NextRequestTime = calculate_next_request_time(ClientConfig#pattern, UpdatedLatencies),

            %% Wait before next request
            timer:sleep(NextRequestTime),

            load_test_client_loop(ClientId, ClientConfig, UpdatedEnv, RequestCount + 1, UpdatedLatencies);

        false ->
            %% Send final metrics
            send_client_metrics(ClientId, TestEnv, RequestCount, Latencies),
            ok
    end.

generate_test_request(ClientConfig, TestEnv) ->
    Workload = ClientConfig#workload,
    Pattern = Workload#workload_pattern.mix,

    %% Select tool type based on workload mix
    ToolType = select_tool_type(Pattern),

    %% Generate realistic test data
    TestRequest = #{
        id => generate_request_id(),
        tool_type => ToolType,
        parameters => generate_tool_parameters(ToolType),
        timestamp => erlang:system_time(millisecond),
        client_id => generate_client_id()
    },

    TestRequest.

send_test_request(Request, Timeout, TestEnv) ->
    StartTime = erlang:system_time(millisecond),

    try
        %% Send to erlmcp server
        Result = erlmcp_client:call_tool(Request#tool_type, Request#parameters),

        Latency = erlang:system_time(millisecond) - StartTime,

        {Latency, Result}

    catch
        Error:Reason ->
            Latency = erlang:system_time(millisecond) - StartTime,
            ?LOG_WARNING("Request failed: ~p:~p", [Error, Reason]),

            %% Record error
            ErrorRecord = #{
                timestamp => erlang:system_time(millisecond),
                request_id => Request#id,
                error => {Error, Reason},
                latency => Latency
            },

            UpdatedEnv = TestEnv#errors ++ [ErrorRecord],

            {Latency, {error, {Error, Reason}}}
    end.

calculate_next_request_time(pattern, Latencies) ->
    %% Implement different request timing patterns
    case pattern of
        constant -> 1; % No delay between requests
        ramp_up -> max(1, 100 - length(Latencies) div 10); % Gradual increase
        spike -> case length(Latencies) rem 10 of
                    0 -> 50; % Spike every 10 requests
                    _ -> 1
                end;
        sustained -> 10; % Constant moderate delay
        burst -> case length(Latencies) rem 5 of
                   0 -> 100; % Burst pattern
                   _ -> 1
               end
    end.

collect_test_metrics(TestClients, TestEnv) ->
    Metrics = erlmcp_metrics:new(),

    %% Collect metrics from all clients
    ClientMetrics = lists:map(fun(Client) ->
        erlmcp_metrics:collect(Client)
    end, TestClients),

    %% Aggregate metrics
    AggregatedMetrics = aggregate_metrics(ClientMetrics),

    %% Add system metrics
    SystemMetrics = collect_system_metrics(),

    #{
        throughput => AggregatedMetrics#throughput,
        latency => AggregatedMetrics#latency,
        error_rate => calculate_error_rate(TestEnv#errors),
        cpu_usage => SystemMetrics#cpu_usage,
        memory_usage => SystemMetrics#memory_usage,
        connection_count => length(TestClients),
        duration => erlang:system_time(millisecond) - TestEnv#start_time
    }.

aggregate_metrics(ClientMetrics) ->
    ThroughputList = [M#throughput || M <- ClientMetrics],
    LatencyList = [M#latency || M <- ClientMetrics],

    #{
        throughput => lists:sum(ThroughputList) / length(ThroughputList),
        latency => #{
            average => lists:sum(LatencyList) / length(LatencyList),
            min => lists:min(LatencyList),
            max => lists:max(LatencyList),
            p95 => calculate_percentile(LatencyList, 95),
            p99 => calculate_percentile(LatencyList, 99)
        }
    }.

validate_results(Metrics, ExpectedResults) ->
    Thresholds = get_performance_thresholds(),

    Throughput = Metrics#throughput,
    Latency = Metrics#latency,
    ErrorRate = Metrics#error_rate,
    CpuUsage = Metrics#cpu_usage,
    MemoryUsage = Metrics#memory_usage,

    ThroughputOK = Throughput >= ExpectedResults#min_throughput,
    LatencyOK = Latency#max <= ExpectedResults#max_latency,
    P99OK = Latency#p99 <= ExpectedResults#p99_latency,
    ErrorRateOK = ErrorRate <= ExpectedResults#error_rate,
    CpuOK = CpuUsage <= ExpectedResults#cpu_usage,
    MemoryOK = MemoryUsage <= ExpectedResults#memory_usage,

    #{
        passed => ThroughputOK and LatencyOK and P99OK and
                ErrorRateOK and CpuOK and MemoryOK,
        throughput => #{
            actual => Throughput,
            expected => ExpectedResults#min_throughput,
            passed => ThroughputOK
        },
        latency => #{
            actual_max => Latency#max,
            expected_max => ExpectedResults#max_latency,
            actual_p99 => Latency#p99,
            expected_p99 => ExpectedResults#p99_latency,
            passed => LatencyOK and P99OK
        },
        error_rate => #{
            actual => ErrorRate,
            expected => ExpectedResults#error_rate,
            passed => ErrorRateOK
        },
        cpu_usage => #{
            actual => CpuUsage,
            expected => ExpectedResults#cpu_usage,
            passed => CpuOK
        },
        memory_usage => #{
            actual => MemoryUsage,
            expected => ExpectedResults#memory_usage,
            passed => MemoryOK
        }
    }.

generate_test_report(Results) ->
    Report = #{
        timestamp => erlang:system_time(millisecond),
        scenarios => Results,
        summary => generate_summary(Results),
        recommendations => generate_recommendations(Results)
    },

    %% Save report
    ReportFile = "/Users/sac/erlmcp/load-testing/test_report_" ++
                 integer_to_list(erlang:system_time(millisecond)) ++ ".json",
    file:write_file(ReportFile, jsx:encode(Report)),

    ?LOG_INFO("Test report saved to: ~p", [ReportFile]),
    Report.

generate_summary(Results) ->
    Passed = length([R || R <- Results, R#status =:= passed]),
    Failed = length([R || R <- Results, R#status =:= failed]),
    Total = length(Results),

    #{
        total_scenarios => Total,
        passed => Passed,
        failed => Failed,
        pass_rate => Passed / Total,
        average_throughput => calculate_average_throughput(Results),
        average_latency => calculate_average_latency(Results),
        average_error_rate => calculate_average_error_rate(Results)
    }.

generate_recommendations(Results) ->
    %% Analyze results and generate recommendations
    Recommendations = lists:map(fun(Result) ->
        case Result#status of
            passed ->
                #{
                    scenario => Result#scenario,
                    recommendation => "No changes needed",
                    priority => low
                };
            failed ->
                #{
                    scenario => Result#scenario,
                    recommendation => generate_failure_recommendation(Result),
                    priority => high
                }
        end
    end, Results),

    Recommendations.

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

generate_test_id() ->
    <<ID:128>> = crypto:strong_rand_bytes(16),
    ID.

generate_client_id() ->
    integer_to_list(erlang:system_time(millisecond)) ++ "_" ++
    integer_to_list(crypto:rand_uniform(1, 1000000)).

generate_request_id() ->
    <<ID:64>> = crypto:strong_rand_bytes(8),
    ID.

select_tool_type(ToolMix) ->
    Types = maps:keys(ToolMix),
    Weights = maps:values(ToolMix),

    %% Weighted random selection
    TotalWeight = lists:sum(Weights),
    Random = crypto:rand_uniform(1, TotalWeight + 1),

    select_tool_type_helper(Types, Weights, Random, 1).

select_tool_type_helper([Type], _, _, _) ->
    Type;
select_tool_type_helper([Type | Rest], [Weight | Weights], Random, Accum) ->
    if
        Random =< Accum + Weight ->
            Type;
        true ->
            select_tool_type_helper(Rest, Weights, Random, Accum + Weight)
    end.

generate_tool_parameters(file_system) ->
    #{
        operation => list,
        path => generate_random_path(),
        recursive => boolean
    };

generate_tool_parameters(file_search) ->
    #{
        pattern => generate_search_pattern(),
        directory => generate_random_path(),
        case_sensitive => boolean
    };

generate_tool_parameters(shell_command) ->
    #{
        command => generate_shell_command(),
        timeout => 30000
    };

generate_tool_parameters(git) ->
    #{
        operation => generate_git_operation(),
        repository => generate_git_repo(),
        branch => generate_branch_name()
    };

generate_tool_parameters(mermaid) ->
    #{
        diagram_type => generate_diagram_type(),
        content => generate_diagram_content(),
        format => png
    }.

generate_random_path() ->
    "/test/" ++ integer_to_list(crypto:rand_uniform(1, 1000000)) ++
    "/file_" ++ integer_to_list(crypto:rand_uniform(1, 1000)) ++ ".txt".

generate_search_pattern() ->
    "test_" ++ integer_to_list(crypto:rand_uniform(1, 1000)) ++ "_*.txt".

generate_shell_command() ->
    "echo " ++ integer_to_list(crypto:rand_uniform(1, 100000)).

generate_git_operation() ->
    lists:nth(crypto:rand_uniform(1, 4), [log, status, diff, branch]).

generate_git_repo() ->
    "https://github.com/example/repo_" ++ integer_to_list(crypto:rand_uniform(1, 1000)).

generate_branch_name() ->
    "feature/" ++ integer_to_list(crypto:rand_uniform(1, 1000)).

generate_diagram_type() ->
    lists:nth(crypto:rand_uniform(1, 4), [graph, sequence, flowchart, pie]).

generate_diagram_content() ->
    "graph TD
    A[Start] --> B{Decision}
    B -->|Yes| C[Action 1]
    B -->|No| D[Action 2]
    C --> E[End]
    D --> E".

calculate_percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Index = trunc((Percentile / 100) * length(Sorted)),
    lists:nth(Index, Sorted).

calculate_error_rate(Errors) ->
    case length(Errors) of
        0 -> 0.0;
        _ -> length(Errors) / 1000 * 100 % Example calculation
    end.

calculate_average_throughput(Results) ->
    Throughputs = [R#metrics#throughput || R <- Results, R#status =:= passed],
    case Throughputs of
        [] -> 0;
        _ -> lists:sum(Throughputs) / length(Throughputs)
    end.

calculate_average_latency(Results) ->
    Latencies = [R#metrics#latency#average || R <- Results, R#status =:= passed],
    case Latencies of
        [] -> 0;
        _ -> lists:sum(Latencies) / length(Latencies)
    end.

calculate_average_error_rate(Results) ->
    ErrorRates = [R#metrics#error_rate || R <- Results, R#status = := passed],
    case ErrorRates of
        [] -> 0;
        _ -> lists:sum(ErrorRates) / length(ErrorRates)
    end.

generate_failure_recommendation(Result) ->
    %% Analyze failure and generate specific recommendation
    Validation = Result#validation,

    case Validation#throughput#passed of
        false ->
            "Increase server resources or optimize request processing";
        true ->
            case Validation#latency#passed of
                false ->
                    "Optimize network latency or increase connection pool size";
                true ->
                    case Validation#error_rate#passed of
                        false ->
                            "Implement better error handling and retry mechanisms";
                        true ->
                            "Check memory leaks and optimize garbage collection"
                    end
            end
    end.

should_continue_client(_ClientId, _TestEnv) ->
    %% Implement continuation logic based on test duration
    true.

send_client_metrics(_ClientId, _TestEnv, _RequestCount, _Latencies) ->
    %% Send client-specific metrics to monitoring system
    ok.

update_client_metrics(_ClientId, TestEnv, Latency, Result) ->
    %% Update test environment with client metrics
    TestEnv.

cleanup_test_environment(TestEnv) ->
    %% Cleanup test resources
    ok.

collect_system_metrics() ->
    %% Collect system metrics (CPU, memory, etc.)
    #{
        cpu_usage => 0.0,
        memory_usage => 0.0
    }.

-spec test_duration(integer(), list(), map()) -> ok.
test_duration(Duration, TestClients, TestEnv) ->
    EndTime = erlang:system_time(millisecond) + Duration,

    test_duration_loop(EndTime, TestClients, TestEnv).

test_duration_loop(EndTime, TestClients, TestEnv) ->
    CurrentTime = erlang:system_time(millisecond),

    if
        CurrentTime < EndTime ->
            %% Periodic metrics collection
            case CurrentTime rem 10000 of
                0 ->
                    %% Collect metrics every 10 seconds
                    Metrics = collect_test_metrics(TestClients, TestEnv),
                    ?LOG_INFO("Current metrics: ~p", [Metrics]),
                    ok;
                _ ->
                    ok
            end,

            test_duration_loop(EndTime, TestClients, TestEnv);

        true ->
            %% Test completed
            ?LOG_INFO("Test duration completed", []),
            ok
    end.