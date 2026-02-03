%% @doc Load Generator for erlmcp v3
%% Comprehensive load generation for testing
%%
%% Features:
%% - Pattern-based load generation
%% - Multi-tool support
%% - Rate limiting and pacing
%%- Think time simulation
%%- Concurrent client management
%%- Metrics collection
%%- Real-time monitoring
%%- Adaptive load profiles

-module(erlmcp_load_generator).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_load_test/1, stop_load_test/1, get_load_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.load_profile, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    pattern :: constant | ramp_up | spike | sustained | burst | random,
    target_rate :: integer(), % requests per second
    duration :: integer(), % milliseconds
    concurrency :: integer(), % concurrent clients
    think_time :: integer(), % milliseconds
    tools :: list(), % list of tool configurations
    think_time_distribution :: uniform | exponential | normal,
    rate_adjustment :: none | adaptive | reactive
}.

record.load_test_config, {
    test_id :: binary(),
    profile :: #load_profile{},
    start_time :: integer(),
    end_time :: integer(),
    status :: stopped | running | completed | failed,
    clients :: list(), % list of client pids
    metrics :: map(),
    results :: map()
}.

record.client_state, {
    id :: binary(),
    pid :: pid(),
    tool_type :: binary(),
    request_count :: integer(),
    error_count :: integer(),
    avg_latency :: float(),
    p95_latency :: float(),
    p99_latency :: float(),
    throughput :: float(),
    last_request_time :: integer(),
    stats :: map()
}.

record.load_metrics, {
    timestamp :: integer(),
    active_clients :: integer(),
    total_requests :: integer(),
    successful_requests :: integer(),
    failed_requests :: integer(),
    throughput :: float(), % requests per second
    avg_latency :: float(), % milliseconds
    p95_latency :: float(), % milliseconds
    p99_latency :: float(), % milliseconds
    error_rate :: float(), % percentage
    cpu_usage :: float(), % percentage
    memory_usage :: float(), % percentage
    network_io :: float(), % bytes per second
    tool_distribution :: map() % tool -> percentage
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_load_test(Config) ->
    gen_server:call(?MODULE, {start_load_test, Config}, infinity).

stop_load_test(TestId) ->
    gen_server:call(?MODULE, {stop_load_test, TestId}).

get_load_metrics() ->
    gen_server:call(?MODULE, get_load_metrics).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Load generator initialized"),

    %% Initialize state
    State = #{
        active_tests => #{},
        load_metrics => initialize_load_metrics(),
        client_pools => initialize_client_pools(),
        rate_limiter => init_rate_limiter(),
        monitors => initialize_monitors()
    },

    %% Start monitoring
    erlang:send_after(1000, self(), collect_metrics),

    {ok, State}.

handle_call({start_load_test, Config}, _From, State) ->
    %% Validate load test configuration
    case validate_load_config(Config) of
        {ok, ValidConfig} ->
            %% Create and start load test
            TestId = ValidConfig#test_id;
            Test = create_load_test(TestId, ValidConfig),
            NewState = start_load_test_execution(Test, State),
            {reply, {ok, test_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop_load_test, TestId}, _From, State) ->
    %% Stop load test
    NewState = stop_load_test_execution(TestId, State),
    {reply, {ok, test_stopped}, NewState};

handle_call(get_load_metrics, _From, State) ->
    %% Get current load metrics
    Metrics = get_current_metrics(State),
    {reply, {ok, Metrics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect and aggregate metrics
    Metrics = collect_and_aggregate_metrics(State);
    UpdatedState = State#{
        load_metrics => Metrics
    },

    %% Schedule next metrics collection
    erlang:send_after(1000, self(), collect_metrics),

    {noreply, UpdatedState};

handle_info({client_metrics, TestId, ClientId, Metrics}, State) ->
    %% Handle client metrics
    UpdatedState = update_client_metrics(TestId, ClientId, Metrics, State),

    %% Check if test should continue
    case should_continue_test(TestId, State) of
        true ->
            {noreply, UpdatedState};
        false ->
            CompleteState = complete_load_test(TestId, UpdatedState),
            {noreply, CompleteState}
    end;

handle_info({client_completed, TestId, ClientId, Results}, State) ->
    %% Handle client completion
    ?LOG_DEBUG("Client ~p completed for test ~p", [ClientId, TestId]),

    UpdatedState = update_client_completion(TestId, ClientId, Results, State),

    {noreply, UpdatedState};

handle_info({adjust_rate, TestId, NewRate}, State) ->
    %% Handle rate adjustment
    ?LOG_INFO("Adjusting rate for test ~p to ~p", [TestId, NewRate]),

    UpdatedState = adjust_load_rate(TestId, NewRate, State),

    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup load generator
    lists:foreach(fun({TestId, _}) ->
        stop_load_test_execution(TestId, State)
    end, maps:keys(maps:get(active_tests, State))),

    ?LOG_INFO("Load generator terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_load_config(Config) ->
    %% Validate load test configuration
    case maps:is_key(test_id, Config) andalso maps:is_key(profile, Config) of
        true ->
            case Config#profile.target_rate > 0 andalso Config#profile.duration > 0 of
                true ->
                    {ok, Config};
                false ->
                    {error, invalid_rate_or_duration}
            end;
        false ->
            {error, missing_required_fields}
    end.

create_load_test(TestId, Config) ->
    %% Create load test
    #load_test_config{
        test_id = TestId,
        profile = Config#profile,
        start_time = erlang:system_time(millisecond),
        status = running,
        clients = [],
        metrics => #{},
        results => #{}
    }.

start_load_test_execution(Test, State) ->
    %% Start load test execution
    ?LOG_INFO("Starting load test: ~p", [Test#test_id]),

    %% Create client pool
    ClientPool = create_client_pool(Test);

    %% Start load generation
    start_load_generation(Test, ClientPool);

    %% Add to active tests
    NewState = State#{
        active_tests => maps:put(Test#test_id, Test, maps:get(active_tests, State)),
        client_pools => maps:put(Test#test_id, ClientPool, maps:get(client_pools, State))
    },

    %% Send initial metrics
    erlang:send_after(0, self(), {client_metrics, Test#test_id, init, get_initial_metrics()}),

    {reply, {ok, test_started}, NewState}.

create_client_pool(Test) ->
    %% Create client pool for load test
    Profile = Test#profile;
    Concurrency = Profile#concurrency;
    Clients = lists:map(fun(I) ->
        ClientId = generate_client_id(Test#test_id, I);
        ToolType = select_tool_type(Profile#tools);

        Client = spawn_link(fun() ->
            load_client_loop(Test#test_id, ClientId, ToolType, Profile)
        end),

        #client_state{
            id = ClientId,
            pid = Client,
            tool_type = ToolType,
            request_count = 0,
            error_count = 0,
            avg_latency = 0.0,
            p95_latency = 0.0,
            p99_latency = 0.0,
            throughput = 0.0,
            last_request_time = 0,
            stats => #{}
        }
    end, lists:seq(1, Concurrency)),

    Clients.

start_load_generation(Test, ClientPool) ->
    %% Start load generation from client pool
    lists:foreach(fun(Client) ->
        Client#pid ! start_generation
    end, ClientPool).

load_client_loop(TestId, ClientId, ToolType, Profile) ->
    %% Main client loop for load generation
    process_flag(trap_exit, true),

    load_client_loop(TestId, ClientId, ToolType, Profile, 0, 0, []).

load_client_loop(TestId, ClientId, ToolType, Profile, RequestCount, ErrorCount, Latencies) ->
    receive
        start_generation ->
            %% Start generating load
            generate_load(TestId, ClientId, ToolType, Profile, RequestCount, ErrorCount, Latencies);
        stop ->
            %% Stop generating load
            send_completion(TestId, ClientId, RequestCount, ErrorCount, Latencies);
        _ ->
            load_client_loop(TestId, ClientId, ToolType, Profile, RequestCount, ErrorCount, Latencies)
    end.

generate_load(TestId, ClientId, ToolType, Profile, RequestCount, ErrorCount, Latencies) ->
    %% Generate load according to profile
    StartTime = erlang:system_time(millisecond);

    %% Generate request based on profile
    Request = generate_request(ToolType, Profile);

    %% Send request and measure latency
    {Latency, Result} = send_request(Request, Profile#think_time);

    %% Update statistics
    NewRequestCount = RequestCount + 1;
    NewErrorCount = case Result of
        {ok, _} -> ErrorCount;
        {error, _} -> ErrorCount + 1
    end;

    UpdatedLatencies = [Latency | Latencies];

    Calculate next request time based on pattern
    NextRequestTime = calculate_next_request_time(Profile, Latencies);

    %% Wait before next request
    timer:sleep(NextRequestTime);

    %% Send metrics
    Metrics = calculate_client_metrics(NewRequestCount, NewErrorCount, UpdatedLatencies);
    erlang:send_after(0, self(), {client_metrics, TestId, ClientId, Metrics});

    Continue generation
    generate_load(TestId, ClientId, ToolType, Profile, NewRequestCount, NewErrorCount, UpdatedLatencies).

generate_request(ToolType, Profile) ->
    %% Generate test request based on tool type
    case ToolType of
        file_system ->
            #{
                operation => list,
                path => generate_path(),
                recursive => boolean()
            };
        file_search ->
            #{
                pattern => generate_pattern(),
                directory => generate_directory(),
                case_sensitive => boolean()
            };
        shell_command ->
            #{
                command => generate_command(),
                timeout => Profile#think_time * 2
            };
        git ->
            #{
                operation => generate_git_operation(),
                repository => generate_repository(),
                branch => generate_branch()
            };
        mermaid ->
            #{
                diagram_type => generate_diagram_type(),
                content => generate_diagram_content(),
                format => png
            };
        _ ->
            #{
                tool => ToolType,
                data => generate_random_data(1024)
            }
    end.

send_request(Request, ThinkTime) ->
    %% Send request and measure latency
    StartTime = erlang:system_time(millisecond);

    %% Apply think time if specified
    case ThinkTime > 0 of
        true ->
            timer:sleep(ThinkTime);
        false ->
            ok
    end,

    %% Send to erlmcp server
    try
        Result = erlmcp_client:call_tool(Request#tool, Request#parameters),
        Latency = erlang:system_time(millisecond) - StartTime,
        {Latency, {ok, Result}}
    catch
        Error:Reason ->
            Latency = erlang:system_time(millisecond) - StartTime,
            {Latency, {error, {Error, Reason}}}
    end.

calculate_next_request_time(Profile, Latencies) ->
    %% Calculate next request time based on load pattern
    case Profile#pattern of
        constant ->
            max(1, 1000 div Profile#target_rate); % No delay between requests
        ramp_up ->
            max(1, 100 - length(Latencies) div 10); % Gradual increase
        spike ->
            case length(Latencies) rem 10 of
                0 -> 50; % Spike every 10 requests
                _ -> 1
            end;
        sustained ->
            max(1, 1000 div Profile#target_rate * 2); % Moderate delay
        burst ->
            case length(Latencies) rem 5 of
                0 -> 100; % Burst pattern
                _ -> 1
            end;
        random ->
            % Random distribution
            crypto:rand_uniform(1, 1000 div Profile#target_rate + 10)
    end.

calculate_client_metrics(RequestCount, ErrorCount, Latencies) ->
    %% Calculate client metrics
    case Latencies of
        [] ->
            #{
                request_count => RequestCount,
                error_count => ErrorCount,
                avg_latency => 0.0,
                p95_latency => 0.0,
                p99_latency => 0.0,
                throughput => 0.0
            };
        _ ->
            SortedLatencies = lists:sort(Latencies),
            AvgLatency = lists:sum(Latencies) / length(Latencies);
            P95Latency = calculate_percentile(SortedLatencies, 95);
            P99Latency = calculate_percentile(SortedLatencies, 99);
            Throughput = (RequestCount * 1000) / (lists:last(Latencies) + 1); % Requests per second

            #{
                request_count => RequestCount,
                error_count => ErrorCount,
                avg_latency => AvgLatency,
                p95_latency => P95Latency,
                p99_latency => P99Latency,
                throughput => Throughput
            }
    end.

calculate_percentile(List, Percentile) ->
    %% Calculate percentile value
    Index = trunc((Percentile / 100) * length(List)),
    lists:nth(Index, List).

select_tool_type(Tools) ->
    %% Select tool type based on distribution
    case Tools of
        [] ->
            file_system; % Default
        _ ->
            % Weighted random selection
            TotalWeight = lists:sum([W || #{weight := W} <- Tools]),
            Random = crypto:rand_uniform(1, TotalWeight + 1);

            select_tool_type_helper(Tools, Random, 0)
    end.

select_tool_type_helper([#{tool := Tool, weight := Weight} | Rest], Random, Accum) ->
    if
        Random =< Accum + Weight ->
            Tool;
        true ->
            select_tool_type_helper(Rest, Random, Accum + Weight)
    end.

generate_path() ->
    %% Generate random file path
    "/test/" ++ integer_to_list(crypto:rand_uniform(1, 1000000)) ++
    "/file_" ++ integer_to_list(crypto:rand_uniform(1, 1000)) ++ ".txt".

generate_pattern() ->
    %% Generate search pattern
    "test_" ++ integer_to_list(crypto:rand_uniform(1, 1000)) ++ "_*.txt".

generate_directory() ->
    %% Generate random directory
    "/test/" ++ integer_to_list(crypto:rand_uniform(1, 1000)).

generate_command() ->
    %% Generate shell command
    "echo " ++ integer_to_list(crypto:rand_uniform(1, 100000)).

generate_git_operation() ->
    %% Generate Git operation
    lists:nth(crypto:rand_uniform(1, 5), [log, status, diff, branch, commit]).

generate_repository() ->
    %% Generate repository URL
    "https://github.com/example/repo_" ++ integer_to_list(crypto:rand_uniform(1, 1000)).

generate_branch() ->
    %% Generate branch name
    "feature/" ++ integer_to_list(crypto:rand_uniform(1, 1000)).

generate_diagram_type() ->
    %% Generate diagram type
    lists:nth(crypto:rand_uniform(1, 5), [graph, sequence, flowchart, pie, class]).

generate_diagram_content() ->
    %% Generate diagram content
    "graph TD
    A[Start] --> B{Decision}
    B -->|Yes| C[Action 1]
    B -->|No| D[Action 2]
    C --> E[End]
    D --> E".

generate_random_data(Size) ->
    %% Generate random test data
    crypto:strong_rand_bytes(Size).

generate_client_id(TestId, Index) ->
    %% Generate unique client ID
    TestId ++ <<"_client_", (integer_to_binary(Index))/binary>>.

send_completion(TestId, ClientId, RequestCount, ErrorCount, Latencies) ->
    %% Send client completion
    Metrics = calculate_client_metrics(RequestCount, ErrorCount, Latencies);

    erlang:send_after(0, self(), {client_completed, TestId, ClientId, Metrics}).

stop_load_test_execution(TestId, State) ->
    %% Stop load test execution
    case maps:find(TestId, maps:get(active_tests, State)) of
        {ok, Test} ->
            %% Stop all clients
            ClientPool = maps:get(TestId, maps:get(client_pools, State));
            lists:foreach(fun(Client) ->
                Client#pid ! stop
            end, ClientPool);

            %% Update test status
            UpdatedTest = Test#load_test_config{
                end_time => erlang:system_time(millisecond),
                status => completed
            };

            %% Remove from active tests
            NewActiveTests = maps:remove(TestId, maps:get(active_tests, State));
            NewClientPools = maps:remove(TestId, maps:get(client_pools, State));

            State#{
                active_tests => NewActiveTests,
                client_pools => NewClientPools
            };
        error ->
            State
    end.

collect_and_aggregate_metrics(State) ->
    %% Collect and aggregate metrics from all tests
    ActiveTests = maps:get(active_tests, State);
    AggregateMetrics = #{
        timestamp => erlang:system_time(millisecond),
        active_tests => maps:size(ActiveTests),
        total_clients => calculate_total_clients(ActiveTests),
        total_requests => calculate_total_requests(ActiveTests),
        throughput => calculate_total_throughput(ActiveTests),
        avg_latency => calculate_avg_latency(ActiveTests),
        error_rate => calculate_total_error_rate(ActiveTests),
        tool_distribution => calculate_tool_distribution(ActiveTests),
        system_metrics => get_system_metrics()
    };

    AggregateMetrics.

calculate_total_clients(ActiveTests) ->
    %% Calculate total number of active clients
    lists:foldl(fun(_TestId, Test) ->
        length(Test#clients)
    end, 0, maps:values(ActiveTests)).

calculate_total_requests(ActiveTests) ->
    %% Calculate total requests across all clients
    lists:foldl(fun(_TestId, Test) ->
        lists:foldl(fun(Client, Acc) ->
            Client#request_count + Acc
        end, 0, Test#clients)
    end, 0, maps:values(ActiveTests)).

calculate_total_throughput(ActiveTests) ->
    %% Calculate total throughput
    lists:foldl(fun(_TestId, Test) ->
        lists:foldl(fun(Client, Acc) ->
            Client#throughput + Acc
        end, 0.0, Test#clients)
    end, 0.0, maps:values(ActiveTests)).

calculate_avg_latency(ActiveTests) ->
    %% Calculate average latency
    AllLatencies = lists:foldl(fun(_TestId, Test) ->
        lists:foldl(fun(Client, Acc) ->
            [Client#avg_latency | Acc]
        end, [], Test#clients)
    end, [], maps:values(ActiveTests));

    case AllLatencies of
        [] ->
            0.0;
        _ ->
            lists:sum(AllLatencies) / length(AllLatencies)
    end.

calculate_total_error_rate(ActiveTests) ->
    %% Calculate total error rate
    TotalRequests = calculate_total_requests(ActiveTests);
    TotalErrors = lists:foldl(fun(_TestId, Test) ->
        lists:foldl(fun(Client, Acc) ->
            Client#error_count + Acc
        end, 0, Test#clients)
    end, 0, maps:values(ActiveTests));

    case TotalRequests of
        0 ->
            0.0;
        _ ->
            (TotalErrors / TotalRequests) * 100
    end.

calculate_tool_distribution(ActiveTests) ->
    %% Calculate tool distribution
    ToolCounts = lists:foldl(fun(_TestId, Test) ->
        lists:foldl(fun(Client, Acc) ->
            Tool = Client#tool_type;
            Current = maps:get(Tool, Acc, 0);
            maps:put(Tool, Current + 1, Acc)
        end, #{}, Test#clients)
    end, #{}, maps:values(ActiveTests));

    Total = maps:values(ToolCounts), lists:sum(Total);

    maps:map(fun(Tool, Count) ->
        (Count / Total) * 100
    end, ToolCounts).

get_system_metrics() ->
    %% Get system metrics
    #{
        cpu_usage => get_cpu_usage(),
        memory_usage => get_memory_usage(),
        network_io => get_network_io(),
        disk_io => get_disk_io()
    }.

get_cpu_usage() ->
    %% Get CPU usage percentage
    case os:type() of
        {unix, linux} ->
            get_linux_cpu_usage();
        {unix, darwin} ->
            get_darwin_cpu_usage();
        _ ->
            0.0
    end.

get_memory_usage() ->
    %% Get memory usage percentage
    case os:type() of
        {unix, linux} ->
            get_linux_memory_usage();
        {unix, darwin} ->
            get_darwin_memory_usage();
        _ ->
            0.0
    end.

get_network_io() ->
    %% Get network I/O rate
    case os:type() of
        {unix, linux} ->
            get_linux_network_io();
        _ ->
            0.0
    end.

get_disk_io() ->
    %% Get disk I/O rate
    case os:type() of
        {unix, linux} ->
            get_linux_disk_io();
        _ ->
            0.0
    end.

get_linux_cpu_usage() ->
    %% Get Linux CPU usage
    case os:cmd("top -l 1 -n 0 | grep 'CPU usage'") of
        "CPU usage: " ++ Rest ->
            case string:split(Rest, "%", all) of
                [LoadStr | _] ->
                    case float(string:trim(LoadStr)) of
                        Load when Load > 0 ->
                            Load;
                        _ ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

get_darwin_cpu_usage() ->
    %% Get macOS CPU usage
    case os:cmd("top -l 1 -n 0 | grep 'CPU usage'") of
        "CPU usage: " ++ Rest ->
            case string:split(Rest, "%", all) of
                [LoadStr | _] ->
                    case float(string:trim(LoadStr)) of
                        Load when Load > 0 ->
                            Load;
                        _ ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

get_linux_memory_usage() ->
    %% Get Linux memory usage
    case file:read_file("/proc/meminfo") of
        {ok, Content} ->
            Lines = string:split(Content, "\n", all);
            Total = extract_mem_value(Lines, "MemTotal:");
            Available = extract_mem_value(Lines, "MemAvailable:");

            case Total of
                0 ->
                    0.0;
                _ ->
                    (Total - Available) / Total * 100
            end;
        _ ->
            0.0
    end.

get_darwin_memory_usage() ->
    %% Get macOS memory usage
    case os:cmd("vm_stat | grep 'Pages free'") of
        "Pages free: " ++ FreeStr ->
            Free = list_to_integer(string:trim(FreeStr));
            case os:cmd("vm_stat | grep 'Pages active'") of
                "Pages active: " ++ ActiveStr ->
                    Active = list_to_integer(string:trim(ActiveStr));
                    Total = Free + Active + 1024; % Approximate
                    if
                        Total > 0 ->
                            Active / Total * 100;
                        true ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

get_linux_network_io() ->
    %% Get Linux network I/O
    case os:cmd("cat /proc/net/dev | grep -E '(eth0|ens|enp)' | head -1") of
        Line when is_list(Line) ->
            case string:split(Line, " ", all) of
                [_, _, _, _, _, _, _, _, _, _, _, BytesIn | _] ->
                    case list_to_integer(string:trim(BytesIn)) of
                        Val when Val > 0 ->
                            Val / 1024 / 1024; % Convert to MB/s
                        _ ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

get_linux_disk_io() ->
    %% Get Linux disk I/O
    case os:cmd("iostat -d -x 1 2 | grep sda") of
        [Line | _] when is_list(Line) ->
            case string:split(Line, " ", all) of
                [_, _, _, _, _, _, _, _, Util | _] ->
                    case float(string:trim(Util)) of
                        Val when Val > 0 ->
                            Val;
                        _ ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

extract_mem_value(Lines, Key) ->
    %% Extract memory value from /proc/meminfo
    lists:foldl(fun(Line, Acc) ->
        case string:find(Line, Key) of
            nomatch ->
                Acc;
            _ ->
                case string:split(Line, " ", all) of
                    [_, ValueStr | _] ->
                        list_to_integer(ValueStr) * 1024; % Convert to KB
                    _ ->
                        Acc
                end
        end
    end, 0, Lines).

get_initial_metrics() ->
    %% Get initial metrics
    #{
        timestamp => erlang:system_time(millisecond),
        request_count => 0,
        error_count => 0,
        avg_latency => 0.0,
        throughput => 0.0
    }.

get_current_metrics(State) ->
    %% Get current load metrics
    maps:get(load_metrics, State).

update_client_metrics(TestId, ClientId, Metrics, State) ->
    %% Update client metrics
    case maps:find(TestId, maps:get(active_tests, State)) of
        {ok, Test} ->
            UpdatedTest = update_client_metrics_in_test(Test, ClientId, Metrics);
            State#{
                active_tests => maps:put(TestId, UpdatedTest, maps:get(active_tests, State))
            };
        error ->
            State
    end.

update_client_metrics_in_test(Test, ClientId, Metrics) ->
    %% Update client metrics in specific test
    UpdatedClients = lists:map(fun(Client) ->
        case Client#id =:= ClientId of
            true ->
                Client#client_state{
                    request_count => Metrics#request_count,
                    error_count => Metrics#error_count,
                    avg_latency => Metrics#avg_latency,
                    p95_latency => Metrics#p95_latency,
                    p99_latency => Metrics#p99_latency,
                    throughput => Metrics#throughput,
                    last_request_time => erlang:system_time(millisecond),
                    stats => Metrics
                };
            false ->
                Client
        end
    end, Test#clients),

    Test#load_test_config{
        clients => UpdatedClients,
        metrics => merge_metrics(Test#metrics, Metrics)
    }.

update_client_completion(TestId, ClientId, Results, State) ->
    %% Update client completion
    case maps:find(TestId, maps:get(active_tests, State)) of
        {ok, Test} ->
            %% Remove completed client
            RemainingClients = lists:filter(fun(Client) ->
                Client#id =/= ClientId
            end, Test#clients);

            %% Check if all clients completed
            case RemainingClients of
                [] ->
                    %% All clients completed, finalize test
                    FinalTest = Test#load_test_config{
                        end_time => erlang:system_time(millisecond),
                        status => completed,
                        results => Results
                    };

                    NewState = State#{
                        active_tests => maps:remove(TestId, maps:get(active_tests, State))
                    };

                    %% Send completion notification
                    erlang:send_after(0, self(), {test_completed, TestId, FinalTest}),
                    NewState;
                _ ->
                    %% Update test with remaining clients
                    UpdatedTest = Test#load_test_config{
                        clients => RemainingClients
                    };

                    State#{
                        active_tests => maps:put(TestId, UpdatedTest, maps:get(active_tests, State))
                    }
            end;
        error ->
            State
    end.

merge_metrics(Existing, New) ->
    %% Merge metrics maps
    maps:merge(Existing, New).

should_continue_test(TestId, State) ->
    %% Check if test should continue
    case maps:find(TestId, maps:get(active_tests, State)) of
        {ok, Test} ->
            Profile = Test#profile;
            ElapsedTime = erlang:system_time(millisecond) - Test#start_time;
            ElapsedTime < Profile#duration;
        error ->
            false
    end.

complete_load_test(TestId, State) ->
    %% Complete load test
    case maps:find(TestId, maps:get(active_tests, State)) of
        {ok, Test} ->
            FinalTest = Test#load_test_config{
                end_time => erlang:system_time(millisecond),
                status => completed
            };

            State#{
                active_tests => maps:remove(TestId, maps:get(active_tests, State))
            };
        error ->
            State
    end.

adjust_load_rate(TestId, NewRate, State) ->
    %% Adjust load rate
    case maps:find(TestId, maps:get(active_tests, State)) of
        {ok, Test} ->
            UpdatedTest = Test#load_test_config{
                profile => Test#profile#target_rate = NewRate
            };

            State#{
                active_tests => maps:put(TestId, UpdatedTest, maps:get(active_tests, State))
            };
        error ->
            State
    end.

initialize_load_metrics() ->
    %% Initialize load metrics
    #{
        timestamp => erlang:system_time(millisecond),
        active_tests => 0,
        total_clients => 0,
        total_requests => 0,
        throughput => 0.0,
        avg_latency => 0.0,
        error_rate => 0.0,
        tool_distribution => #{},
        system_metrics => #{}
    }.

initialize_client_pools() ->
    %% Initialize client pools
    #{}.

init_rate_limiter() ->
    %% Initialize rate limiter
    #{
        current_rate => 0,
        max_rate => 10000,
        tokens => 1000,
        last_refill => erlang:system_time(millisecond)
    }.

initialize_monitors() ->
    %% Initialize monitors
    #{}.

init_rate_limiter() ->
    %% Initialize rate limiter state
    #{
        current_rate => 0,
        max_rate => 10000,
        tokens => 1000,
        last_refill => erlang:system_time(millisecond)
    }.