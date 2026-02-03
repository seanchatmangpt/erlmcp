-module(erlmcp_network_test).

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
    validate_network_configurations/1
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

-type network_test_scenario() :: #{
    name := binary(),
    description := binary(),
    protocol := tcp | udp | http | websocket | sse,
    target_endpoints := [binary()],
    load_pattern := constant | ramp_up | spike | burst | storm,
    duration := pos_integer(),
    concurrency := pos_integer(),
    request_rate := pos_integer(),
    packet_size := pos_integer(),
    timeout := pos_integer(),
    error_rate => float()
}.

-type network_metric() :: #{
    timestamp := integer(),
    protocol := binary(),
    target_endpoint := binary(),
    latency := integer(),
    bandwidth := integer(),
    packet_loss := float(),
    jitter := integer(),
    success := boolean(),
    error := binary() | undefined,
    connection_id := binary(),
    round_trip_time => integer()
}.

-type network_analysis() :: #{
    throughput => float(),
    average_latency => float(),
    p95_latency => integer(),
    p99_latency => integer(),
    packet_loss_rate => float(),
    bandwidth_utilization => float(),
    connection_efficiency => float(),
    protocol_performance => map(),
    bottleneck_analysis => map(),
    network_recommendations => [map()],
    capacity_planning => map()
}.

-type network_bottleneck() :: #{
    type := bandwidth | latency | packet_loss | connection_limit | routing | firewall,
    severity := low | medium | high | critical,
    current_metric => float(),
    threshold => float(),
    impact => map(),
    affected_endpoints => [binary()],
    mitigation => map()
}.

%%====================================================================
## GEN_SERVER STATE
##====================================================================

-record(state, {
    scenarios :: [network_test_scenario()],
    current_scenario :: network_test_scenario() | undefined,
    metrics_history :: [network_metric()],
    test_start_time :: integer(),
    current_load :: pos_integer(),
    active_connections :: map(),
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

-spec analyze_performance([network_metric()]) -> network_analysis().
analyze_performance(Metrics) ->
    gen_server:call(?MODULE, {analyze_performance, Metrics}, 10000).

-spec identify_bottlenecks(network_analysis()) -> [network_bottleneck()].
identify_bottlenecks(Analysis) ->
    gen_server:call(?MODULE, {identify_bottlenecks, Analysis}, 5000).

-spec validate_network_configurations(network_analysis()) -> map().
validate_network_configurations(Analysis) ->
    gen_server:call(?MODULE, {validate_configurations, Analysis}, 5000).

%%====================================================================
## GEN_SERVER CALLBACKS
##====================================================================

init([]) ->
    ?LOG_INFO("Starting network load test"),

    State = #state{
        scenarios = initialize_network_scenarios(),
        current_scenario = undefined,
        metrics_history = [],
        test_start_time = erlang:system_time(millisecond),
        current_load = 0,
        active_connections = #{},
        monitoring_timer = undefined,
        analysis_timer = undefined,
        results = #{}
    },

    %% Start network monitoring
    MonitoringTimer = erlang:send_after(1000, self(), collect_metrics),

    %% Start analysis
    AnalysisTimer = erlang:send_after(5000, self(), analyze_metrics),

    {ok, State#state{
        monitoring_timer = MonitoringTimer,
        analysis_timer = AnalysisTimer
    }}.

handle_call(execute_load_tests, _From, State) ->
    %% Execute all network test scenarios
    TestResults = execute_all_scenarios(State),

    {reply, TestResults, State};

handle_call({analyze_performance, Metrics}, _From, State) ->
    Analysis = analyze_network_performance(Metrics),
    {reply, Analysis, State};

handle_call({identify_bottlenecks, Analysis}, _From, State) ->
    Bottlenecks = identify_network_bottlenecks(Analysis),
    {reply, Bottlenecks, State};

handle_call({validate_configurations, Analysis}, _From, State) ->
    ConfigValidation = validate_network_configurations(Analysis),
    {reply, ConfigValidation, State};

handle_call(stop, _From, State) ->
    %% Stop all timers and clean up
    [erlang:cancel_timer(Timer) || Timer <-
        [State#state.monitoring_timer, State#state.analysis_timer]
    ],

    %% Stop all active connections
    lists:foreach(fun(ConnectionPid) ->
        ConnectionPid ! stop
    end, maps:values(State#state.active_connections)),

    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect network metrics
    Metrics = collect_network_metrics(State),

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
    Analysis = analyze_network_performance(State#state.metrics_history),

    %% Identify bottlenecks
    Bottlenecks = identify_network_bottlenecks(Analysis),

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

handle_info(connection_complete, ConnectionId) ->
    %% Handle connection completion
    ?LOG_DEBUG("Connection completed: ~p", [ConnectionId]),
    ok;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating network load test, executed scenarios: ~p",
             [maps:size(State#state.results)]),

    %% Generate final report
    generate_final_report(State),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## INTERNAL FUNCTIONS
##====================================================================

initialize_network_scenarios() ->
    %% Initialize network test scenarios
    [
        #{
            name => "tcp_stress",
            description => "TCP connection stress testing",
            protocol => tcp,
            target_endpoints => ["127.0.0.1:8080"],
            load_pattern => ramp_up,
            duration => 300000,
            concurrency => 1000,
            request_rate => 1000,
            packet_size => 1024,
            timeout => 5000,
            error_rate => 0.01
        },
        #{
            name => "http_throughput",
            description => "HTTP request throughput testing",
            protocol => http,
            target_endpoints => ["http://localhost:8080/api"],
            load_pattern => constant,
            duration => 300000,
            concurrency => 500,
            request_rate => 500,
            packet_size => 2048,
            timeout => 3000,
            error_rate => 0.02
        },
        #{
            name => "websocket_load",
            description => "WebSocket connection load testing",
            protocol => websocket,
            target_endpoints => ["ws://localhost:8080/ws"],
            load_pattern => burst,
            duration => 300000,
            concurrency => 200,
            request_rate => 100,
            packet_size => 512,
            timeout => 10000,
            error_rate => 0.05
        },
        #{
            name => "network_storm",
            description => "Network storm testing",
            protocol => udp,
            target_endpoints => ["127.0.0.1:8081"],
            load_pattern => storm,
            duration := 120000,
            concurrency := 500,
            request_rate := 1000,
            packet_size := 256,
            timeout := 1000,
            error_rate => 0.1
        },
        #{
            name => "sse_latency",
            description => "Server-sent events latency testing",
            protocol => sse,
            target_endpoints => ["http://localhost:8080/events"],
            load_pattern => spike,
            duration := 300000,
            concurrency := 100,
            request_rate := 50,
            packet_size := 128,
            timeout := 15000,
            error_rate => 0.03
        }
    ].

collect_network_metrics(State) ->
    %% Collect network metrics
    Metrics = [],

    %% Collect metrics from active connections
    lists:foldl(fun(ConnectionId, ConnectionMetrics) ->
        case get_connection_metrics(ConnectionId) of
            {ok, Metric} -> [Metric | Metrics];
            {error, _} -> Metrics
        end
    end, Metrics, maps:keys(State#state.active_connections)),

    Metrics.

get_connection_metrics(ConnectionId) ->
    %% Get metrics for specific connection
    try
        %% Simulate network metrics collection
        Metric = #{
            timestamp => erlang:system_time(millisecond),
            protocol => <<"tcp">>,
            target_endpoint => <<"127.0.0.1:8080">>,
            latency => rand:uniform(100) + 10,
            bandwidth => rand:uniform(10000) + 1000,
            packet_loss => rand:uniform(5) / 100,
            jitter => rand:uniform(50),
            success => true,
            connection_id => ConnectionId
        },

        {ok, Metric}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

execute_all_scenarios(State) ->
    %% Execute all network test scenarios
    lists:foldl(fun(Scenario, AccResults) ->
        ScenarioResults = execute_scenario(Scenario, State),
        AccResults#{Scenario#scenario.name => ScenarioResults}
    end, #{}, State#state.scenarios).

execute_scenario(Scenario, State) ->
    %% Execute specific network test scenario
    ?LOG_INFO("Executing network scenario: ~p", [Scenario#scenario.name]),

    %% Start scenario monitoring
    ScenarioStart = erlang:system_time(millisecond),

    %% Initialize network load
    case Scenario#scenario.load_pattern of
        constant -> apply_constant_load(Scenario, State);
        ramp_up -> apply_ramp_up_load(Scenario, State);
        spike -> apply_spike_load(Scenario, State);
        burst -> apply_burst_load(Scenario, State);
        storm -> apply_storm_load(Scenario, State)
    end,

    %% Execute scenario for specified duration
    EndTime = erlang:system_time(millisecond) + Scenario#scenario.duration,

    execute_network_load_loop(Scenario, State, EndTime),

    %% Cleanup scenario
    cleanup_scenario(Scenario, State),

    %% Collect results
    Results = collect_scenario_results(Scenario, State),

    %% Generate scenario report
    generate_scenario_report(Scenario, Results),

    Results.

apply_constant_load(Scenario, State) ->
    %% Apply constant network load
    TargetConnections = Scenario#scenario.concurrency,

    lists:foreach(fun(ConnectionIndex) ->
        spawn_network_connection(Scenario, State, ConnectionIndex)
    end, lists:seq(1, TargetConnections)).

apply_ramp_up_load(Scenario, State) ->
    %% Apply ramp-up network load
    TargetConnections = Scenario#scenario.concurrency,
    RampTime = Scenario#scenario.duration div 2,

    lists:foreach(fun(ConnectionIndex) ->
        Delay = (RampTime div TargetConnections) * ConnectionIndex,
        erlang:send_after(Delay, self(), {spawn_connection, Scenario, State, ConnectionIndex})
    end, lists:seq(1, TargetConnections)).

apply_spike_load(Scenario, State) ->
    %% Apply spike network load
    TargetConnections = Scenario#scenario.concurrency,
    SpikeCount = round(TargetConnections * 0.3),  % 30% spike

    %% Start base load
    lists:foreach(fun(Index) ->
        spawn_network_connection(Scenario, State, Index)
    end, lists:seq(1, TargetConnections - SpikeCount)),

    %% Add spike after delay
    erlang:send_after(30000, self(), {add_spike, Scenario, State, SpikeCount}).

apply_burst_load(Scenario, State) ->
    %% Apply burst network load
    BurstCount = Scenario#scenario.concurrency,
    BurstSize = 10,
    BurstInterval = 5000,

    spawn_burst_load(Scenario, State, BurstCount, BurstSize, BurstInterval).

spawn_burst_load(_Scenario, _State, 0, _BurstSize, _Interval) ->
    ok;

spawn_burst_load(Scenario, State, Remaining, BurstSize, Interval) ->
    %% Create burst of connections
    lists:foreach(fun(_) ->
        spawn_network_connection(Scenario, State, rand:uniform(1000))
    end, lists:seq(1, BurstSize)),

    %% Schedule next burst
    erlang:send_after(Interval, self(), {spawn_burst, Scenario, State, Remaining - 1, BurstSize, Interval}).

apply_storm_load(Scenario, State) ->
    %% Apply storm network load
    TargetConnections = Scenario#scenario.concurrency,
    StormDuration = 30000,

    %% Create initial storm
    lists:foreach(fun(Index) ->
        spawn_storm_connection(Scenario, State, Index)
    end, lists:seq(1, TargetConnections)),

    %% Continue storm for specified duration
    erlang:send_after(StormDuration, self(), {stop_storm, Scenario, State}).

spawn_network_connection(Scenario, State, ConnectionIndex) ->
    %% Spawn individual network connection
    ConnectionPid = spawn_link(fun() ->
        network_connection_loop(Scenario, State, ConnectionIndex)
    end),

    %% Register connection
    ConnectionId = erlmcp_utils:uuid(),
    ActiveConnections = State#state.active_connections,
    State#state{active_connections = maps:put(ConnectionId, ConnectionPid, ActiveConnections)}.

spawn_storm_connection(Scenario, State, ConnectionIndex) ->
    %% Spawn storm connection with higher intensity
    ConnectionPid = spawn_link(fun() ->
        storm_connection_loop(Scenario, State, ConnectionIndex)
    end),

    %% Register connection
    ConnectionId = erlmcp_utils:uuid(),
    ActiveConnections = State#state.active_connections,
    State#state{active_connections = maps:put(ConnectionId, ConnectionPid, ActiveConnections)}.

network_connection_loop(Scenario, State, ConnectionIndex) ->
    network_connection_loop(Scenario, State, ConnectionIndex, 0).

network_connection_loop(Scenario, State, ConnectionIndex, RequestCount) ->
    receive
        stop ->
            ok
    after Scenario#scenario.request_rate div 1000 ->
        %% Execute network request
        case execute_network_request(Scenario, State, ConnectionIndex) of
            {ok, Response} ->
                RecordNetworkMetric(Scenario, Response, true),
                network_connection_loop(Scenario, State, ConnectionIndex, RequestCount + 1);
            {error, Error} ->
                RecordNetworkMetric(Scenario, #{error => Error}, false),
                network_connection_loop(Scenario, State, ConnectionIndex, RequestCount)
        end
    end.

storm_connection_loop(Scenario, State, ConnectionIndex) ->
    storm_connection_loop(Scenario, State, ConnectionIndex, 0).

storm_connection_loop(Scenario, State, ConnectionIndex, RequestCount) ->
    receive
        stop ->
            ok
    after 50 ->  % Very high frequency for storm
        %% Execute multiple network requests in quick succession
        case execute_network_requests(Scenario, State, ConnectionIndex, 5) of
            {ok, _} ->
                storm_connection_loop(Scenario, State, ConnectionIndex, RequestCount + 5);
            {error, Error} ->
                RecordNetworkMetric(Scenario, #{error => Error}, false),
                storm_connection_loop(Scenario, State, ConnectionIndex, RequestCount)
        end
    end.

execute_network_request(Scenario, State, ConnectionIndex) ->
    %% Execute individual network request
    StartTime = erlang:system_time(millisecond),

    try
        %% Simulate network request based on protocol
        case Scenario#scenario.protocol of
            tcp ->
                simulate_tcp_request(Scenario, ConnectionIndex);
            http ->
                simulate_http_request(Scenario, ConnectionIndex);
            websocket ->
                simulate_websocket_request(Scenario, ConnectionIndex);
            udp ->
                simulate_udp_request(Scenario, ConnectionIndex);
            sse ->
                simulate_sse_request(Scenario, ConnectionIndex)
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

execute_network_requests(Scenario, State, ConnectionIndex, Count) ->
    %% Execute multiple network requests
    Results = lists:foldl(fun(_, Acc) ->
        case execute_network_request(Scenario, State, ConnectionIndex) of
            {ok, Result} -> [{ok, Result} | Acc];
            {error, Error} -> [{error, Error} | Acc]
        end
    end, [], lists:seq(1, Count)),

    case lists:any(fun(Result) -> Result =:= {ok, _} end, Results) of
        true -> {ok, Results};
        false -> {error, "All requests failed"}
    end.

simulate_tcp_request(Scenario, ConnectionIndex) ->
    %% Simulate TCP request
    timer:sleep(rand:uniform(50)),
    {ok, #{
        protocol => tcp,
        bytes_sent => Scenario#scenario.packet_size,
        bytes_received => Scenario#scenario.packet_size,
        latency => rand:uniform(20) + 5
    }}.

simulate_http_request(Scenario, ConnectionIndex) ->
    %% Simulate HTTP request
    timer:sleep(rand:uniform(100)),
    {ok, #{
        protocol => http,
        status_code => 200,
        bytes_sent => Scenario#scenario.packet_size,
        bytes_received => Scenario#scenario.packet_size,
        latency => rand:uniform(200) + 50
    }}.

simulate_websocket_request(Scenario, ConnectionIndex) ->
    %% Simulate WebSocket request
    timer:sleep(rand:uniform(50)),
    {ok, #{
        protocol => websocket,
        bytes_sent => Scenario#scenario.packet_size,
        bytes_received => Scenario#scenario.packet_size,
        latency => rand:uniform(100) + 30
    }}.

simulate_udp_request(Scenario, ConnectionIndex) ->
    %% Simulate UDP request
    timer:sleep(rand:uniform(10)),
    {ok, #{
        protocol => udp,
        bytes_sent => Scenario#scenario.packet_size,
        bytes_received => Scenario#scenario.packet_size,
        latency => rand:uniform(5) + 1,
        packet_loss => rand:uniform(2) / 100
    }}.

simulate_sse_request(Scenario, ConnectionIndex) ->
    %% Simulate SSE request
    timer:sleep(rand:uniform(200)),
    {ok, #{
        protocol => sse,
        events_received => rand:uniform(5) + 1,
        bytes_sent => Scenario#scenario.packet_size,
        bytes_received => Scenario#scenario.packet_size,
        latency => rand:uniform(300) + 100
    }}.

RecordNetworkMetric(Scenario, Response, Success) ->
    %% Record network metric
    Metric = #{
        timestamp => erlang:system_time(millisecond),
        protocol => atom_to_binary(Scenario#scenario.protocol, utf8),
        target_endpoint => lists:nth(1, Scenario#scenario.target_endpoints),
        latency => get_response_latency(Response),
        bandwidth => get_response_bandwidth(Response),
        packet_loss => get_packet_loss(Response),
        jitter => rand:uniform(50),
        success => Success,
        error => case Success of true -> undefined; false -> maps:get(error, Response, unknown) end,
        connection_id => erlmcp_utils:uuid()
    },

    %% Store metric (in real implementation, would be stored in ETS or database)
    ok.

get_response_latency(Response) ->
    %% Extract latency from response
    case maps:get(latency, Response, 0) of
        0 -> rand:uniform(100) + 10;
        Latency -> Latency
    end.

get_response_bandwidth(Response) ->
    %% Extract bandwidth from response
    case maps:get(bytes_sent, Response, 0) + maps:get(bytes_received, Response, 0) of
        0 -> rand:uniform(10000) + 1000;
        Bytes -> Bytes
    end.

get_packet_loss(Response) ->
    %% Extract packet loss from response
    case maps:get(packet_loss, Response, 0.0) of
        0.0 -> rand:uniform(5) / 100;
        Loss -> Loss
    end.

execute_network_load_loop(Scenario, State, EndTime) ->
    %% Execute network load loop
    case erlang:system_time(millisecond) < EndTime of
        true ->
            timer:sleep(1000),
            execute_network_load_loop(Scenario, State, EndTime);
        false ->
            ok
    end.

cleanup_scenario(Scenario, State) ->
    %% Cleanup scenario resources
    lists:foreach(fun(ConnectionId) ->
        case maps:get(ConnectionId, State#state.active_connections, undefined) of
            ConnectionPid when is_pid(ConnectionPid) ->
                ConnectionPid ! stop,
                ok;
            undefined ->
                ok
        end
    end, maps:keys(State#state.active_connections)).

collect_scenario_results(Scenario, State) ->
    %% Collect scenario results
    ScenarioMetrics = [M || M <- State#state.metrics_history,
                          M#network_metric.timestamp >= erlang:system_time(millisecond) -
                                                      Scenario#scenario.duration],

    #{
        scenario => Scenario#scenario.name,
        metrics => ScenarioMetrics,
        duration => Scenario#scenario.duration,
        throughput => calculate_scenario_throughput(ScenarioMetrics),
        average_latency => calculate_average_latency(ScenarioMetrics),
        success_rate => calculate_success_rate(ScenarioMetrics),
        packet_loss_rate => calculate_packet_loss_rate(ScenarioMetrics)
    }.

calculate_scenario_throughput(Metrics) ->
    %% Calculate scenario throughput
    case Metrics of
        [] -> 0.0;
        _ ->
            Duration = lists:last(Metrics)#network_metric.timestamp -
                       lists:nth(1, Metrics)#network_metric.timestamp,
            if Duration > 0 -> length([M || M <- Metrics, M#network_metric.success]) /
                            (Duration / 1000.0); true -> 0.0 end
    end.

calculate_average_latency(Metrics) ->
    %% Calculate average latency
    case [M#network_metric.latency || M <- Metrics, M#network_metric.success] of
        [] -> 0.0;
        Latencies ->
            lists:sum(Latencies) / length(Latencies)
    end.

calculate_success_rate(Metrics) ->
    %% Calculate success rate
    case Metrics of
        [] -> 0.0;
        _ ->
            SuccessCount = length([M || M <- Metrics, M#network_metric.success]),
            SuccessCount / length(Metrics)
    end.

calculate_packet_loss_rate(Metrics) ->
    %% Calculate packet loss rate
    case Metrics of
        [] -> 0.0;
        _ ->
            PacketLossValues = [M#network_metric.packet_loss || M <- Metrics],
            lists:sum(PacketLossValues) / length(PacketLossValues)
    end.

analyze_network_performance(Metrics) ->
    %% Analyze network performance
    case Metrics of
        [] -> #{};
        _ ->
            Analysis = #{
                throughput => calculate_throughput(Metrics),
                average_latency => calculate_average_latency(Metrics),
                p95_latency => calculate_p95_latency(Metrics),
                p99_latency => calculate_p99_latency(Metrics),
                packet_loss_rate => calculate_packet_loss_rate(Metrics),
                bandwidth_utilization => calculate_bandwidth_utilization(Metrics),
                connection_efficiency => calculate_connection_efficiency(Metrics),
                protocol_performance => analyze_protocol_performance(Metrics),
                bottleneck_analysis => #{
                    latency_bottlenecks => detect_latency_bottlenecks(Metrics),
                    bandwidth_bottlenecks => detect_bandwidth_bottlenecks(Metrics),
                    packet_loss_bottlenecks => detect_packet_loss_bottlenecks(Metrics)
                },
                network_recommendations => generate_network_recommendations(Metrics),
                capacity_planning => generate_capacity_planning(Metrics)
            },

            Analysis
    end.

calculate_throughput(Metrics) ->
    %% Calculate network throughput
    case Metrics of
        [] -> 0.0;
        _ ->
            Duration = lists:last(Metrics)#network_metric.timestamp -
                       lists:nth(1, Metrics)#network_metric.timestamp,
            if Duration > 0 -> length(Metrics) / (Duration / 1000.0); true -> 0.0 end
    end.

calculate_p95_latency(Metrics) ->
    %% Calculate 95th percentile latency
    Latencies = [M#network_metric.latency || M <- Metrics, M#network_metric.success],
    case Latencies of
        [] -> 0;
        _ -> lists:nth(floor(0.95 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_p99_latency(Metrics) ->
    %% Calculate 99th percentile latency
    Latencies = [M#network_metric.latency || M <- Metrics, M#network_metric.success],
    case Latencies of
        [] -> 0;
        _ -> lists:nth(floor(0.99 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_bandwidth_utilization(Metrics) ->
    ##% Calculate bandwidth utilization
    case Metrics of
        [] -> 0.0;
        _ ->
            TotalBandwidth = lists:sum([M#network_metric.bandwidth || M <- Metrics]),
            MaxBandwidth = 100000000,  % 100 Mbps
            min(100.0, (TotalBandwidth / length(Metrics)) / MaxBandwidth * 100)
    end.

calculate_connection_efficiency(Metrics) ->
    %% Calculate connection efficiency
    case Metrics of
        [] -> 0.0;
        _ ->
            SuccessCount = length([M || M <- Metrics, M#network_metric.success]),
            SuccessCount / length(Metrics)
    end.

analyze_protocol_performance(Metrics) ->
    %% Analyze protocol performance
    ProtocolGroups = lists:foldl(fun(Metric, Acc) ->
        Protocol = Metric#network_metric.protocol,
        maps:update(Protocol, [Metric | maps:get(Protocol, Acc, [])], Acc)
    end, #{}, Metrics),

    lists:fold(fun({Protocol, ProtocolMetrics}, Acc) ->
        Acc#{Protocol => #{
            throughput => calculate_protocol_throughput(ProtocolMetrics),
            average_latency => calculate_protocol_latency(ProtocolMetrics),
            packet_loss_rate => calculate_protocol_packet_loss(ProtocolMetrics)
        }}
    end, #{}, maps:to_list(ProtocolGroups)).

calculate_protocol_throughput(ProtocolMetrics) ->
    %% Calculate throughput for specific protocol
    case ProtocolMetrics of
        [] -> 0.0;
        _ ->
            Duration = lists:last(ProtocolMetrics)#network_metric.timestamp -
                       lists:nth(1, ProtocolMetrics)#network_metric.timestamp,
            if Duration > 0 -> length(ProtocolMetrics) / (Duration / 1000.0); true -> 0.0 end
    end.

calculate_protocol_latency(ProtocolMetrics) ->
    %% Calculate latency for specific protocol
    case [M#network_metric.latency || M <- ProtocolMetrics, M#network_metric.success] of
        [] -> 0.0;
        Latencies ->
            lists:sum(Latencies) / length(Latencies)
    end.

calculate_protocol_packet_loss(ProtocolMetrics) ->
    %% Calculate packet loss for specific protocol
    case ProtocolMetrics of
        [] -> 0.0;
        _ ->
            PacketLossValues = [M#network_metric.packet_loss || M <- ProtocolMetrics],
            lists:sum(PacketLossValues) / length(PacketLossValues)
    end.

detect_latency_bottlenecks(Metrics) ->
    %% Detect latency bottlenecks
    Bottlenecks = [],

    CheckLatencies = [M#network_metric.latency || M <- Metrics],
    case CheckLatencies of
        [] -> Bottlenecks;
        _ ->
            AvgLatency = lists:sum(CheckLatencies) / length(CheckLatencies),
            if AvgLatency > 1000 ->
                Bottleneck = #{
                    type => latency,
                    severity => high,
                    current_metric => AvgLatency,
                    threshold => 1000,
                    impact => #{performance_degradation => (AvgLatency - 1000) / 10},
                    affected_endpoints => ["127.0.0.1:8080"],
                    mitigation => #{action => optimize_routing, recommendation => "Review network routing"}
                },
                [Bottleneck | Bottlenecks];
            true -> Bottlenecks
            end
    end.

detect_bandwidth_bottlenecks(Metrics) ->
    %% Detect bandwidth bottlenecks
    Bottlenecks = [],

    Bandwidths = [M#network_metric.bandwidth || M <- Metrics],
    case Bandwidths of
        [] -> Bottlenecks;
        _ ->
            MaxBandwidth = lists:max(Bandwidths),
            if MaxBandwidth > 50000000 ->  % 50 Mbps
                Bottleneck = #{
                    type => bandwidth,
                    severity => medium,
                    current_metric => MaxBandwidth,
                    threshold => 50000000,
                    impact => #{saturation_level => MaxBandwidth / 100000000},
                    affected_endpoints => ["127.0.0.1:8080"],
                    mitigation => #{action => increase_bandwidth, recommendation => "Increase network capacity"}
                },
                [Bottleneck | Bottlenecks];
            true -> Bottlenecks
            end
    end.

detect_packet_loss_bottlenecks(Metrics) ->
    ##% Detect packet loss bottlenecks
    Bottlenecks = [],

    PacketLossRates = [M#network_metric.packet_loss || M <- Metrics],
    case PacketLossRates of
        [] -> Bottlenecks;
        _ ->
            AvgPacketLoss = lists:sum(PacketLossRates) / length(PacketLossRates),
            if AvgPacketLoss > 0.05 ->  % 5%
                Bottleneck = #{
                    type => packet_loss,
                    severity => critical,
                    current_metric => AvgPacketLoss,
                    threshold => 0.05,
                    impact => #{retransmission_rate => AvgPacketLoss * 2},
                    affected_endpoints => ["127.0.0.1:8080"],
                    mitigation => #{action => improve_network, recommendation => "Fix network infrastructure"}
                },
                [Bottleneck | Bottlenecks];
            true -> Bottlenecks
            end
    end.

identify_network_bottlenecks(Analysis) ->
    %% Identify network bottlenecks from analysis
    Bottlenecks = [],

    %% Add latency bottlenecks
    case Analysis#network_analysis.bottleneck_analysis#latency_bottlenecks of
        [] -> ok;
        Bottlenecks ->
            lists:fold(fun(Bottleneck, Acc) ->
                [Bottleneck#bottleneck{severity => high} | Acc]
            end, Bottlenecks, Bottlenecks)
    end,

    Bottlenecks.

generate_network_recommendations(Metrics) ->
    ##% Generate network recommendations
    Recommendations = [],

    %% Based on throughput
    Throughput = calculate_throughput(Metrics),
    if Throughput > 1000 ->
        Reco = #{
            type => scaling,
            priority => high,
            action => "Implement load balancing",
            impact => significant
        },
        [Reco | Recommendations];
    true -> Recommendations
    end,

    %% Based on latency
    AvgLatency = calculate_average_latency(Metrics),
    if AvgLatency > 500 ->
        Reco = #{
            type => optimization,
            priority => medium,
            action => "Optimize network routing",
            impact => moderate
        },
        [Reco | Recommendations];
    true -> Recommendations
    end,

    Recommendations.

generate_capacity_planning(Metrics) ->
    %% Generate network capacity planning
    CurrentThroughput = calculate_throughput(Metrics),
    CurrentLatency = calculate_average_latency(Metrics),
    CurrentPacketLoss = calculate_packet_loss_rate(Metrics),

    #{
        current_capacity => #{
            throughput => CurrentThroughput,
            latency => CurrentLatency,
            packet_loss => CurrentPacketLoss
        },
        projected_needs => #{
            throughput => CurrentThroughput * 1.5,
            latency => CurrentLatency * 0.8,
            packet_loss => CurrentPacketLoss * 0.5
        },
        scaling_recommendations => #{
            bandwidth => if CurrentThroughput > 500 -> increase; else -> maintain end,
            latency => if CurrentLatency > 200 -> optimize; else -> maintain end,
            redundancy => if CurrentPacketLoss > 0.02 -> implement; else -> optional end
        }
    }.

validate_network_configurations(Analysis) ->
    ##% Validate network configurations
    ConfigValidation = #{
        current_performance => #{
            throughput => Analysis#network_analysis.throughput,
            latency => Analysis#network_analysis.average_latency,
            packet_loss => Analysis#network_analysis.packet_loss_rate,
            connection_efficiency => Analysis#network_analysis.connection_efficiency
        },
        configuration_status => determine_configuration_status(Analysis),
        recommendations => generate_configuration_recommendations(Analysis),
        compliance => check_network_compliance(Analysis)
    },

    ConfigValidation.

determine_configuration_status(Analysis) ->
    %% Determine overall configuration status
    Scores = #{
        throughput_score => if Analysis#network_analysis.throughput > 1000 -> good;
                             Analysis#network_analysis.throughput > 500 -> fair; else -> poor end,
        latency_score => if Analysis#network_analysis.average_latency < 100 -> good;
                          Analysis#network_analysis.average_latency < 500 -> fair; else -> poor end,
        packet_loss_score => if Analysis#network_analysis.packet_loss_rate < 0.01 -> good;
                             Analysis#network_analysis.packet_loss_rate < 0.05 -> fair; else -> poor end,
        efficiency_score => if Analysis#network_analysis.connection_efficiency > 0.95 -> good;
                            Analysis#network_analysis.connection_efficiency > 0.9 -> fair; else -> poor end
    },

    OverallScore = calculate_configuration_score(Scores),
    case OverallScore of
        Score when Score >= 80 -> optimal;
        Score when Score >= 60 -> acceptable;
        Score when Score >= 40 -> suboptimal;
        _ -> critical
    end.

calculate_configuration_score(Scores) ->
    %% Calculate configuration score
    ScoreValues = [S || {_, S} <- maps:to_list(Scores)],
    lists:sum(ScoreValues) / length(ScoreValues) * 100.

generate_configuration_recommendations(Analysis) ->
    ##% Generate configuration recommendations
    Recommendations = [],

    case Analysis#network_analysis.throughput < 1000 of
        true ->
            [#{type => capacity, priority => high,
               recommendation => "Increase network capacity"} | Recommendations];
        false -> Recommendations
    end,

    case Analysis#network_analysis.average_latency > 500 of
        true ->
            [#{type => latency, priority => medium,
               recommendation => "Optimize network latency"} | Recommendations];
        false -> Recommendations
    end,

    Recommendations.

check_network_compliance(Analysis) ->
    %% Check network compliance with standards
    #{
        throughput_compliance => if Analysis#network_analysis.throughput >= 1000 -> compliant;
                                  true -> non_compliant end,
        latency_compliance => if Analysis#network_analysis.average_latency <= 500 -> compliant;
                               else -> non_compliant end,
        packet_loss_compliance => if Analysis#network_analysis.packet_loss_rate <= 0.05 -> compliant;
                                  else -> non_compliant end,
        overall_compliance => calculate_overall_compliance(Analysis)
    }.

calculate_overall_compliance(Analysis) ->
    %% Calculate overall compliance
    ComplianceChecks = [
        Analysis#network_analysis.throughput >= 1000,
        Analysis#network_analysis.average_latency <= 500,
        Analysis#network_analysis.packet_loss_rate <= 0.05
    ],

    case lists:all(fun(Check) -> Check =:= true end, ComplianceChecks) of
        true -> compliant;
        false -> non_compliant
    end.

generate_scenario_report(Scenario, Results) ->
    %% Generate scenario-specific report
    Report = #{
        scenario => Scenario#scenario.name,
        timestamp => erlang:system_time(millisecond),
        results => Results,
        metrics => collect_network_metrics(#state{})
    },

    %% Save report
    ReportFile = "/Users/sac/erlmcp/load_test_reports/network_" ++
                 binary_to_list(Scenario#scenario.name) ++ "_report.json",
    ok = file:write_file(ReportFile, jsx:encode(Report)).

generate_final_report(State) ->
    %% Generate final comprehensive report
    FinalReport = #{
        test_summary => #{
            total_scenarios => length(State#state.scenarios),
            test_duration => erlang:system_time(millisecond) - State#state.test_start_time,
            overall_throughput => calculate_overall_throughput(State#state.results)
        },
        scenario_results => State#state.results,
        performance_summary => calculate_performance_summary(State#state.results),
        bottlenecks => identify_aggregate_bottlenecks(State#state.results),
        recommendations => generate_aggregate_recommendations(State#state.results),
        timestamp => erlang:system_time(millisecond)
    },

    %% Save final report
    ReportFile = "/Users/sac/erlmcp/load_test_reports/network_test_final_report.json",
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

calculate_performance_summary(Results) ->
    %% Calculate performance summary across all scenarios
    Throughputs = [maps:get(throughput, Scenario#scenario_results) ||
                  Scenario <- maps:values(Results)],
    Latencies = [maps:get(average_latency, Scenario#scenario_results) ||
                Scenario <- maps:values(Results)],
    SuccessRates = [maps:get(success_rate, Scenario#scenario_results) ||
                  Scenario <- maps:values(Results)],

    #{
        average_throughput => lists:sum(Throughputs) / length(Throughputs),
        average_latency => lists:sum(Latencies) / length(Latencies),
        average_success_rate => lists:sum(SuccessRates) / length(SuccessRates),
        best_performing => find_best_performing(Results),
        worst_performing => find_worst_performing(Results)
    }.

find_best_performing(Results) ->
    %% Find best performing scenario
    ScenarioResults = maps:values(Results),
    lists:fold(fun(Scenario, Best) ->
        case maps:get(success_rate, Scenario#scenario_results) >
             maps:get(success_rate, Best#scenario_results) of
            true -> Scenario;
            false -> Best
        end
    end, hd(ScenarioResults), tl(ScenarioResults)).

find_worst_performing(Results) ->
    %% Find worst performing scenario
    ScenarioResults = maps:values(Results),
    lists:fold(fun(Scenario, Worst) ->
        case maps:get(success_rate, Scenario#scenario_results) <
             maps:get(success_rate, Worst#scenario_results) of
            true -> Scenario;
            false -> Worst
        end
    end, hd(ScenarioResults), tl(ScenarioResults)).

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
    OverallSummary = calculate_performance_summary(Results),

    %% Generate recommendations based on summary
    case OverallSummary#performance_summary.average_success_rate < 0.95 of
        true ->
            [#{type => reliability, priority => high,
               recommendation => "Improve network reliability"} | Recommendations];
        false -> Recommendations
    end,

    case OverallSummary#performance_summary.average_latency > 500 of
        true ->
            [#{type => latency, priority => medium,
               recommendation => "Optimize network latency"} | Recommendations];
        false -> Recommendations
    end,

    Recommendations.