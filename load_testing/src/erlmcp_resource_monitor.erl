-module(erlmcp_resource_monitor).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

-behaviour(gen_server).

%% API exports
-export([
    start/0,
    stop/0,
    monitor_resources/0,
    get_resource_usage/0,
    identify_bottlenecks/0,
    generate_resource_report/0
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

-type resource_metric() :: #{
    timestamp := integer(),
    cpu := #{
        total_usage := float(),
        erlmcp_usage := float(),
        load_average := [float()],
        core_usage := [float()],
        temp => float()
    },
    memory := #{
        total := integer(),
        used := integer(),
        available := integer(),
        erlmcp_used => integer(),
        erlmcp_heap => integer(),
        gc_stats => map()
    },
    network := #{
        bytes_in := integer(),
        bytes_out := integer(),
        packets_in := integer(),
        packets_out := integer(),
        connections := integer(),
        connections_active => integer()
    },
    disk := #{
        read_bytes := integer(),
        write_bytes := integer(),
        read_ops := integer(),
        write_ops := integer(),
        usage => float(),
        inodes => map()
    },
    process := #{
        count := integer(),
        erlmcp_count => integer(),
        memory => integer(),
        threads => integer()
    },
    application := #{
        memory => integer(),
        cpu => float(),
        response_time => float(),
        throughput => float(),
        error_rate => float()
    }
}.

-type resource_threshold() :: #{
    cpu := #{
        critical := float(),
        warning := float(),
        erlmcp_critical => float(),
        erlmcp_warning => float()
    },
    memory := #{
        critical := float(),
        warning => float(),
        erlmcp_critical => integer(),
        erlmcp_warning => integer()
    },
    network := #{
        critical := integer(),
        warning => integer(),
        connections_critical => integer()
    },
    disk := #{
        critical := float(),
        warning => float()
    },
    process := #{
        critical := integer(),
        warning => integer()
    }
}.

-type resource_bottleneck() :: #{
    type := cpu | memory | network | disk | process | application,
    severity := low | medium | high | critical,
    current_usage => float(),
    threshold => float(),
    impact => map(),
    duration := integer(),
    trend => increasing | decreasing | stable
}.

-type resource_report() :: #{
    summary := map(),
    metrics := [resource_metric()],
    bottlenecks := [resource_bottleneck()],
    trends => map(),
    recommendations => [map()],
    timestamp => integer()
}.

%%====================================================================
## GEN_SERVER STATE
##====================================================================

-record(state, {
    metrics_history :: [resource_metric()],
    bottlenecks :: [resource_bottleneck()],
    thresholds :: resource_threshold(),
    monitoring_interval :: pos_integer(),
    alert_threshold :: pos_integer(),
    trend_buffer :: map(),
    metrics_timer :: reference() | undefined,
    analysis_timer :: reference() | undefined,
    start_time :: integer()
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

-spec monitor_resources() -> ok.
monitor_resources() ->
    gen_server:call(?MODULE, monitor_resources, 5000).

-spec get_resource_usage() -> resource_metric().
get_resource_usage() ->
    gen_server:call(?MODULE, get_resource_usage, 5000).

-spec identify_bottlenecks() -> [resource_bottleneck()].
identify_bottlenecks() ->
    gen_server:call(?MODULE, identify_bottlenecks, 5000).

-spec generate_resource_report() -> resource_report().
generate_resource_report() ->
    gen_server:call(?MODULE, generate_resource_report, 5000).

%%====================================================================
## GEN_SERVER CALLBACKS
##====================================================================

init([]) ->
    ?LOG_INFO("Starting resource monitor"),

    State = #state{
        metrics_history = [],
        bottlenecks = [],
        thresholds = initialize_thresholds(),
        monitoring_interval = 1000,  % 1 second
        alert_threshold = 5,  % 5 consecutive alerts
        trend_buffer = initialize_trend_buffer(),
        metrics_timer = undefined,
        analysis_timer = undefined,
        start_time = erlang:system_time(millisecond)
    },

    %% Start resource monitoring
    MetricsTimer = erlang:send_after(State#state.monitoring_interval, self(), collect_metrics),

    %% Start analysis
    AnalysisTimer = erlang:send_after(10000, self(), analyze_metrics),

    {ok, State#state{metrics_timer = MetricsTimer, analysis_timer = AnalysisTimer}}.

handle_call(monitor_resources, _From, State) ->
    %% Start/stop resource monitoring
    case State#state.metrics_timer of
        undefined ->
            MetricsTimer = erlang:send_after(State#state.monitoring_interval, self(), collect_metrics),
            {reply, ok, State#state{metrics_timer = MetricsTimer}};
        _ ->
            erlang:cancel_timer(State#state.metrics_timer),
            {reply, ok, State#state{metrics_timer = undefined}}
    end;

handle_call(get_resource_usage, _From, State) ->
    case State#state.metrics_history of
        [] ->
            {reply, #{error => no_metrics}, State};
        _ ->
            LatestMetrics = lists:last(State#state.metrics_history),
            {reply, LatestMetrics, State}
    end;

handle_call(identify_bottlenecks, _From, State) ->
    Bottlenecks = identify_resource_bottlenecks(State),
    {reply, Bottlenecks, State};

handle_call(generate_resource_report, _From, State) ->
    Report = generate_report(State),
    {reply, Report, State};

handle_call(stop, _From, State) ->
    %% Stop all timers
    [erlang:cancel_timer(Timer) || Timer <-
        [State#state.metrics_timer, State#state.analysis_timer]
    ],

    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect resource metrics
    Metrics = collect_current_metrics(),

    %% Update metrics history
    NewMetricsHistory = [Metrics | State#state.metrics_history],
    if length(NewMetricsHistory) > 1000 ->
        TruncatedHistory = lists:sublist(NewMetricsHistory, 1000),
        {NewHistory, UpdatedTrendBuffer} = update_trend_buffer(TruncatedHistory, State#state.trend_buffer);
    true ->
        {NewHistory, UpdatedTrendBuffer} = update_trend_buffer(NewMetricsHistory, State#state.trend_buffer)
    end,

    %% Schedule next metrics collection
    MetricsTimer = erlang:send_after(State#state.monitoring_interval, self(), collect_metrics),

    {noreply, State#state{
        metrics_history = NewHistory,
        trend_buffer = UpdatedTrendBuffer,
        metrics_timer = MetricsTimer
    }};

handle_info(analyze_metrics, State) ->
    %% Analyze collected metrics
    Analysis = analyze_resource_metrics(State#state.metrics_history),

    %% Update bottlenecks
    Bottlenecks = identify_bottlenecks_from_analysis(Analysis, State),

    %% Schedule next analysis
    AnalysisTimer = erlang:send_after(10000, self(), analyze_metrics),

    {noreply, State#state{
        bottlenecks = Bottlenecks,
        analysis_timer = AnalysisTimer
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating resource monitor, collected metrics: ~p",
             [length(State#state.metrics_history)]),

    %% Generate final report
    Report = generate_report(State),
    save_resource_report(Report),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## INTERNAL FUNCTIONS
##====================================================================

initialize_thresholds() ->
    %% Initialize resource monitoring thresholds
    #{
        cpu => #{
            critical => 95.0,
            warning => 80.0,
            erlmcp_critical => 90.0,
            erlmcp_warning => 70.0
        },
        memory => #{
            critical => 95.0,
            warning => 85.0,
            erlmcp_critical => 80.0,
            erlmcp_warning => 60.0
        },
        network => #{
            critical => 100 * 1024 * 1024,  % 100 MB/s
            warning => 50 * 1024 * 1024,   % 50 MB/s
            connections_critical => 10000
        },
        disk => #{
            critical => 95.0,
            warning => 85.0
        },
        process => #{
            critical => 50000,
            warning => 30000
        }
    }.

initialize_trend_buffer() ->
    %% Initialize trend buffer for each resource type
    #{
        cpu => [],
        memory => [],
        network => [],
        disk => [],
        process => [],
        application => []
    }.

collect_current_metrics() ->
    %% Collect current resource metrics
    #{
        timestamp => erlang:system_time(millisecond),
        cpu => collect_cpu_metrics(),
        memory => collect_memory_metrics(),
        network => collect_network_metrics(),
        disk => collect_disk_metrics(),
        process => collect_process_metrics(),
        application => collect_application_metrics()
    }.

collect_cpu_metrics() ->
    %% Collect CPU metrics
    #{
        total_usage => get_cpu_total_usage(),
        erlmcp_usage => get_erlmcp_cpu_usage(),
        load_average => get_load_average(),
        core_usage => get_core_usage(),
        temp => get_cpu_temperature()
    }.

collect_memory_metrics() ->
    %% Collect memory metrics
    #{
        total => get_total_memory(),
        used => get_used_memory(),
        available => get_available_memory(),
        erlmcp_used => get_erlmcp_memory(),
        erlmcp_heap => get_erlmcp_heap(),
        gc_stats => get_gc_stats()
    }.

collect_network_metrics() ->
    %% Collect network metrics
    #{
        bytes_in => get_network_bytes_in(),
        bytes_out => get_network_bytes_out(),
        packets_in => get_network_packets_in(),
        packets_out => get_network_packets_out(),
        connections => get_connection_count(),
        connections_active => get_active_connections()
    }.

collect_disk_metrics() ->
    ##% Collect disk metrics
    #{
        read_bytes => get_disk_read_bytes(),
        write_bytes => get_disk_write_bytes(),
        read_ops => get_disk_read_ops(),
        write_ops => get_disk_write_ops(),
        usage => get_disk_usage(),
        inodes => get_disk_inodes()
    }.

collect_process_metrics() ->
    %% Collect process metrics
    #{
        count => get_process_count(),
        erlmcp_count => get_erlmcp_process_count(),
        memory => get_process_memory(),
        threads => get_process_threads()
    }.

collect_application_metrics() ->
    %% Collect application metrics
    #{
        memory => get_application_memory(),
        cpu => get_application_cpu(),
        response_time => get_response_time(),
        throughput => get_throughput(),
        error_rate => get_error_rate()
    }.

get_cpu_total_usage() ->
    %% Get total CPU usage percentage
    {ok, Usage} = cpu_sup:util(),
    Usage.

get_erlmcp_cpu_usage() ->
    %% Get erlmcp-specific CPU usage
    Processes = erlang:processes(),
    TotalCPUTime = lists:fold(fun(Pid, Acc) ->
        case process_info(Pid, reductions) of
            {reductions, Red} -> Acc + Red;
            undefined -> Acc
        end
    end, 0, Processes),
    TotalCPUTime.

get_load_average() ->
    %% Get system load average
    {Load1, Load5, Load15} = cpu_sup:avg(60),
    [Load1, Load5, Load15].

get_core_usage() ->
    %% Get per-core CPU usage
    cpu_sup:util().

get_cpu_temperature() ->
    %% Get CPU temperature (if available)
    case os:type() of
        {unix, linux} ->
            try
                {ok, Content} = file:read_file("/sys/class/thermal/thermal_zone0/temp"),
                Temp = list_to_integer(string:trim(Content)),
                Temp / 1000
            catch
                _ -> undefined
            end;
        _ -> undefined
    end.

get_total_memory() ->
    %% Get total memory in bytes
    {ok, MemInfo} = file:read_file("/proc/meminfo"),
    TotalMatch = re:run(MemInfo, "MemTotal:\\s+(\\d+)", [{capture, all_but_first, list}]),
    case TotalMatch of
        {match, [SizeStr]} -> list_to_integer(SizeStr) * 1024;
        _ -> 0
    end.

get_used_memory() ->
    %% Get used memory in bytes
    {ok, MemInfo} = file:read_file("/proc/meminfo"),
    MemFreeMatch = re:run(MemInfo, "MemFree:\\s+(\\d+)", [{capture, all_but_first, list}]),
    BuffersMatch = re:run(MemInfo, "Buffers:\\s+(\\d+)", [{capture, all_but_first, list}]),
    CachedMatch = re:run(MemInfo, "Cached:\\s+(\\d+)", [{capture, all_but_first, list}]),

    case {MemFreeMatch, BuffersMatch, CachedMatch} of
        {{match, [FreeStr]}, {match, [BuffersStr]}, {match, [CachedStr]}} ->
            (list_to_integer(FreeStr) + list_to_integer(BuffersStr) + list_to_integer(CachedStr)) * 1024;
        _ -> 0
    end.

get_available_memory() ->
    %% Get available memory
    Total = get_total_memory(),
    Used = get_used_memory(),
    max(0, Total - Used).

get_erlmcp_memory() ->
    %% Get erlmcp memory usage
    Processes = erlang:processes(),
    list:fold(fun(Pid, Acc) ->
        case process_info(Pid, memory) of
            {memory, Size} -> Acc + Size;
            undefined -> Acc
        end
    end, 0, Processes).

get_erlmcp_heap() ->
    %% Get erlmcp heap memory
    Processes = erlang:processes(),
    list:fold(fun(Pid, Acc) ->
        case process_info(Pid, heap_size) of
            {heap_size, Size} -> Acc + Size;
            undefined -> Acc
        end
    end, 0, Processes).

get_gc_stats() ->
    %% Get GC statistics
    Garbage = lists:fold(fun(Pid, Acc) ->
        case process_info(Pid, garbage_collection) of
            {garbage_collection, GCStats} -> GCStats;
            undefined -> Acc
        end
    end, #{}, erlang:processes()),
    Garbage.

get_network_bytes_in() ->
    %% Get network bytes received
    {ok, NetStats} = file:read_file("/proc/net/dev"),
    case re:run(NetStats, "eth0.*\\n.*\\s+(\\d+)", [{capture, all_but_first, list}]) of
        {match, [BytesStr]} -> list_to_integer(BytesStr);
        _ -> 0
    end.

get_network_bytes_out() ->
    %% Get network bytes sent
    {ok, NetStats} = file:read_file("/proc/net/dev"),
    case re:run(NetStats, "eth0.*\\n.*\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+(\\d+)", [{capture, all_but_first, list}]) of
        {match, [BytesStr]} -> list_to_integer(BytesStr);
        _ -> 0
    end.

get_network_packets_in() ->
    %% Get network packets received
    0.

get_network_packets_out() ->
    %% Get network packets sent
    0.

get_connection_count() ->
    %% Get total connection count
    {ok, Connections} = file:read_file("/proc/net/tcp"),
    length(re:split(Connections, "\n", [{return, list}, trim])) - 2.

get_active_connections() ->
    %% Get active connection count
    {ok, Connections} = file:read_file("/proc/net/tcp"),
    ActiveCount = lists:fold(fun(Line, Acc) ->
        case re:run(Line, "01", []) of
            {match, _} -> Acc + 1;
            nomatch -> Acc
        end
    end, 0, string:split(Connections, "\n", trim)),
    ActiveCount.

get_disk_read_bytes() ->
    ##% Get disk read bytes
    {ok, DiskStats} = file:read_file("/proc/diskstats"),
    case re:run(DiskStats, "sda\\s+\\d+\\s+\\d+\\s+\\d+\\s+(\\d+)", [{capture, all_but_first, list}]) of
        {match, [ReadStr]} -> list_to_integer(ReadStr) * 512;  % sectors to bytes
        _ -> 0
    end.

get_disk_write_bytes() ->
    %% Get disk write bytes
    {ok, DiskStats} = file:read_file("/proc/diskstats"),
    case re:run(DiskStats, "sda\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+(\\d+)", [{capture, all_but_first, list}]) of
        {match, [WriteStr]} -> list_to_integer(WriteStr) * 512;  % sectors to bytes
        _ -> 0
    end.

get_disk_read_ops() ->
    %% Get disk read operations
    {ok, DiskStats} = file:read_file("/proc/diskstats"),
    case re:run(DiskStats, "sda\\s+\\d+\\s+\\d+\\s+(\\d+)", [{capture, all_but_first, list}]) of
        {match, [ReadOpsStr]} -> list_to_integer(ReadOpsStr);
        _ -> 0
    end.

get_disk_write_ops() ->
    %% Get disk write operations
    {ok, DiskStats} = file:read_file("/proc/diskstats"),
    case re:run(DiskStats, "sda\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+(\\d+)", [{capture, all_but_first, list}]) of
        {match, [WriteOpsStr]} -> list_to_integer(WriteOpsStr);
        _ -> 0
    end.

get_disk_usage() ->
    %% Get disk usage percentage
    {ok, DFOutput} = os:cmd("df -h /"),
    case re:run(DFOutput, "(/dev/sda\\d+).*?\\s+(\\d+)%", [{capture, all_but_first, list}]) of
        {match, [_, UsageStr]} -> list_to_integer(UsageStr);
        _ -> 0
    end.

get_disk_inodes() ->
    %% Get disk inode usage
    {ok, DFOutput} = os:cmd("df -i /"),
    case re:run(DFOutput, "(/dev/sda\\d+).*?\\s+(\\d+)%", [{capture, all_but_first, list}]) of
        {match, [_, UsageStr]} -> list_to_integer(UsageStr);
        _ -> 0
    end.

get_process_count() ->
    %% Get total process count
    length(erlang:processes()).

get_erlmcp_process_count() ->
    %% Get erlmcp process count
    length([P || P <- erlang:processes(), erlang:node(P) =:= node()]).

get_process_memory() ->
    %% Get total process memory
    Processes = erlang:processes(),
    list:fold(fun(Pid, Acc) ->
        case process_info(Pid, memory) of
            {memory, Size} -> Acc + Size;
            undefined -> Acc
        end
    end, 0, Processes).

get_process_threads() ->
    %% Get total thread count
    erlang:system_info(process_count).

get_application_memory() ->
    %% Get application-specific memory
    Processes = erlang:processes(),
    list:fold(fun(Pid, Acc) ->
        case process_info(Pid, registered_name) of
            {registered_name, erlmcp} -> Acc + get_process_memory(Pid);
            _ -> Acc
        end
    end, 0, Processes).

get_application_cpu() ->
    %% Get application-specific CPU usage
    0.0.

get_response_time() ->
    %% Get application response time
    {ok, Metrics} = erlmcp_metrics:get(),
    maps:get(response_time, Metrics, 0.0).

get_throughput() ->
    %% Get application throughput
    {ok, Metrics} = erlmcp_metrics:get(),
    maps:get(throughput, Metrics, 0.0).

get_error_rate() ->
    %% Get application error rate
    {ok, Metrics} = erlmcp_metrics:get(),
    maps:get(error_rate, Metrics, 0.0).

update_trend_buffer(MetricsHistory, TrendBuffer) ->
    %% Update trend buffer with new metrics
    case MetricsHistory of
        [] -> {MetricsHistory, TrendBuffer};
        [Latest | _] ->
            UpdatedBuffer = lists:fold(fun({Type, Values}, Acc) ->
                case maps:get(Type, Acc) of
                    Buffer when length(Buffer) >= 60 ->
                        NewBuffer = lists:sublist([Values | Buffer], 60);
                    Buffer ->
                        NewBuffer = [Values | Buffer]
                end,
                Acc#{Type => NewBuffer}
            end, TrendBuffer, maps:to_list(trend_values(Latest))),
            {MetricsHistory, UpdatedBuffer}
    end.

trend_values(Metric) ->
    %% Extract trend values from metric
    #{
        cpu => Metric#resource_metric.cpu#cpu.total_usage,
        memory => Metric#resource_metric.memory#memory.usage,
        network => Metric#resource_metric.network#network.bytes_out,
        disk => Metric#resource_metric.disk#disk.usage,
        process => Metric#resource_metric.process#process.count,
        application => Metric#resource_metric.application#application.response_time
    }.

identify_resource_bottlenecks(State) ->
    %% Identify resource bottlenecks from metrics
    LatestMetrics = case State#state.metrics_history of
        [] -> collect_current_metrics();
        [Latest | _] -> Latest
    end,

    Thresholds = State#state.thresholds,

    Bottlenecks = [],

    %% Check CPU bottlenecks
    CPUUsage = LatestMetrics#resource_metric.cpu#cpu.total_usage,
    case CPUUsage >= Thresholds#resource_threshold.cpu#cpu.critical of
        true ->
            Bottleneck = create_bottleneck(cpu, critical, CPUUsage,
                                         Thresholds#resource_threshold.cpu#cpu.critical, LatestMetrics),
            [Bottleneck | Bottlenecks];
        false ->
            case CPUUsage >= Thresholds#resource_threshold.cpu#cpu.warning of
                true ->
                    Bottleneck = create_bottleneck(cpu, high, CPUUsage,
                                                 Thresholds#resource_threshold.cpu#cpu.warning, LatestMetrics),
                    [Bottleneck | Bottlenecks];
                false -> Bottlenecks
            end
    end,

    %% Check memory bottlenecks
    MemoryUsage = LatestMetrics#resource_metric.memory#memory.usage,
    case MemoryUsage >= Thresholds#resource_threshold.memory#memory.critical of
        true ->
            Bottleneck = create_bottleneck(memory, critical, MemoryUsage,
                                         Thresholds#resource_threshold.memory#memory.critical, LatestMetrics),
            [Bottleneck | Bottlenecks];
        false ->
            case MemoryUsage >= Thresholds#resource_threshold.memory#memory.warning of
                true ->
                    Bottleneck = create_bottleneck(memory, high, MemoryUsage,
                                                 Thresholds#resource_threshold.memory#memory.warning, LatestMetrics),
                    [Bottleneck | Bottlenecks];
                false -> Bottlenecks
            end
    end,

    %% Check network bottlenecks
    NetworkBytes = LatestMetrics#resource_metric.network#network.bytes_out,
    case NetworkBytes >= Thresholds#resource_threshold.network#network.critical of
        true ->
            Bottleneck = create_bottleneck(network, critical, NetworkBytes,
                                         Thresholds#resource_threshold.network#network.critical, LatestMetrics),
            [Bottleneck | Bottlenecks];
        false ->
            case NetworkBytes >= Thresholds#resource_threshold.network#network.warning of
                true ->
                    Bottleneck = create_bottleneck(network, high, NetworkBytes,
                                                 Thresholds#resource_threshold.network#network.warning, LatestMetrics),
                    [Bottleneck | Bottlenecks];
                false -> Bottlenecks
            end
    end,

    Bottlenecks.

create_bottleneck(Type, Severity, CurrentUsage, Threshold, Metrics) ->
    %% Create bottleneck record
    Trend = determine_trend(Type, State#state.trend_buffer),

    #{
        type => Type,
        severity => Severity,
        current_usage => CurrentUsage,
        threshold => Threshold,
        impact => calculate_bottleneck_impact(Type, CurrentUsage, Threshold, Metrics),
        duration => measure_bottleneck_duration(Type, State),
        trend => Trend
    }.

determine_trend(Type, TrendBuffer) ->
    %% Determine trend for resource type
    Values = maps:get(Type, TrendBuffer, []),
    case Values of
        [] -> stable;
        _ ->
            Recent = lists:sublist(Values, min(10, length(Values))),
            First = lists:nth(1, Recent),
            Last = lists:last(Recent),
            if Last > First * 1.1 -> increasing;
            Last < First * 0.9 -> decreasing;
            true -> stable
            end
    end.

calculate_bottleneck_impact(Type, CurrentUsage, Threshold, Metrics) ->
    %% Calculate impact of bottleneck
    ImpactPercentage = CurrentUsage / Threshold,

    case Type of
        cpu ->
            #{
                cpu_overload => ImpactPercentage - 1.0,
                performance_degradation => min(1.0, ImpactPercentage * 0.5),
                thermal_throttling => CurrentUsage > 90.0
            };
        memory ->
            #{
                memory_pressure => ImpactPercentage - 1.0,
                swapping => CurrentUsage > 85.0,
                oom_risk => CurrentUsage > 95.0
            };
        network ->
            #{
                bandwidth_saturation => ImpactPercentage - 1.0,
                packet_loss => min(0.1, (CurrentUsage - Threshold) / Threshold),
                latency_increase => min(100, (CurrentUsage - Threshold) div (Threshold div 10))
            };
        _ ->
            #{severity => ImpactPercentage - 1.0}
    end.

measure_bottleneck_duration(Type, State) ->
    %% Measure how long bottleneck has persisted
    CurrentTime = erlang:system_time(millisecond),
    StartTime = find_bottleneck_start_time(Type, State#state.metrics_history),
    case StartTime of
        undefined -> 0;
        _ -> CurrentTime - StartTime
    end.

find_bottleneck_start_time(Type, MetricsHistory) ->
    %% Find when bottleneck first occurred
    Threshold = State#state.thresholds,
    ThresholdValue = case Type of
        cpu -> Threshold#resource_threshold.cpu#cpu.warning;
        memory -> Threshold#resource_threshold.memory#memory.warning;
        network -> Threshold#resource_threshold.network#network.warning;
        _ -> 0
    end,

    lists:foldl(fun(Metric, StartTime) ->
        case maps:get(Type, Metric#resource_metric) >= ThresholdValue of
            true -> Metric#resource_metric.timestamp;
            false -> StartTime
        end
    end, undefined, MetricsHistory).

identify_bottlenecks_from_analysis(Analysis, State) ->
    %% Identify bottlenecks from analysis
    Bottlenecks = [],

    %% Add bottlenecks from analysis
    case Analysis#analysis.cpu_bottleneck of
        true ->
            Bottleneck = #{
                type => cpu,
                severity => high,
                current_usage => Analysis#analysis.cpu_usage,
                threshold => State#state.thresholds#resource_threshold.cpu#cpu.warning,
                impact => Analysis#analysis.cpu_impact,
                duration => 0,
                trend => increasing
            },
            [Bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    Bottlenecks.

analyze_resource_metrics(MetricsHistory) ->
    %% Analyze resource metrics
    case MetricsHistory of
        [] -> #analysis{};
        _ ->
            Latest = lists:last(MetricsHistory),
            History = lists:sublist(MetricsHistory, min(100, length(MetricsHistory))),

            Analysis = #analysis{
                cpu_usage = Latest#resource_metric.cpu#cpu.total_usage,
                memory_usage = Latest#resource_metric.memory#memory.usage,
                network_usage = Latest#resource_metric.network#network.bytes_out,
                disk_usage = Latest#resource_metric.disk#disk.usage,
                cpu_bottleneck = is_cpu_bottleneck(Latest, State#state.thresholds),
                memory_bottleneck = is_memory_bottleneck(Latest, State#state.thresholds),
                network_bottleneck = is_network_bottleneck(Latest, State#state.thresholds),
                cpu_impact = calculate_cpu_impact(Latest, History),
                memory_impact = calculate_memory_impact(Latest, History),
                network_impact = calculate_network_impact(Latest, History)
            },

            Analysis
    end.

is_cpu_bottleneck(Metrics, Thresholds) ->
    Metrics#resource_metric.cpu#cpu.total_usage >= Thresholds#resource_threshold.cpu#cpu.warning.

is_memory_bottleneck(Metrics, Thresholds) ->
    Metrics#resource_metric.memory#memory.usage >= Thresholds#resource_threshold.memory#memory.warning.

is_network_bottleneck(Metrics, Thresholds) ->
    Metrics#resource_metric.network#network.bytes_out >= Thresholds#resource_threshold.network#network.warning.

calculate_cpu_impact(Latest, History) ->
    %% Calculate CPU impact
    RecentCPU = [M#resource_metric.cpu#cpu.total_usage || M <- History],
    if RecentCPU =/= [] ->
        AverageCPU = lists:sum(RecentCPU) / length(RecentCPU),
        CurrentCPU = Latest#resource_metric.cpu#cpu.total_usage,
        {impact => (CurrentCPU - AverageCPU) / AverageCPU};
    true -> #{impact => 0.0}
    end.

calculate_memory_impact(Latest, History) ->
    %% Calculate memory impact
    RecentMemory = [M#resource_metric.memory#memory.usage || M <- History],
    if RecentMemory =/= [] ->
        AverageMemory = lists:sum(RecentMemory) / length(RecentMemory),
        CurrentMemory = Latest#resource_metric.memory#memory.usage,
        {impact => (CurrentMemory - AverageMemory) / AverageMemory};
    true -> #{impact => 0.0}
    end.

calculate_network_impact(Latest, History) ->
    %% Calculate network impact
    RecentNetwork = [M#resource_metric.network#network.bytes_out || M <- History],
    if RecentNetwork =/= [] ->
        AverageNetwork = lists:sum(RecentNetwork) / length(RecentNetwork),
        CurrentNetwork = Latest#resource_metric.network#network.bytes_out,
        {impact => (CurrentNetwork - AverageNetwork) / AverageNetwork};
    true -> #{impact => 0.0}
    end.

generate_report(State) ->
    %% Generate comprehensive resource report
    LatestMetrics = case State#state.metrics_history of
        [] -> collect_current_metrics();
        [Latest | _] -> Latest
    end,

    Report = #{
        summary => #{
            monitoring_duration => erlang:system_time(millisecond) - State#state.start_time,
            metrics_collected => length(State#state.metrics_history),
            bottlenecks_detected => length(State#state.bottlenecks),
            resource_utilization => calculate_resource_utilization(LatestMetrics)
        },
        metrics => State#state.metrics_history,
        bottlenecks => State#state.bottlenecks,
        trends => calculate_trends(State#state.trend_buffer),
        recommendations => generate_recommendations(State#state.bottlenecks, LatestMetrics),
        timestamp => erlang:system_time(millisecond)
    },

    Report.

calculate_resource_utilization(Metrics) ->
    %% Calculate overall resource utilization
    #{
        cpu => Metrics#resource_metric.cpu#cpu.total_usage,
        memory => Metrics#resource_metric.memory#memory.usage,
        network => Metrics#resource_metric.network#network.bytes_out / (100 * 1024 * 1024) * 100,
        disk => Metrics#resource_metric.disk#disk.usage,
        process => Metrics#resource_metric.process#process.count / 1000 * 100
    }.

calculate_trends(TrendBuffer) ->
    %% Calculate trends for each resource type
    maps:fold(fun(Type, Values, Acc) ->
        Trend = determine_trend_from_values(Values),
        Acc#{Type => Trend}
    end, #{}, TrendBuffer).

determine_trend_from_values(Values) ->
    %% Determine trend from values
    case Values of
        [] -> unknown;
        _ ->
            First = lists:nth(1, Values),
            Last = lists:last(Values),
            Change = Last - First,
            if Change > 5 -> increasing;
            Change < -5 -> decreasing;
            true -> stable
            end
    end.

generate_recommendations(Bottlenecks, Metrics) ->
    ##% Generate recommendations based on bottlenecks
    Recommendations = [],

    %% Add CPU recommendations
    lists:foldl(fun(Bottleneck, Acc) ->
        case Bottleneck#bottleneck.type of
            cpu ->
                Reco = #{
                    type => cpu_optimization,
                    priority => Bottleneck#bottleneck.severity,
                    action => optimize_cpu_action(Bottleneck, Metrics),
                    impact => high
                },
                [Reco | Acc];
            _ -> Acc
        end
    end, Recommendations, Bottlenecks),

    Recommendations.

optimize_cpu_action(Bottleneck, Metrics) ->
    %% Generate CPU optimization action
    case Bottleneck#bottleneck.severity of
        critical -> "Emergency CPU scaling - vertical scaling required";
        high -> "Aggressive CPU optimization - process prioritization";
        medium -> "CPU optimization - check for inefficient processes";
        low -> "Monitor CPU usage - no immediate action required"
    end.

save_resource_report(Report) ->
    %% Save resource report
    ReportFile = "/Users/sac/erlmcp/load_test_reports/resource_report.json",
    ok = file:write_file(ReportFile, jsx:encode(Report)).