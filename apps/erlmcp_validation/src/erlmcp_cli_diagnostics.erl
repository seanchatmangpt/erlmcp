%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Diagnostics Module
%%%
%%% Provides system diagnostic commands for erlmcp CLI:
%%% - System health check (diagnose)
%%% - Memory usage analysis
%%% - Process monitoring
%%% - System metrics collection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_diagnostics).

-export([
    diagnose/0,
    diagnose/1,
    memory_usage/0,
    memory_usage/1,
    system_info/0,
    process_info/0,
    process_info/1,
    check_health/0,
    check_health/1
]).

%%====================================================================
%% Types
%%====================================================================

-type diagnostic_opts() :: #{
    verbose => 1..5,
    format => atom(),
    include_processes => boolean(),
    threshold => non_neg_integer()
}.

%%====================================================================
%% API Functions - Diagnostics
%%====================================================================

%% @doc Run full system diagnostics
-spec diagnose() -> {ok, map()} | {error, term()}.
diagnose() ->
    diagnose(#{}).

%% @doc Run diagnostics with options
-spec diagnose(diagnostic_opts()) -> {ok, map()} | {error, term()}.
diagnose(Opts) ->
    try
        %% Collect all diagnostic information
        HealthStatus = check_health(Opts),
        SystemInfo = system_info(),
        MemoryInfo = memory_usage(Opts),
        ProcessInfo = case maps:get(include_processes, Opts, false) of
            true -> process_info(Opts);
            false -> #{total_processes => erlang:system_info(process_count)}
        end,

        %% Build comprehensive diagnostic report
        Diagnostics = #{
            timestamp => iso8601_timestamp(),
            status => determine_overall_status([HealthStatus, MemoryInfo]),
            health => HealthStatus,
            system => SystemInfo,
            memory => MemoryInfo,
            processes => ProcessInfo,
            recommendations => generate_recommendations(HealthStatus, MemoryInfo, ProcessInfo)
        },

        {ok, Diagnostics}
    catch
        Class:Error:Stack ->
            {error, #{
                class => Class,
                error => Error,
                stacktrace => Stack
            }}
    end.

%% @doc Get memory usage information
-spec memory_usage() -> map().
memory_usage() ->
    memory_usage(#{}).

%% @doc Get memory usage with options
-spec memory_usage(diagnostic_opts()) -> map().
memory_usage(Opts) ->
    Verbose = maps:get(verbose, Opts, 3),

    %% Get basic memory info
    MemoryTotal = erlang:memory(total),
    MemoryProcesses = erlang:memory(processes),
    MemoryProcessesUsed = erlang:memory(processes_used),
    MemorySystem = erlang:memory(system),
    MemoryAtom = erlang:memory(atom),
    MemoryAtomUsed = erlang:memory(atom_used),
    MemoryBinary = erlang:memory(binary),
    MemoryCode = erlang:memory(code),
    MemoryEts = erlang:memory(ets),

    BaseInfo = #{
        total_bytes => MemoryTotal,
        total_mb => bytes_to_mb(MemoryTotal),
        processes_bytes => MemoryProcesses,
        processes_used_bytes => MemoryProcessesUsed,
        processes_mb => bytes_to_mb(MemoryProcesses),
        system_bytes => MemorySystem,
        system_mb => bytes_to_mb(MemorySystem),
        binary_bytes => MemoryBinary,
        binary_mb => bytes_to_mb(MemoryBinary),
        atom_bytes => MemoryAtom,
        atom_used_bytes => MemoryAtomUsed,
        code_bytes => MemoryCode,
        code_mb => bytes_to_mb(MemoryCode),
        ets_bytes => MemoryEts,
        ets_mb => bytes_to_mb(MemoryEts),
        usage_percentage => calculate_memory_usage_percentage()
    },

    %% Add detailed info based on verbosity
    case Verbose >= 4 of
        true ->
            BaseInfo#{
                detailed_breakdown => erlang:memory(),
                top_memory_processes => get_top_memory_processes(10),
                large_binaries => get_large_binaries(10)
            };
        false ->
            BaseInfo
    end.

%% @doc Get system information
-spec system_info() -> map().
system_info() ->
    #{
        otp_release => erlang:system_info(otp_release),
        erts_version => erlang:system_info(version),
        system_architecture => erlang:system_info(system_architecture),
        os_type => os:type(),
        os_version => os:version(),
        schedulers_online => erlang:system_info(schedulers_online),
        schedulers_total => erlang:system_info(schedulers),
        process_limit => erlang:system_info(process_limit),
        process_count => erlang:system_info(process_count),
        port_limit => erlang:system_info(port_limit),
        port_count => erlang:system_info(port_count),
        atom_limit => erlang:system_info(atom_limit),
        atom_count => erlang:system_info(atom_count),
        ets_limit => erlang:system_info(ets_limit),
        ets_count => length(ets:all()),
        uptime_seconds => element(1, erlang:statistics(wall_clock)) div 1000
    }.

%% @doc Get process information
-spec process_info() -> map().
process_info() ->
    process_info(#{}).

%% @doc Get process information with options
-spec process_info(diagnostic_opts()) -> map().
process_info(Opts) ->
    Threshold = maps:get(threshold, Opts, 100),

    %% Get total process count
    TotalProcesses = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),

    %% Find overloaded processes
    {ok, OverloadedProcs} = case whereis(erlmcp_health_monitor) of
        undefined ->
            {ok, []};
        _Pid ->
            erlmcp_health_monitor:find_overloaded_processes(Threshold)
    end,

    %% Get top processes by various metrics
    TopByMemory = get_top_memory_processes(10),
    TopByReductions = get_top_reduction_processes(10),
    TopByMessageQueue = get_top_message_queue_processes(10),

    #{
        total_processes => TotalProcesses,
        process_limit => ProcessLimit,
        usage_percentage => (TotalProcesses / ProcessLimit) * 100,
        overloaded_count => length(OverloadedProcs),
        overloaded_processes => OverloadedProcs,
        top_by_memory => TopByMemory,
        top_by_reductions => TopByReductions,
        top_by_message_queue => TopByMessageQueue
    }.

%% @doc Check system health
-spec check_health() -> map().
check_health() ->
    check_health(#{}).

%% @doc Check system health with options
-spec check_health(diagnostic_opts()) -> map().
check_health(_Opts) ->
    %% Check if health monitor is running
    HealthMonitorStatus = case whereis(erlmcp_health_monitor) of
        undefined -> not_running;
        _Pid -> running
    end,

    %% Get system health if monitor is running
    SystemHealth = case HealthMonitorStatus of
        running ->
            try
                erlmcp_health_monitor:get_system_health()
            catch
                _:_ -> #{system => unknown, components => #{}}
            end;
        not_running ->
            #{system => unknown, components => #{}}
    end,

    %% Check memory health
    MemoryUsage = calculate_memory_usage_percentage(),
    MemoryStatus = if
        MemoryUsage > 95 -> critical;
        MemoryUsage > 85 -> warning;
        true -> ok
    end,

    %% Check process health
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    ProcessUsage = (ProcessCount / ProcessLimit) * 100,
    ProcessStatus = if
        ProcessUsage > 90 -> critical;
        ProcessUsage > 80 -> warning;
        true -> ok
    end,

    %% Check scheduler utilization
    SchedulerUtil = get_scheduler_utilization(),

    %% Determine overall status
    OverallStatus = determine_status([MemoryStatus, ProcessStatus]),

    #{
        overall_status => OverallStatus,
        health_monitor => HealthMonitorStatus,
        system_health => SystemHealth,
        memory => #{
            usage_percentage => MemoryUsage,
            status => MemoryStatus
        },
        processes => #{
            count => ProcessCount,
            limit => ProcessLimit,
            usage_percentage => ProcessUsage,
            status => ProcessStatus
        },
        scheduler_utilization => SchedulerUtil
    }.

%%====================================================================
%% Internal Functions - Metrics Collection
%%====================================================================

%% @private Get top processes by memory usage
-spec get_top_memory_processes(pos_integer()) -> [map()].
get_top_memory_processes(N) ->
    Processes = erlang:processes(),
    ProcessesWithMem = lists:filtermap(fun(Pid) ->
        case process_info(Pid, [memory, registered_name, current_function]) of
            undefined -> false;
            Info ->
                Memory = proplists:get_value(memory, Info, 0),
                RegName = proplists:get_value(registered_name, Info, undefined),
                CurrentFun = proplists:get_value(current_function, Info, undefined),
                {true, #{
                    pid => pid_to_list(Pid),
                    memory_bytes => Memory,
                    memory_mb => bytes_to_mb(Memory),
                    registered_name => RegName,
                    current_function => CurrentFun
                }}
        end
    end, Processes),

    Sorted = lists:sort(fun(#{memory_bytes := M1}, #{memory_bytes := M2}) ->
        M1 > M2
    end, ProcessesWithMem),

    lists:sublist(Sorted, N).

%% @private Get top processes by reductions
-spec get_top_reduction_processes(pos_integer()) -> [map()].
get_top_reduction_processes(N) ->
    Processes = erlang:processes(),
    ProcessesWithReds = lists:filtermap(fun(Pid) ->
        case process_info(Pid, [reductions, registered_name]) of
            undefined -> false;
            Info ->
                Reds = proplists:get_value(reductions, Info, 0),
                RegName = proplists:get_value(registered_name, Info, undefined),
                {true, #{
                    pid => pid_to_list(Pid),
                    reductions => Reds,
                    registered_name => RegName
                }}
        end
    end, Processes),

    Sorted = lists:sort(fun(#{reductions := R1}, #{reductions := R2}) ->
        R1 > R2
    end, ProcessesWithReds),

    lists:sublist(Sorted, N).

%% @private Get top processes by message queue length
-spec get_top_message_queue_processes(pos_integer()) -> [map()].
get_top_message_queue_processes(N) ->
    Processes = erlang:processes(),
    ProcessesWithQueue = lists:filtermap(fun(Pid) ->
        case process_info(Pid, [message_queue_len, registered_name]) of
            undefined -> false;
            Info ->
                QLen = proplists:get_value(message_queue_len, Info, 0),
                RegName = proplists:get_value(registered_name, Info, undefined),
                case QLen > 0 of
                    true ->
                        {true, #{
                            pid => pid_to_list(Pid),
                            message_queue_len => QLen,
                            registered_name => RegName
                        }};
                    false ->
                        false
                end
        end
    end, Processes),

    Sorted = lists:sort(fun(#{message_queue_len := Q1}, #{message_queue_len := Q2}) ->
        Q1 > Q2
    end, ProcessesWithQueue),

    lists:sublist(Sorted, N).

%% @private Get large binaries in the system
-spec get_large_binaries(pos_integer()) -> [map()].
get_large_binaries(N) ->
    Processes = erlang:processes(),
    AllBinaries = lists:flatmap(fun(Pid) ->
        case process_info(Pid, [binary, registered_name]) of
            undefined -> [];
            Info ->
                Binaries = proplists:get_value(binary, Info, []),
                RegName = proplists:get_value(registered_name, Info, undefined),
                [{Pid, RegName, BinInfo} || BinInfo <- Binaries]
        end
    end, Processes),

    BinaryMaps = lists:map(fun({Pid, RegName, {_BinId, Size, RefCount}}) ->
        #{
            pid => pid_to_list(Pid),
            registered_name => RegName,
            size_bytes => Size,
            size_mb => bytes_to_mb(Size),
            ref_count => RefCount
        }
    end, AllBinaries),

    Sorted = lists:sort(fun(#{size_bytes := S1}, #{size_bytes := S2}) ->
        S1 > S2
    end, BinaryMaps),

    lists:sublist(Sorted, N).

%% @private Get scheduler utilization
-spec get_scheduler_utilization() -> map().
get_scheduler_utilization() ->
    try
        erlang:system_flag(scheduler_wall_time, true),
        timer:sleep(100),
        SchedulerTimes = erlang:statistics(scheduler_wall_time),

        TotalActive = lists:sum([Active || {_, Active, _} <- SchedulerTimes]),
        TotalTotal = lists:sum([Total || {_, _, Total} <- SchedulerTimes]),

        Utilization = case TotalTotal of
            0 -> 0.0;
            _ -> (TotalActive / TotalTotal) * 100
        end,

        #{
            utilization_percentage => Utilization,
            scheduler_count => length(SchedulerTimes),
            per_scheduler => [#{
                scheduler_id => Id,
                utilization_percentage => case Total of
                    0 -> 0.0;
                    _ -> (Active / Total) * 100
                end
            } || {Id, Active, Total} <- SchedulerTimes]
        }
    catch
        _:_ ->
            #{utilization_percentage => 0.0, scheduler_count => erlang:system_info(schedulers_online)}
    end.

%%====================================================================
%% Internal Functions - Analysis
%%====================================================================

%% @private Calculate memory usage percentage
-spec calculate_memory_usage_percentage() -> float().
calculate_memory_usage_percentage() ->
    MemoryTotal = erlang:memory(total),
    MemoryProcesses = erlang:memory(processes),
    (MemoryProcesses / MemoryTotal) * 100.

%% @private Determine overall status from multiple statuses
-spec determine_overall_status(list()) -> atom().
determine_overall_status(Statuses) ->
    HasCritical = lists:any(fun(S) ->
        maps:get(status, S, ok) =:= critical orelse S =:= critical
    end, Statuses),

    HasWarning = lists:any(fun(S) ->
        maps:get(status, S, ok) =:= warning orelse S =:= warning
    end, Statuses),

    if
        HasCritical -> critical;
        HasWarning -> warning;
        true -> ok
    end.

%% @private Determine status from list of status atoms
-spec determine_status([atom()]) -> atom().
determine_status(Statuses) ->
    case lists:member(critical, Statuses) of
        true -> critical;
        false ->
            case lists:member(warning, Statuses) of
                true -> warning;
                false -> ok
            end
    end.

%% @private Generate recommendations based on diagnostics
-spec generate_recommendations(map(), map(), map()) -> [binary()].
generate_recommendations(HealthStatus, MemoryInfo, ProcessInfo) ->
    Recommendations = [],

    %% Memory recommendations
    MemRecs = case maps:get(usage_percentage, MemoryInfo, 0) of
        Usage when Usage > 90 ->
            [<<"CRITICAL: Memory usage is very high. Consider reducing load or increasing memory.">>];
        Usage when Usage > 80 ->
            [<<"WARNING: Memory usage is high. Monitor closely and consider optimization.">>];
        _ -> []
    end,

    %% Process recommendations
    ProcRecs = case maps:get(usage_percentage, ProcessInfo, 0) of
        Usage when Usage > 85 ->
            [<<"CRITICAL: Process count is very high. Review process creation patterns.">>];
        Usage when Usage > 75 ->
            [<<"WARNING: Process count is high. Monitor process lifecycle.">>];
        _ -> []
    end,

    %% Health monitor recommendations
    HealthRecs = case maps:get(health_monitor, HealthStatus, not_running) of
        not_running ->
            [<<"INFO: Health monitor is not running. Consider starting it for better monitoring.">>];
        _ -> []
    end,

    lists:flatten([Recommendations, MemRecs, ProcRecs, HealthRecs]).

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Convert bytes to megabytes
-spec bytes_to_mb(non_neg_integer()) -> float().
bytes_to_mb(Bytes) ->
    Bytes / (1024 * 1024).

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec])).
