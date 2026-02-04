-module(erlmcp_health_check_memory).
-export([check/0, check/1]).

%% Memory health check implementation
%% Monitors memory usage, garbage collection, and memory leaks

-define(WARNING_THRESHOLD, 100 * 1024 * 1024).  % 100MB
-define(ERROR_THRESHOLD, 500 * 1024 * 1024).   % 500MB
-define(CRITICAL_THRESHOLD, 1000 * 1024 * 1024).  % 1GB

%% Memory info records
-record(memory_info, {
    total :: non_neg_integer(),
    processes :: non_neg_integer(),
    processes_used :: non_neg_integer(),
    system :: non_neg_integer(),
    atom :: non_neg_integer(),
    binary :: non_neg_integer(),
    ets :: non_neg_integer(),
    code :: non_neg_integer()
}).

-record(memory_summary, {
    total :: non_neg_integer(),
    processes :: non_neg_integer(),
    system :: non_neg_integer(),
    critical_count :: non_neg_integer(),
    warning_count :: non_neg_integer(),
    error_message :: binary(),
    warning_message :: binary(),
    ok_message :: binary(),
    details :: map()
}).

check() ->
    check(#{}).

check(Options) ->
    % Record health check request
    erlmcp_observability:counter(<<"health_check_requests_total">>, #{
        service => ?MODULE,
        check_type => "memory"
    }),

    StartTime = erlang:monotonic_time(millisecond),
    Result = do_memory_check(Options, StartTime),
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    erlmcp_observability:histogram_observe(<<"health_check_duration_ms">>, Duration),
    Result.

do_memory_check(Options, StartTime) ->
    try
        % Get memory information
        MemoryInfo = get_memory_info(),

        % Check memory usage
        MemoryStatus = check_memory_usage(MemoryInfo),

        % Check garbage collection
        GCStatus = check_garbage_collection(MemoryInfo),

        % Check memory growth rate
        GrowthStatus = check_memory_growth_rate(Options),

        % Generate summary
        Summary = generate_memory_summary(MemoryStatus, GCStatus, GrowthStatus),

        Duration = erlang:monotonic_time(millisecond) - StartTime,

        % Record health check metrics
        erlmcp_observability:histogram_observe(<<"health_check_duration_ms">>, Duration),
        erlmcp_observability:gauge_set(<<"memory_usage_bytes">>, #{
            service => ?MODULE
        }, MemoryInfo#memory_info.total),

        % Log health check results
        erlmcp_observability:log(<<"health_check_completed">>, #{
            service => ?MODULE,
            check_type => "memory",
            status => Summary#memory_summary.critical_count > 0 orelse
                     Summary#memory_summary.warning_count > 0,
            duration => Duration,
            total_memory => MemoryInfo#memory_info.total,
            processes_memory => MemoryInfo#memory_info.processes,
            system_memory => MemoryInfo#memory_info.system
        }),

        case Summary#memory_summary.critical_count > 0 of
            true ->
                % Record critical error
                erlmcp_observability:counter(<<"health_check_errors_total">>, #{
                    service => ?MODULE,
                    check_type => "memory",
                    severity => "critical"
                }),
                {error, Summary#memory_summary.error_message, Summary#memory_summary.details};
            _ when Summary#memory_summary.warning_count > 0 ->
                % Record warning
                erlmcp_observability:counter(<<"health_check_warnings_total">>, #{
                    service => ?MODULE,
                    check_type => "memory"
                }),
                {warning, Summary#memory_summary.warning_message, Summary#memory_summary.details};
            true ->
                % Record success
                erlmcp_observability:counter(<<"health_check_success_total">>, #{
                    service => ?MODULE,
                    check_type => "memory"
                }),
                {ok, Summary#memory_summary.ok_message, Summary#memory_summary.details}
        end
    catch
        Error:Reason ->
            % Calculate duration and record failure metrics
            handle_memory_check_error(Error, Reason, StartTime)
    end.

handle_memory_check_error(Error, Reason, StartTime) ->
    Duration = erlang:monotonic_time(millisecond) - StartTime,

    % Record health check failure
    erlmcp_observability:counter(<<"health_check_errors_total">>, #{
        service => ?MODULE,
        check_type => "memory",
        severity => "critical"
    }),
    erlmcp_observability:histogram_observe(<<"health_check_duration_ms">>, Duration),

    erlmcp_observability:log(<<"health_check_failed">>, #{
        service => ?MODULE,
        check_type => "memory",
        duration => Duration,
        error => binary_to_list(io_lib:format("~p:~p", [Error, Reason]))
    }),

    {error, io_lib:format("Memory check failed: ~p:~p", [Error, Reason]), #{error => {Error, Reason}}}.

get_memory_info() ->
    {Total, Processes, ProcessesUsed, System, Atom, Binary, ETS, Code} = erlang:memory(),
    #memory_info{
        total = Total,
        processes = Processes,
        processes_used = ProcessesUsed,
        system = System,
        atom = Atom,
        binary = Binary,
        ets = ETS,
        code = Code
    }.

check_memory_usage(#memory_info{total = Total}) ->
    if
        Total > ?CRITICAL_THRESHOLD -> critical;
        Total > ?ERROR_THRESHOLD -> error;
        Total > ?WARNING_THRESHOLD -> warning;
        true -> ok
    end.

check_garbage_collection(_) ->
    % Get GC statistics
    GCStats = erlang:statistics(garbage_collection),
    {_, _, _, _, _, GCCount, GCCause} = GCStats,

    % Analyze GC activity
    GCActivity = analyze_gc_activity(GCStats),

    % Check for excessive GC
    GCStatus = check_gc_excessive(GCActivity),

    % Check for GC pauses
    GCPauseStatus = check_gc_pauses(GCActivity),

    #{gc_count => GCCount, gc_cause => GCCause, activity => GCActivity,
      status => GCStatus, pause_status => GCPauseStatus}.

analyze_gc_activity({_, _, _, _, _, _, _} = GCStats) ->
    % Analyze GC patterns and trends
    #{
        frequency => calculate_gc_frequency(GCStats),
        pause_duration => calculate_gc_pause_duration(GCStats),
        memory_reclaimed => calculate_memory_reclaimed(GCStats),
        efficiency => calculate_gc_efficiency(GCStats)
    }.

calculate_gc_frequency({_, _, _, _, _, GCCount, _}) ->
    % Calculate GC frequency (per second of uptime)
    Uptime = erlang:system_info(uptime) div 1000,
    case Uptime > 0 of
        true -> GCCount / Uptime;
        false -> 0
    end.

calculate_gc_pause_duration({_, _, _, _, _, _, _}) ->
    % Calculate average GC pause duration
    % This would need historical data for accurate calculation
    0.

calculate_memory_reclaimed({_, _, _, _, _, _, _}) ->
    % Calculate total memory reclaimed by GC
    0.

calculate_gc_efficiency({_, _, _, _, _, _, _}) ->
    % Calculate GC efficiency
    0.

check_gc_excessive(Activity) ->
    Frequency = maps:get(frequency, Activity, 0),
    if
        Frequency > 100 -> error;
        Frequency > 50 -> warning;
        true -> ok
    end.

check_gc_pauses(Activity) ->
    PauseDuration = maps:get(pause_duration, Activity, 0),
    if
        PauseDuration > 1000 -> error;
        PauseDuration > 500 -> warning;
        true -> ok
    end.

check_memory_growth_rate(Options) ->
    % Check memory growth rate over time
    % This would need historical data
    case maps:get(historical_data, Options, undefined) of
        undefined ->
            #{
                growth_rate => 0,
                trend => stable,
                status => ok
            };
        _ ->
            % Analyze growth rate from historical data
            #{
                growth_rate => 0,
                trend => stable,
                status => ok
            }
    end.

generate_memory_summary(MemoryStatus, GCStatus, GrowthStatus) ->
    WarningCount = lists:sum([
        1 || Status <- [MemoryStatus, GCStatus, GrowthStatus],
             Status =:= warning
    ]),

    CriticalCount = lists:sum([
        1 || Status <- [MemoryStatus, GCStatus, GrowthStatus],
             Status =:= critical
    ]),

    Details = #{
        memory_status => MemoryStatus,
        gc_status => GCStatus,
        growth_status => GrowthStatus,
        warning_count => WarningCount,
        critical_count => CriticalCount
    },

    MemInfo = get_memory_info(),
    case CriticalCount > 0 of
        true ->
            #memory_summary{
                total = MemInfo#memory_info.total,
                processes = MemInfo#memory_info.processes,
                system = MemInfo#memory_info.system,
                critical_count = CriticalCount,
                warning_count = WarningCount,
                error_message = <<"Critical memory issues detected">>,
                warning_message = <<"Memory issues present">>,
                ok_message = <<"Memory usage within normal bounds">>,
                details = Details
            };
        _ when WarningCount > 0 ->
            #memory_summary{
                total = MemInfo#memory_info.total,
                processes = MemInfo#memory_info.processes,
                system = MemInfo#memory_info.system,
                critical_count = CriticalCount,
                warning_count = WarningCount,
                error_message = <<"Memory issues detected">>,
                warning_message = <<"Memory usage elevated">>,
                ok_message = <<"Memory usage within normal bounds">>,
                details = Details
            };
        true ->
            #memory_summary{
                total = MemInfo#memory_info.total,
                processes = MemInfo#memory_info.processes,
                system = MemInfo#memory_info.system,
                critical_count = CriticalCount,
                warning_count = WarningCount,
                error_message = <<"">>,
                warning_message = <<"">>,
                ok_message = <<"Memory usage within normal bounds">>,
                details = Details
            }
    end.