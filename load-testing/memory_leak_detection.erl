%% @doc Memory Leak Detection for erlmcp v3
%% Implements comprehensive memory leak detection and monitoring
%%
%% Features:
%%- Long-running memory monitoring
%%- Object tracking and pattern recognition
%%- Garbage collection analysis
%%- Memory growth rate calculation
%%- Leak detection thresholds
%%- Memory profiling integration
%%- Automated reporting

-module(erlmcp_memory_leak_detection).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_monitoring/2, stop_monitoring/1, get_memory_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.memory_stats, {
    timestamp :: integer(),
    process_memory :: integer(),
    ets_memory :: integer(),
    mnesia_memory :: integer(),
    total_memory :: integer(),
    gc_count :: integer(),
    gc_reclaimed :: integer(),
    heap_size :: integer(),
    stack_size :: integer(),
    reductions :: integer()
}.

-record.memory_profile, {
    pid :: pid(),
    initial_memory :: integer(),
    memory_history :: list(),
    leak_threshold :: float(),
    growth_rate :: float(),
    last_gc :: integer()
}.

-record.monitor_config, {
    duration :: integer(),
    sample_interval :: integer(),
    leak_threshold :: float(),
    growth_rate_threshold :: float(),
    profile_processes :: list(),
    alert_threshold :: integer(),
    reporting_interval :: integer()
}.

-record.monitor_state, {
    config :: #monitor_config{},
    start_time :: integer(),
    end_time :: integer(),
    memory_profiles :: map(),
    memory_history :: list(),
    alerts :: list(),
    gc_analysis :: map(),
    final_report :: map()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_monitoring(MonitorId, Config) ->
    gen_server:call(?MODULE, {start_monitoring, MonitorId, Config}, infinity).

stop_monitoring(MonitorId) ->
    gen_server:call(?MODULE, {stop_monitoring, MonitorId}).

get_memory_stats() ->
    gen_server:call(?MODULE, get_memory_stats).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Memory leak detection monitor initialized"),

    %% Initialize state
    State = #monitor_state{
        memory_profiles = maps:new(),
        memory_history = [],
        alerts = [],
        gc_analysis = maps:new(),
        final_report = maps:new()
    },

    %% Start periodic cleanup
    erlang:send_after(3600000, self(), cleanup_old_profiles), % 1 hour

    {ok, State}.

handle_call({start_monitoring, MonitorId, Config}, _From, State) ->
    %% Validate configuration
    case validate_monitor_config(Config) of
        {ok, ValidConfig} ->
            %% Start memory monitoring
            NewState = start_memory_monitoring(MonitorId, ValidConfig, State),
            {reply, {ok, monitoring_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop_monitoring, MonitorId}, _From, State) ->
    %% Stop memory monitoring
    NewState = stop_memory_monitoring(MonitorId, State),
    {reply, {ok, monitoring_stopped}, NewState};

handle_call(get_memory_stats, _From, State) ->
    %% Get current memory statistics
    Stats = get_current_memory_stats(),
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({memory_sample, MonitorId, Sample}, State) ->
    %% Process memory sample
    ?LOG_DEBUG("Memory sample for ~p: ~p", [MonitorId, Sample]),

    %% Update memory history
    UpdatedHistory = [Sample | State#monitor_state.memory_history],

    %% Check for memory leaks
    {MemoryProfiles, Alerts} = check_memory_leaks(MonitorId, Sample, State),

    %% Check for garbage collection patterns
    GCAnalysis = analyze_gc_patterns(MonitorId, Sample, State#monitor_state.gc_analysis);

    State#monitor_state{
        memory_profiles = MemoryProfiles,
        memory_history = UpdatedHistory,
        alerts = Alerts,
        gc_analysis = GCAnalysis
    };

handle_info({memory_alert, MonitorId, Alert}, State) ->
    %% Handle memory alert
    ?LOG_WARNING("Memory alert for ~p: ~p", [MonitorId, Alert]),

    NewAlerts = [Alert | State#monitor_state.alerts],

    State#monitor_state{
        alerts = NewAlerts
    };

handle_info({monitoring_complete, MonitorId, Report}, State) ->
    %% Handle monitoring completion
    ?LOG_INFO("Monitoring completed for ~p", [MonitorId]),

    FinalReport = generate_final_report(MonitorId, Report, State);

    State#monitor_state{
        final_report = FinalReport
    };

handle_info(cleanup_old_profiles, State) ->
    %% Clean up old memory profiles
    NewState = cleanup_old_profiles(State),

    %% Schedule next cleanup
    erlang:send_after(3600000, self(), cleanup_old_profiles),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup memory monitoring
    cleanup_memory_profiles(State),
    ?LOG_INFO("Memory leak detection monitor terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_monitor_config(Config) ->
    %% Validate monitoring configuration
    RequiredFields = [duration, sample_interval, leak_threshold, growth_rate_threshold],

    case lists:all(fun(Field) -> maps:is_key(Field, Config) end, RequiredFields) of
        true ->
            %% Additional validation
            case Config#duration > 0 andalso Config#sample_interval > 0 of
                true ->
                    {ok, Config};
                false ->
                    {error, invalid_interval}
            end;
        false ->
            {error, missing_required_fields}
    end.

start_memory_monitoring(MonitorId, Config, State) ->
    %% Start memory monitoring for specific processes
    ?LOG_INFO("Starting memory monitoring: ~p for ~p ms", [MonitorId, Config#duration]),

    %% Create memory profiles for target processes
    MemoryProfiles = create_memory_profiles(Config, State);

    %% Start monitoring loop
    StartSample = get_current_memory_stats();
    StartTime = erlang:system_time(millisecond);

    %% Schedule first sample
    erlang:send_after(Config#sample_interval, self(), take_memory_sample);

    State#monitor_state{
        config = Config,
        start_time = StartTime,
        memory_profiles = MemoryProfiles
    }.

stop_memory_monitoring(MonitorId, State) ->
    %% Stop memory monitoring
    ?LOG_INFO("Stopping memory monitoring: ~p", [MonitorId]),

    %% Generate final report
    Report = generate_monitoring_report(MonitorId, State);

    %% Send completion message
    erlang:send_after(0, self(), {monitoring_complete, MonitorId, Report});

    %% Remove memory profile
    UpdatedProfiles = maps:remove(MonitorId, State#monitor_state.memory_profiles),

    State#monitor_state{
        memory_profiles = UpdatedProfiles
    }.

create_memory_profiles(Config, State) ->
    %% Create memory profiles for target processes
    case Config#profile_processes of
        [] ->
            %% Monitor all processes
            AllPids = processes(),
            create_process_profiles(AllPids, Config, State);
        Pids ->
            %% Monitor specific processes
            create_process_profiles(Pids, Config, State)
    end.

create_process_profiles(Pids, Config, State) ->
    %% Create profiles for specific processes
    lists:foldl(fun(Pid, Acc) ->
        case is_process_alive(Pid) of
            true ->
                Profile = create_memory_profile(Pid, Config),
                maps:put(Pid, Profile, Acc);
            false ->
                Acc
        end
    end, maps:new(), Pids).

create_memory_profile(Pid, Config) ->
    %% Create memory profile for a process
    InitialSample = get_process_memory_stats(Pid),
    InitialMemory = InitialSample#memory_stats.total_memory;

    #memory_profile{
        pid = Pid,
        initial_memory = InitialMemory,
        memory_history = [InitialSample],
        leak_threshold = Config#leak_threshold,
        growth_rate_threshold = Config#growth_rate_threshold,
        last_gc = InitialSample#memory_stats.gc_count
    }.

take_memory_sample(State) ->
    %% Take memory sample for all monitored processes
    Now = erlang:system_time(millisecond);
    Sample = get_current_memory_stats();

    %% Process sample for each monitored process
    lists:foreach(fun({Pid, Profile}) ->
        ProcessSample = get_process_memory_stats(Pid),
        erlang:send_after(0, self(), {memory_sample, Pid, ProcessSample})
    end, maps:to_list(State#monitor_state.memory_profiles));

    %% Schedule next sample
    erlang:send_after(State#config#sample_interval, self(), take_memory_sample).

check_memory_leaks(MonitorId, Sample, State) ->
    %% Check for memory leaks
    MemoryProfiles = State#monitor_state.memory_profiles;
    Alerts = State#monitor_state.alerts;

    case maps:find(MonitorId, MemoryProfiles) of
        {ok, Profile} ->
            %% Calculate memory growth
            InitialMemory = Profile#memory_profile.initial_memory;
            CurrentMemory = Sample#memory_stats.total_memory;

            GrowthRate = calculate_growth_rate(InitialMemory, CurrentMemory,
                                               length(Profile#memory_profile.memory_history));

            %% Check for leak
            LeakDetected = check_for_leak(Profile, Sample, GrowthRate);

            %% Update profile
            UpdatedProfile = Profile#memory_profile{
                memory_history = [Sample | Profile#memory_profile.memory_history],
                growth_rate = GrowthRate
            };

            %% Handle leak detection
            case LeakDetected of
                true ->
                    Alert = create_memory_alert(MonitorId, Sample, GrowthRate),
                    NewAlerts = [Alert | Alerts];
                    {maps:put(MonitorId, UpdatedProfile, MemoryProfiles), NewAlerts};
                false ->
                    {maps:put(MonitorId, UpdatedProfile, MemoryProfiles), Alerts}
            end;
        error ->
            %% Process not found
            {MemoryProfiles, Alerts}
    end.

calculate_growth_rate(InitialMemory, CurrentMemory, SampleCount) ->
    %% Calculate memory growth rate
    if
        SampleCount < 2 ->
            0.0;
        InitialMemory =:= 0 ->
            0.0;
        true ->
            (CurrentMemory - InitialMemory) / (SampleCount * 1000) % Bytes per second
    end.

check_for_leak(Profile, Sample, GrowthRate) ->
    %% Check if memory leak detected
    Threshold = Profile#memory_profile.leak_threshold;
    GrowthThreshold = Profile#memory_profile.growth_rate_threshold;

    CurrentMemory = Sample#memory_stats.total_memory;
    InitialMemory = Profile#memory_profile.initial_memory;

    GrowthRate > GrowthThreshold andalso
    CurrentMemory > InitialMemory * 1.1. % 10% growth threshold

create_memory_alert(MonitorId, Sample, GrowthRate) ->
    %% Create memory alert
    #{
        timestamp => erlang:system_time(millisecond),
        monitor_id => MonitorId,
        alert_type => memory_leak,
        memory_stats => Sample,
        growth_rate => GrowthRate,
        severity => determine_alert_severity(GrowthRate),
        message => format_alert_message(MonitorId, GrowthRate)
    }.

determine_alert_severity(GrowthRate) ->
    %% Determine alert severity based on growth rate
    case GrowthRate of
        Rate when Rate > 1000000 -> % 1MB/s
            critical;
        Rate when Rate > 500000 -> % 500KB/s
            high;
        Rate when Rate > 100000 -> % 100KB/s
            medium;
        _ ->
            low
    end.

format_alert_message(MonitorId, GrowthRate) ->
    io_lib:format("Memory leak detected for ~p: ~.2f bytes/s growth rate", [MonitorId, GrowthRate]).

analyze_gc_patterns(MonitorId, Sample, GCAnalysis) ->
    %% Analyze garbage collection patterns
    CurrentGC = Sample#memory_stats.gc_count;
    LastGC = maps:get(MonitorId, GCAnalysis, 0);
    GCCountDelta = CurrentGC - LastGC;

    GCStats = #{
        timestamp => erlang:system_time(millisecond),
        gc_count => CurrentGC,
        gc_count_delta => GCCountDelta,
        gc_reclaimed => Sample#memory_stats.gc_reclaimed,
        memory_before => Sample#memory_stats.total_memory - Sample#memory_stats.gc_reclaimed,
        memory_after => Sample#memory_stats.total_memory
    };

    maps:put(MonitorId, GCStats, GCAnalysis).

get_current_memory_stats() ->
    %% Get current memory statistics
    #memory_stats{
        timestamp => erlang:system_time(millisecond),
        process_memory => process_info(self(), memory),
        ets_memory => get_ets_memory_usage(),
        mnesia_memory => get_mnesia_memory_usage(),
        total_memory => erlang:memory(total),
        gc_count => erlang:statistics(garbage_collection),
        gc_reclaimed => erlang:statistics(garbage_collection),
        heap_size => erlang:memory(heap_size),
        stack_size => erlang:memory(stack_size),
        reductions => erlang:statistics(reductions)
    }.

get_process_memory_stats(Pid) ->
    %% Get memory statistics for a specific process
    case is_process_alive(Pid) of
        true ->
            #memory_stats{
                timestamp => erlang:system_time(millisecond),
                process_memory => process_info(Pid, memory),
                ets_memory => get_process_ets_memory(Pid),
                mnesia_memory => get_process_mnesia_memory(Pid),
                total_memory => get_process_total_memory(Pid),
                gc_count => get_process_gc_count(Pid),
                gc_reclaimed => get_process_gc_reclaimed(Pid),
                heap_size => get_process_heap_size(Pid),
                stack_size => get_process_stack_size(Pid),
                reductions => get_process_reductions(Pid)
            };
        false ->
            #memory_stats{
                timestamp => erlang:system_time(millisecond),
                process_memory => 0,
                ets_memory => 0,
                mnesia_memory => 0,
                total_memory => 0,
                gc_count => 0,
                gc_reclaimed => 0,
                heap_size => 0,
                stack_size => 0,
                reductions => 0
            }
    end.

get_ets_memory_usage() ->
    %% Get total ETS memory usage
    ETSTables = ets:all(),
    lists:foldl(fun(Table, Acc) ->
        case ets:info(Table, memory) of
            Size when is_integer(Size) ->
                Acc + Size;
            undefined ->
                Acc
        end
    end, 0, ETSTables).

get_mnesia_memory_usage() ->
    %% Get Mnesia memory usage
    case mnesia:system_info({tables, memory}) of
        undefined ->
            0;
        Tables ->
            lists:sum([Size || {_, Size} <- Tables])
    end.

get_process_ets_memory(Pid) ->
    %% Get ETS memory for a process
    case process_info(Pid, [dictionary, registered_name]) of
        {dictionary, Dict} ->
            %% Check for ETS tables created by this process
            lists:foldl(fun({ets, Table}, Acc) ->
                case ets:info(Table, memory) of
                    Size when is_integer(Size) ->
                        Acc + Size;
                    undefined ->
                        Acc
                end
            end, 0, Dict);
        _ ->
            0
    end.

get_process_mnesia_memory(Pid) ->
    %% Get Mnesia memory for a process
    0 % Simplified implementation

get_process_total_memory(Pid) ->
    %% Get total memory for a process
    case process_info(Pid, memory) of
        {memory, Size} ->
            Size;
        undefined ->
            0
    end.

get_process_gc_count(Pid) ->
    %% Get garbage collection count for a process
    case process_info(Pid, garbage_collection) of
        {garbage_collection, GCInfo} ->
            proplists:get_value(gc_count, GCInfo, 0);
        undefined ->
            0
    end.

get_process_gc_reclaimed(Pid) ->
    %% Get garbage collection reclaimed bytes for a process
    case process_info(Pid, garbage_collection) of
        {garbage_collection, GCInfo} ->
            proplists:get_value(gc_reclaimed, GCInfo, 0);
        undefined ->
            0
    end.

get_process_heap_size(Pid) ->
    %% Get heap size for a process
    case process_info(Pid, heap_size) of
        {heap_size, Size} ->
            Size;
        undefined ->
            0
    end.

get_process_stack_size(Pid) ->
    %% Get stack size for a process
    case process_info(Pid, stack_size) of
        {stack_size, Size} ->
            Size;
        undefined ->
            0
    end.

get_process_reductions(Pid) ->
    %% Get reductions for a process
    case process_info(Pid, reductions) of
        {reductions, Count} ->
            Count;
        undefined ->
            0
    end.

generate_monitoring_report(MonitorId, State) ->
    %% Generate monitoring report
    MemoryProfiles = State#monitor_state.memory_profiles;
    Alerts = State#monitor_state.alerts;
    GCAnalysis = State#monitor_state.gc_analysis;

    Report = #{
        monitor_id => MonitorId,
        start_time => State#monitor_state.start_time,
        end_time => erlang:system_time(millisecond),
        duration => erlang:system_time(millisecond) - State#monitor_state.start_time,
        memory_profiles => maps:values(MemoryProfiles),
        alerts => Alerts,
        gc_analysis => maps:values(GCAnalysis),
        summary => generate_summary(State),
        recommendations => generate_recommendations(State)
    },

    %% Save report
    ReportFile = "/Users/sac/erlmcp/load-testing/memory_report_" ++
                 integer_to_list(erlang:system_time(millisecond)) ++ ".json",
    file:write_file(ReportFile, jsx:encode(Report)),

    ?LOG_INFO("Memory monitoring report saved to: ~p", [ReportFile]),

    Report.

generate_final_report(MonitorId, Report, State) ->
    %% Generate final monitoring report
    State#monitor_state{
        final_report = Report
    }.

generate_summary(State) ->
    %% Generate monitoring summary
    MemoryProfiles = State#monitor_state.memory_profiles;
    Alerts = State#monitor_state.alerts;

    #{
        total_monitored => maps:size(MemoryProfiles),
        memory_leaks_detected => length([P || P <- maps:values(MemoryProfiles),
                                            has_memory_leak(P)]),
        alerts_generated => length(Alerts),
        critical_alerts => length([A || A <- Alerts, A#severity =:= critical]),
        average_growth_rate => calculate_average_growth_rate(MemoryProfiles),
        total_gc_count => calculate_total_gc_count(State#monitor_state.gc_analysis)
    }.

has_memory_leak(Profile) ->
    %% Check if profile indicates memory leak
    GrowthRate = Profile#memory_profile.growth_rate;
    Threshold = Profile#memory_profile.growth_rate_threshold;

    GrowthRate > Threshold.

calculate_average_growth_rate(MemoryProfiles) ->
    %% Calculate average growth rate
    GrowthRates = [P#memory_profile.growth_rate || P <- maps:values(MemoryProfiles)],
    case GrowthRates of
        [] ->
            0.0;
        _ ->
            lists:sum(GrowthRates) / length(GrowthRates)
    end.

calculate_total_gc_count(GCAnalysis) ->
    %% Calculate total garbage collection count
    GCCounts = [G#gc_count || G <- maps:values(GCAnalysis)],
    lists:sum(GCCounts).

generate_recommendations(State) ->
    %% Generate recommendations based on monitoring results
    Recommendations = case State#monitor_state.alerts of
        [] ->
            [#{recommendation => "No memory leaks detected", priority => low}];
        _ ->
            lists:map(fun(Alert) ->
                generate_alert_recommendation(Alert)
            end, State#monitor_state.alerts)
    end,

    %% Add GC analysis recommendations
    GCRecommendations = analyze_gc_recommendations(State#monitor_state.gc_analysis),
    Recommendations ++ GCRecommendations.

generate_alert_recommendation(Alert) ->
    %% Generate recommendation for specific alert
    case Alert#alert_type of
        memory_leak ->
            #{
                recommendation => format_alert_message(Alert#monitor_id, Alert#growth_rate),
                priority => Alert#severity,
                action => investigate_memory_usage
            }
    end.

analyze_gc_recommendations(GCAnalysis) ->
    %% Analyze garbage collection patterns and generate recommendations
    GCStats = maps:values(GCAnalysis);

    case GCStats of
        [] ->
            [#{recommendation => "No GC analysis available", priority => low}];
        _ ->
            lists:foldl(fun(GCStat, Acc) ->
                case analyze_gc_stat(GCStat) of
                    none ->
                        Acc;
                    Recommendation ->
                        [Recommendation | Acc]
                end
            end, [], GCStats)
    end.

analyze_gc_stat(GCStat) ->
    %% Analyze individual GC statistics
    GCDelta = GCStat#gc_count_delta;
    MemoryBefore = GCStat#memory_before;
    MemoryAfter = GCStat#memory_after;

    case GCDelta of
        Delta when Delta > 1000 ->
            #{
                recommendation => "High GC frequency detected. Consider optimizing memory usage",
                priority => medium,
                action => optimize_memory_patterns
            };
        _ ->
            case MemoryAfter > MemoryBefore * 1.1 of
                true ->
                    #{
                        recommendation => "Memory growing after GC. Check for memory leaks",
                        priority => high,
                        action => investigate_memory_leaks
                    };
                false ->
                    none
            end
    end.

cleanup_old_profiles(State) ->
    %% Clean up old memory profiles
    CurrentTime = erlang:system_time(millisecond);
    CleanupThreshold = CurrentTime - 86400000; % 24 hours

    FilteredProfiles = maps:filter(fun(_Pid, Profile) ->
        LastSample = case Profile#memory_profile.memory_history of
            [Sample | _] -> Sample#timestamp;
            [] -> Profile#memory_profile.last_gc
        end,
        LastSample > CleanupThreshold
    end, State#monitor_state.memory_profiles),

    State#monitor_state{
        memory_profiles = FilteredProfiles
    }.

cleanup_memory_profiles(State) ->
    %% Cleanup all memory profiles
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true ->
                erlang:exit(Pid, normal);
            false ->
                ok
        end
    end, maps:keys(State#monitor_state.memory_profiles)).