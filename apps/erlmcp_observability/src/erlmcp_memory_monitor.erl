%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Monitor for ErlMCP - Process Memory Leak Detection
%%%
%%% This gen_server provides continuous monitoring of process memory usage,
%%% automatic detection of memory leaks, and integration with OpenTelemetry
%%% for comprehensive observability.
%%%
%%% == Key Features ==
%%%
%%% 1. **Periodic Memory Checks**: Scans all registered erlmcp processes
%%% 2. **Memory Leak Detection**: Identifies processes with growing memory patterns
%%% 3. **OTEL Integration**: Emits memory metrics as spans and events
%%% 4. **Auto-Hibernation**: Recommends or forces hibernation on high memory
%%% 5. **Dashboard Integration**: Sends metrics to erlmcp_dashboard_server
%%% 6. **Alert Thresholds**: Configurable per-process-type thresholds
%%%
%%% == Architecture ==
%%%
%%% The monitor runs periodic checks (default 5 seconds) on all registered
%%% erlmcp processes. For each process, it:
%%%
%%% <ol>
%%%   <li>Checks current memory usage via process_info/2</li>
%%%   <li>Compares against configured limits for process type</li>
%%%   <li>Tracks history to detect growth patterns (memory leaks)</li>
%%%   <li>Emits OTEL spans with memory attributes</li>
%%%   <li>Logs warnings or forces hibernation based on thresholds</li>
%%%   <li>Sends metrics to dashboard for visualization</li>
%%% </ol>
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Start the monitor
%%% {ok, Pid} = erlmcp_memory_monitor:start_link(#{
%%%     check_interval => 5000,
%%%     alert_threshold => 0.85,
%%%     enable_hibernation => true
%%% }),
%%%
%%% %% Register a process for monitoring
%%% ok = erlmcp_memory_monitor:register_process(self(), context),
%%%
%%% %% Manually trigger a check
%%% {ok, Report} = erlmcp_memory_monitor:check_now(),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_monitor).

-behaviour(gen_server).

%% API
-export([start_link/1, register_process/2, unregister_process/1,
         check_now/0, get_report/0, set_config/1, get_config/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include OTEL for observability (optional)
-ifdef(OTEL_AVAILABLE).
-include_lib("erlmcp_observability/include/erlmcp_otel.hrl").
-endif.

%%====================================================================
%% Type Definitions
%%====================================================================

-type process_type() :: context | tool | transport | generic.
-type process_entry() :: #{pid := pid(),
                            type := process_type(),
                            registered_at := integer(),
                            history => [integer()],
                            alerts => integer()}.
-type monitor_config() :: #{check_interval := pos_integer(),
                            alert_threshold := float(),
                            enable_hibernation := boolean(),
                            max_history => pos_integer()}.
-type monitor_report() :: #{timestamp => integer(),
                            checked_processes => pos_integer(),
                            high_memory => [pid()],
                            memory_leaks => [pid()],
                            hibernated => [pid()],
                            total_memory => non_neg_integer()}.

-record(state, {
    config :: monitor_config(),
    monitored_processes :: #{pid() => process_entry()},
    check_timer :: reference() | undefined,
    telemetry_enabled :: boolean()
}).

-type state() :: #state{}.

-export_type([process_type/0, monitor_config/0, monitor_report/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the memory monitor with configuration.
%% @param Config Monitor configuration map
-spec start_link(monitor_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Register a process for monitoring.
%% @param Pid Process to monitor
%% @param Type Process type (context, tool, transport, generic)
-spec register_process(pid(), process_type()) -> ok.
register_process(Pid, Type) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register_process, Pid, Type}).

%% @doc Unregister a process from monitoring.
%% @param Pid Process to stop monitoring
-spec unregister_process(pid()) -> ok.
unregister_process(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {unregister_process, Pid}).

%% @doc Trigger an immediate memory check.
%% Useful for testing or manual inspection.
%% @return {ok, Report} with monitoring results
-spec check_now() -> {ok, monitor_report()}.
check_now() ->
    gen_server:call(?MODULE, check_now).

%% @doc Get the latest monitoring report.
%% @return Monitor report map
-spec get_report() -> {ok, monitor_report()} | {error, not_available}.
get_report() ->
    gen_server:call(?MODULE, get_report).

%% @doc Update monitor configuration at runtime.
%% @param NewConfig Partial or full config update
-spec set_config(monitor_config()) -> ok.
set_config(NewConfig) ->
    gen_server:call(?MODULE, {set_config, NewConfig}).

%% @doc Get current monitor configuration.
%% @return Current configuration
-spec get_config() -> {ok, monitor_config()}.
get_config() ->
    gen_server:call(?MODULE, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(monitor_config()) -> {ok, state()}.
init(Config) ->
    %% Trap exits for supervised processes
    process_flag(trap_exit, true),

    %% Set high priority for monitoring
    try
        erlang:process_flag(priority, high)
    catch
        _:_ -> ok
    end,

    %% Check if OTEL is available
    TelemetryEnabled = code:is_loaded(erlmcp_otel) =/= false,

    %% Schedule first check
    CheckInterval = maps:get(check_interval, Config, 5000),
    Timer = erlang:send_after(CheckInterval, self(), check_memory),

    logger:info("Memory monitor started: interval=~pms, telemetry=~p",
                [CheckInterval, TelemetryEnabled]),

    {ok, #state{
        config = normalize_config(Config),
        monitored_processes = #{},
        check_timer = Timer,
        telemetry_enabled = TelemetryEnabled
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({register_process, Pid, Type}, _From, State) ->
    %% Add process to monitored set
    Entry = #{
        pid => Pid,
        type => Type,
        registered_at => erlang:system_time(millisecond),
        history => [],
        alerts => 0
    },
    NewProcesses = maps:put(Pid, Entry, State#state.monitored_processes),

    %% Monitor the process
    erlang:monitor(process, Pid),

    logger:debug("Registered process ~p for monitoring (type=~p)", [Pid, Type]),

    {reply, ok, State#state{monitored_processes = NewProcesses}, hibernate};

handle_call({unregister_process, Pid}, _From, State) ->
    %% Remove from monitored set
    NewProcesses = maps:remove(Pid, State#state.monitored_processes),

    logger:debug("Unregistered process ~p from monitoring", [Pid]),

    {reply, ok, State#state{monitored_processes = NewProcesses}, hibernate};

handle_call(check_now, _From, State) ->
    %% Perform immediate memory check
    {ok, Report, NewState} = do_memory_check(State),
    {reply, {ok, Report}, NewState, hibernate};

handle_call(get_report, _From, State) ->
    %% Return latest report (if available)
    case erlang:get(memory_monitor_report) of
        undefined ->
            {reply, {error, not_available}, State, hibernate};
        Report ->
            {reply, {ok, Report}, State, hibernate}
    end;

handle_call({set_config, NewConfig}, _From, State) ->
    %% Update configuration
    MergedConfig = maps:merge(State#state.config, NewConfig),
    NormalizedConfig = normalize_config(MergedConfig),

    %% Reschedule timer if interval changed
    OldInterval = maps:get(check_interval, State#state.config),
    NewTimer = case maps:get(check_interval, NewConfig) of
                   Interval when Interval =/= OldInterval ->
                       erlang:cancel_timer(State#state.check_timer),
                       erlang:send_after(Interval, self(), check_memory);
                   _ ->
                       State#state.check_timer
               end,

    logger:info("Memory monitor config updated: ~p", [NormalizedConfig]),

    {reply, ok, State#state{config = NormalizedConfig, check_timer = NewTimer}, hibernate};

handle_call(get_config, _From, State) ->
    {reply, {ok, State#state.config}, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(check_memory, State) ->
    %% Perform periodic memory check
    {ok, _Report, NewState} = do_memory_check(State),

    %% Schedule next check
    Interval = maps:get(check_interval, NewState#state.config),
    Timer = erlang:send_after(Interval, self(), check_memory),

    {noreply, NewState#state{check_timer = Timer}, hibernate};

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Process died - remove from monitoring
    NewProcesses = maps:remove(Pid, State#state.monitored_processes),

    logger:debug("Monitored process ~p died, removed from monitoring", [Pid]),

    {noreply, State#state{monitored_processes = NewProcesses}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel timer
    case State#state.check_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    logger:info("Memory monitor stopped"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Perform memory check on all monitored processes.
%% @private
-spec do_memory_check(state()) -> {ok, monitor_report(), state()}.
do_memory_check(State) ->
    MonitoredProcesses = State#state.monitored_processes,
    Config = State#state.config,

    %% Check each process
    {HighMemory, MemoryLeaks, Hibernated, NewProcesses, TotalMem} =
        maps:fold(fun(Pid, Entry, Acc) ->
                    check_process_memory(Pid, Entry, Config, Acc)
                  end,
                  {[], [], [], MonitoredProcesses, 0},
                  MonitoredProcesses),

    %% Build report
    Report = #{
        timestamp => erlang:system_time(millisecond),
        checked_processes => map_size(MonitoredProcesses),
        high_memory => HighMemory,
        memory_leaks => MemoryLeaks,
        hibernated => Hibernated,
        total_memory => TotalMem
    },

    %% Store report
    erlang:put(memory_monitor_report, Report),

    %% Emit OTEL metrics
    emit_telemetry(Report, State),

    %% Send to dashboard if available
    send_to_dashboard(Report),

    {ok, Report, State#state{monitored_processes = NewProcesses}}.

%% @doc Check memory usage of a single process.
%% @private
-spec check_process_memory(pid(), process_entry(), monitor_config(),
                            {[pid()], [pid()], [pid()], #{pid() => process_entry()}, non_neg_integer()}) ->
    {[pid()], [pid()], [pid()], #{pid() => process_entry()}, non_neg_integer()}.
check_process_memory(Pid, Entry, Config, {HighMemAcc, LeakAcc, HibAcc, ProcAcc, TotalAcc}) ->
    try
        %% Get memory info
        {memory, Memory} = erlang:process_info(Pid, memory),
        {binary, BinMemory} = erlang:process_info(Pid, binary),
        TotalMemory = Memory + BinMemory,

        %% Get limits for process type
        Type = maps:get(type, Entry),
        Limits = get_limits(Type),
        MaxHeap = maps:get(max_heap, Limits) * erlang:system_info(wordsize),
        AlertThreshold = maps:get(alert_threshold, Config, 0.85),

        %% Calculate usage percentage
        UsagePercent = (TotalMemory / MaxHeap) * 100,

        %% Update history
        History = maps:get(history, Entry, []),
        NewHistory = lists:sublist([TotalMemory | History], maps:get(max_history, Config, 10)),
        UpdatedEntry = Entry#{history => NewHistory},

        %% Check for issues
        {NewHighMem, NewLeak, NewHib} =
            case UsagePercent >= AlertThreshold * 100 of
                true ->
                    %% High memory usage
                    logger:warning("Process ~p (~p) high memory: ~.2f% (~p / ~p bytes)",
                                  [Pid, Type, UsagePercent, TotalMemory, MaxHeap]),

                    %% Check if we should force hibernation
                    ShouldHibernate = maps:get(enable_hibernation, Config, false)
                                      andalso UsagePercent >= maps:get(hibernate_threshold, Limits, 0.9) * 100,

                    case ShouldHibernate of
                        true ->
                            logger:warning("Forcing hibernation for process ~p", [Pid]),
                            %% Send hibernation message
                            Pid ! {hibernate_now, self()},
                            { HibAcc ++ [Pid], LeakAcc, HibAcc ++ [Pid] };
                        false ->
                            { HighMemAcc ++ [Pid], LeakAcc, HibAcc }
                    end;
                false ->
                    %% Check for memory leak (growing trend)
                    case detect_memory_leak(NewHistory) of
                        true ->
                            logger:warning("Process ~p (~p) suspected memory leak: ~p",
                                          [Pid, Type, NewHistory]),
                            { HighMemAcc, LeakAcc ++ [Pid], HibAcc };
                        false ->
                            { HighMemAcc, LeakAcc, HibAcc }
                    end
            end,

        {NewHighMem, NewLeak, NewHib, ProcAcc#{Pid => UpdatedEntry}, TotalAcc + TotalMemory}
    catch
        _:_ ->
            %% Process died or info unavailable
            {HighMemAcc, LeakAcc, HibAcc, ProcAcc, TotalAcc}
    end.

%% @doc Detect memory leak from history (growing trend).
%% @private
-spec detect_memory_leak([non_neg_integer()]) -> boolean().
detect_memory_leak(History) when length(History) < 5 ->
    false;
detect_memory_leak(History) ->
    %% Calculate linear regression slope
    %% If consistently growing, return true
    N = length(History),
    Indices = lists:seq(1, N),

    MeanY = lists:sum(History) / N,
    MeanX = lists:sum(Indices) / N,

    %% Calculate slope
    Numerator = lists:sum([ (X - MeanX) * (Y - MeanY) || {X, Y} <- lists:zip(Indices, History) ]),
    Denominator = lists:sum([ (X - MeanX) * (X - MeanX) || X <- Indices ]),

    case Denominator of
        +0.0 -> false;
        _ ->
            Slope = Numerator / Denominator,
            %% If slope > 1KB per check and mostly increasing
            Slope > 1024 andalso is_increasing(History)
    end.

%% @doc Check if history is mostly increasing.
%% @private
-spec is_increasing([non_neg_integer()]) -> boolean().
is_increasing([]) ->
    false;
is_increasing([_]) ->
    false;
is_increasing(History) ->
    Increases = [ Y > X || {X, Y} <- lists:zip(History, tl(History)) ],
    TrueCount = length([ I || I <- Increases, I =:= true ]),
    TrueCount / length(Increases) > 0.7.

%% @doc Get memory limits for process type.
%% @private
-spec get_limits(process_type()) -> #{max_heap => pos_integer(), hibernate_threshold => float()}.
get_limits(context) ->
    #{max_heap => 100_000_000, hibernate_threshold => 0.9};
get_limits(tool) ->
    #{max_heap => 50_000_000, hibernate_threshold => 0.85};
get_limits(transport) ->
    #{max_heap => 30_000_000, hibernate_threshold => 0.80};
get_limits(generic) ->
    #{max_heap => 20_000_000, hibernate_threshold => 0.80}.

%% @doc Emit OTEL telemetry for report.
%% @private
-spec emit_telemetry(monitor_report(), state()) -> ok.
emit_telemetry(_Report, #state{telemetry_enabled = false}) ->
    ok;
emit_telemetry(Report, #state{telemetry_enabled = true}) ->
    try
        %% Create span for memory check
        SpanCtx = erlmcp_otel:start_span(
            <<"erlmcp.memory.check">>,
            #{<<"monitor.checked_processes">> => maps:get(checked_processes, Report),
              <<"monitor.high_memory_count">> => length(maps:get(high_memory, Report)),
              <<"monitor.memory_leak_count">> => length(maps:get(memory_leaks, Report)),
              <<"monitor.hibernated_count">> => length(maps:get(hibernated, Report)),
              <<"monitor.total_memory">> => maps:get(total_memory, Report)}
        ),

        %% Add events for issues
        lists:foreach(fun(Pid) ->
            erlmcp_otel:add_event(SpanCtx, <<"high_memory">>, #{<<"pid">> => list_to_binary(pid_to_list(Pid))})
        end, maps:get(high_memory, Report)),

        lists:foreach(fun(Pid) ->
            erlmcp_otel:add_event(SpanCtx, <<"memory_leak">>, #{<<"pid">> => list_to_binary(pid_to_list(Pid))})
        end, maps:get(memory_leaks, Report)),

        erlmcp_otel:end_span(SpanCtx),
        ok
    catch
        _:_ -> ok
    end.

%% @doc Send report to dashboard.
%% @private
-spec send_to_dashboard(monitor_report()) -> ok.
send_to_dashboard(Report) ->
    try
        case whereis(erlmcp_dashboard_server) of
            undefined ->
                ok;
            Pid ->
                Pid ! {memory_report, Report},
                ok
        end
    catch
        _:_ -> ok
    end.

%% @doc Normalize configuration with defaults.
%% @private
-spec normalize_config(monitor_config()) -> monitor_config().
normalize_config(Config) ->
    Defaults = #{
        check_interval => 5000,
        alert_threshold => 0.85,
        enable_hibernation => false,
        max_history => 10
    },
    maps:merge(Defaults, Config).
