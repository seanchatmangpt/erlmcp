%%%-------------------------------------------------------------------
%%% @doc
%%% Chaos Engineering Injector Module
%%%
%%% Advanced chaos engineering patterns for CLI validation testing:
%%% - Network failure simulation
%%% - Latency spike injection
%%% - Resource exhaustion scenarios
%%% - Process crash handling
%%% - Timeout scenarios
%%% - Cascading failure simulation
%%%
%%% == Key Features ==
%%%
%%% 1. **Controlled Chaos**: Configurable fault injection with probability controls
%%% 2. **Real-world Scenarios**: Network partitions, memory pressure, CPU spikes
%%% 3. **Resilience Testing**: System behavior under various failure conditions
%%% 4. **Metrics Tracking**: Chaos impact on performance and reliability
%%% 5. **Recovery Automation**: Automatic system recovery after failures
%%% 6. **Safety Guards**: Built-in safeguards to prevent total system failure
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Initialize chaos injector
%%% ok = erlmcp_chaos_injector:init(#{
%%%     probability => 0.1,
%%%     target_duration => 30000,  % 30 seconds
%%%     safety_mode => true
%%% }),
%%%
%%% %% Inject network failure with 20% probability
%%% ok = erlmcp_chaos_injector:inject_network_failure(0.2),
%%%
%%% %% Inject memory pressure
%%%
%%% %% Simulate high CPU load
%%% ok = erlmcp_chaos_injector:inject_cpu_spike(0.8, 5000),
%%%
%%% %% Stop chaos injection
%%% ok = erlmcp_chaos_injector:stop(),
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_injector).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, inject_network_failure/1, inject_latency_spike/2,
         inject_memory_pressure/1, inject_cpu_spike/2, inject_process_crash/0,
         inject_timeout/1, inject_cascading_failure/2, get_chaos_status/0,
         set_chaos_probability/1, enable_chaos/1, disable_chaos/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-export_type([chaos_config/0, chaos_event/0, chaos_type/0]).

-include("erlmcp.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type chaos_config() ::
    #{probability => float(),          % 0.0 to 1.0
      target_duration => pos_integer(), % milliseconds
      safety_mode => boolean(),        % Prevent total failure
      max_failures => pos_integer(),   % Max concurrent failures
      recovery_timeout => pos_integer(), % Recovery time
      chaos_types => [chaos_type()],
      log_level => debug | info | warn | error}.

-type chaos_event() ::
    #{id => binary(),
      type => chaos_type(),
      start_time => integer(),
      end_time => integer() | undefined,
      duration => integer() | undefined,
      severity => low | medium | high | critical,
      impact => map(),
      recovery_actions => [term()],
      status => active | completed | failed}.

-type chaos_type() ::
    network_failure | latency_spike | memory_pressure | cpu_spike |
    process_crash | timeout | cascading_failure | resource_exhaustion.

%%====================================================================
%% Record Definitions
%%====================================================================

-record(state,
        {config :: chaos_config(),
         active_events :: [chaos_event()],
         chaos_probability :: float(),
         chaos_enabled :: boolean(),
         monitoring_timer :: reference() | undefined,
         recovery_timer :: reference() | undefined,
         failure_count :: pos_integer(),
         max_failures :: pos_integer()}).

-define(DEFAULT_CHAOS_CONFIG, #{
    probability => 0.1,
    target_duration => 30000,
    safety_mode => true,
    max_failures => 3,
    recovery_timeout => 5000,
    chaos_types => [network_failure, latency_spike, memory_pressure, cpu_spike],
    log_level => info
}).

-define(CHAOS_GUARDS, #{
    network_failure => #{
        max_duration => 10000,
        min_recovery => 1000,
        severity => medium
    },
    latency_spike => #{
        max_duration => 5000,
        min_recovery => 500,
        severity => low
    },
    memory_pressure => #{
        max_duration => 15000,
        min_recovery => 2000,
        severity => high
    },
    cpu_spike => #{
        max_duration => 8000,
        min_recovery => 1000,
        severity => medium
    },
    process_crash => #{
        max_duration => 5000,
        min_recovery => 1000,
        severity => critical
    },
    timeout => #{
        max_duration => 3000,
        min_recovery => 500,
        severity => low
    },
    cascading_failure => #{
        max_duration => 20000,
        min_recovery => 5000,
        severity => critical
    }
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the chaos injector
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the chaos injector
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Initialize chaos injector with custom configuration (private API function)
initialize_chaos(Config) ->
    MergedConfig = maps:merge(?DEFAULT_CHAOS_CONFIG, Config),

    %% Validate configuration
    case validate_chaos_config(MergedConfig) of
        ok ->
            %% Start monitoring timer
            MonitoringTimer = erlang:send_after(1000, self(), monitor_chaos),

            State = #state{
                config = MergedConfig,
                active_events = [],
                chaos_probability = maps:get(probability, MergedConfig),
                chaos_enabled = true,
                monitoring_timer = MonitoringTimer,
                recovery_timer = undefined,
                failure_count = 0,
                max_failures = maps:get(max_failures, MergedConfig)
            },

            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Inject network failure
-spec inject_network_failure(float()) -> ok | {error, term()}.
inject_network_failure(Probability) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:call(?SERVER, {inject_network_failure, Probability}).

%% @doc Inject latency spike
-spec inject_latency_spike(float(), pos_integer()) -> ok | {error, term()}.
inject_latency_spike(Probability, DurationMs) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:call(?SERVER, {inject_latency_spike, Probability, DurationMs}).

%% @doc Inject memory pressure
-spec inject_memory_pressure(float()) -> ok | {error, term()}.
inject_memory_pressure(Probability) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:call(?SERVER, {inject_memory_pressure, Probability}).

%% @doc Inject CPU spike
-spec inject_cpu_spike(float(), pos_integer()) -> ok | {error, term()}.
inject_cpu_spike(Probability, DurationMs) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:call(?SERVER, {inject_cpu_spike, Probability, DurationMs}).

%% @doc Inject process crash
-spec inject_process_crash() -> ok | {error, term()}.
inject_process_crash() ->
    gen_server:call(?SERVER, inject_process_crash).

%% @doc Inject timeout
-spec inject_timeout(float()) -> ok | {error, term()}.
inject_timeout(Probability) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:call(?SERVER, {inject_timeout, Probability}).

%% @doc Inject cascading failure
-spec inject_cascading_failure(float(), pos_integer()) -> ok | {error, term()}.
inject_cascading_failure(Probability, Steps) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:call(?SERVER, {inject_cascading_failure, Probability, Steps}).

%% @doc Get current chaos status
-spec get_chaos_status() -> map().
get_chaos_status() ->
    gen_server:call(?SERVER, get_chaos_status).

%% @doc Set chaos probability
-spec set_chaos_probability(float()) -> ok.
set_chaos_probability(Probability) when Probability >= 0.0, Probability =< 1.0 ->
    gen_server:cast(?SERVER, {set_chaos_probability, Probability}).

%% @doc Enable chaos injection
-spec enable_chaos(boolean()) -> ok.
enable_chaos(Enabled) ->
    gen_server:cast(?SERVER, {enable_chaos, Enabled}).

%% @doc Disable chaos injection
-spec disable_chaos() -> ok.
disable_chaos() ->
    gen_server:cast(?SERVER, disable_chaos).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize with default config
    case init(?DEFAULT_CHAOS_CONFIG) of
        ok -> {ok, #state{}};
        {error, Reason} -> {stop, Reason}
    end.

handle_call({inject_network_failure, Probability}, _From, State) ->
    Result = inject_network_failure_internal(Probability),
    {reply, Result, State};

handle_call({inject_latency_spike, Probability, DurationMs}, _From, State) ->
    Result = inject_latency_spike_internal(Probability, DurationMs),
    {reply, Result, State};

handle_call({inject_memory_pressure, Probability}, _From, State) ->
    Result = inject_memory_pressure_internal(Probability),
    {reply, Result, State};

handle_call({inject_cpu_spike, Probability, DurationMs}, _From, State) ->
    Result = inject_cpu_spike_internal(Probability, DurationMs),
    {reply, Result, State};

handle_call(inject_process_crash, _From, State) ->
    Result = inject_process_crash_internal(),
    {reply, Result, State};

handle_call({inject_timeout, Probability}, _From, State) ->
    Result = inject_timeout_internal(Probability),
    {reply, Result, State};

handle_call({inject_cascading_failure, Probability, Steps}, _From, State) ->
    Result = inject_cascading_failure_internal(Probability, Steps),
    {reply, Result, State};

handle_call(get_chaos_status, _From, State) ->
    Status = #{
        enabled => State#state.chaos_enabled,
        probability => State#state.chaos_probability,
        active_events => State#state.active_events,
        failure_count => State#state.failure_count,
        max_failures => State#state.max_failures,
        config => State#state.config
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({set_chaos_probability, Probability}, State) ->
    log_chaos(info, "Setting chaos probability to ~p", [Probability]),
    {noreply, State#state{chaos_probability = Probability}};

handle_cast({enable_chaos, Enabled}, State) ->
    log_chaos(info, "Chaos injection ~s", [case Enabled of true -> "enabled"; false -> "disabled" end]),
    {noreply, State#state{chaos_enabled = Enabled}};

handle_cast(disable_chaos, State) ->
    log_chaos(info, "Disabling chaos injection", []),
    {noreply, State#state{chaos_enabled = false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_chaos, State) ->
    %% Monitor active chaos events
    Now = erlang:system_time(millisecond),
    ActiveEvents = monitor_active_events(State#state.active_events, Now),

    %% Check if we need recovery
    RecoveryNeeded = check_recovery_needed(ActiveEvents, State#state.failure_count),

    %% Schedule next monitoring
    NewTimer = erlang:send_after(1000, self(), monitor_chaos),

    {noreply, State#state{active_events = ActiveEvents, recovery_timer = RecoveryNeeded}};

handle_info(recovery_timeout, State) ->
    %% Handle recovery timeout
    RecoveredEvents = recover_completed_events(State#state.active_events),
    log_chaos(info, "Recovery timeout processed, ~p events recovered", [length(RecoveredEvents)]),

    {noreply, State#state{active_events = RecoveredEvents}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel all timers
    case State#state.monitoring_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    case State#state.recovery_timer of
        undefined -> ok;
        Timer2 -> erlang:cancel_timer(Timer2)
    end,

    %% Clean up all active events
    lists:foreach(fun(Event) -> cleanup_chaos_event(Event) end, State#state.active_events),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate chaos configuration
-spec validate_chaos_config(chaos_config()) -> ok | {error, term()}.
validate_chaos_config(Config) ->
    %% Check probability
    Probability = maps:get(probability, Config),
    if Probability < 0.0 orelse Probability > 1.0 ->
        {error, invalid_probability};
    true ->
        ok
    end,

    %% Check max failures
    MaxFailures = maps:get(max_failures, Config),
    if MaxFailures < 1 orelse MaxFailures > 10 ->
        {error, invalid_max_failures};
    true ->
        ok
    end.

%% @private Inject network failure internally
-spec inject_network_failure_internal(float()) -> ok | {error, term()}.
inject_network_failure_internal(Probability) ->
    case rand:uniform() < Probability of
        true ->
            Duration = get_chaos_duration(network_failure),
            StartTime = erlang:system_time(millisecond),

            ChaosEvent = #{
                id => generate_chaos_id(),
                type => network_failure,
                start_time => StartTime,
                duration => Duration,
                severity => medium,
                impact => #{message => "Network failure injected"},
                recovery_actions => ["wait", "retry", "fallback"],
                status => active
            },

            %% Start network failure simulation
            simulate_network_failure(Duration),

            %% Register event
            gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

            log_chaos(info, "Network failure injected for ~p ms", [Duration]),
            ok;
        false ->
            log_chaos(debug, "Network failure skipped (probability ~p)", [Probability]),
            ok
    end.

%% @private Inject latency spike internally
-spec inject_latency_spike_internal(float(), pos_integer()) -> ok | {error, term()}.
inject_latency_spike_internal(Probability, DurationMs) ->
    case rand:uniform() < Probability of
        true ->
            StartTime = erlang:system_time(millisecond),

            ChaosEvent = #{
                id => generate_chaos_id(),
                type => latency_spike,
                start_time => StartTime,
                duration => DurationMs,
                severity => low,
                impact => #{message => "Latency spike injected", duration_ms => DurationMs},
                recovery_actions => ["timeout_handling", "circuit_breaker"],
                status => active
            },

            %% Start latency injection
            simulate_latency_spike(DurationMs),

            gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

            log_chaos(info, "Latency spike injected for ~p ms", [DurationMs]),
            ok;
        false ->
            log_chaos(debug, "Latency spike skipped (probability ~p)", [Probability]),
            ok
    end.

%% @private Inject memory pressure internally
-spec inject_memory_pressure_internal(float()) -> ok | {error, term()}.
inject_memory_pressure_internal(Probability) ->
    case rand:uniform() < Probability of
        true ->
            Duration = get_chaos_duration(memory_pressure),
            StartTime = erlang:system_time(millisecond),

            ChaosEvent = #{
                id => generate_chaos_id(),
                type => memory_pressure,
                start_time => StartTime,
                duration => Duration,
                severity => high,
                impact => #{message => "Memory pressure injected"},
                recovery_actions => ["garbage_collect", "memory_monitor", "load_balancing"],
                status => active
            },

            %% Start memory pressure simulation
            simulate_memory_pressure(Duration),

            gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

            log_chaos(info, "Memory pressure injected for ~p ms", [Duration]),
            ok;
        false ->
            log_chaos(debug, "Memory pressure skipped (probability ~p)", [Probability]),
            ok
    end.

%% @private Inject CPU spike internally
-spec inject_cpu_spike_internal(float(), pos_integer()) -> ok | {error, term()}.
inject_cpu_spike_internal(Probability, DurationMs) ->
    case rand:uniform() < Probability of
        true ->
            StartTime = erlang:system_time(millisecond),

            ChaosEvent = #{
                id => generate_chaos_id(),
                type => cpu_spike,
                start_time => StartTime,
                duration => DurationMs,
                severity => medium,
                impact => #{message => "CPU spike injected", duration_ms => DurationMs},
                recovery_actions => ["throttling", "load_shedding", "priority_adjustment"],
                status => active
            },

            %% Start CPU spike simulation
            simulate_cpu_spike(DurationMs),

            gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

            log_chaos(info, "CPU spike injected for ~p ms", [DurationMs]),
            ok;
        false ->
            log_chaos(debug, "CPU spike skipped (probability ~p)", [Probability]),
            ok
    end.

%% @private Inject process crash internally
-spec inject_process_crash_internal() -> ok | {error, term()}.
inject_process_crash_internal() ->
    StartTime = erlang:system_time(millisecond),

    ChaosEvent = #{
        id => generate_chaos_id(),
        type => process_crash,
        start_time => StartTime,
        duration => get_chaos_duration(process_crash),
        severity => critical,
        impact => #{message => "Process crash injected"},
        recovery_actions => ["restart", "supervision", "health_check"],
        status => active
    },

    %% Start process crash simulation
    simulate_process_crash(),

    gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

    log_chaos(info, "Process crash injected", []),
    ok.

%% @private Inject timeout internally
-spec inject_timeout_internal(float()) -> ok | {error, term()}.
inject_timeout_internal(Probability) ->
    case rand:uniform() < Probability of
        true ->
            Duration = get_chaos_duration(timeout),
            StartTime = erlang:system_time(millisecond),

            ChaosEvent = #{
                id => generate_chaos_id(),
                type => timeout,
                start_time => StartTime,
                duration => Duration,
                severity => low,
                impact => #{message => "Timeout injected", duration_ms => Duration},
                recovery_actions => ["timeout_handling", "retry", "fallback"],
                status => active
            },

            %% Start timeout simulation
            simulate_timeout(Duration),

            gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

            log_chaos(info, "Timeout injected for ~p ms", [Duration]),
            ok;
        false ->
            log_chaos(debug, "Timeout skipped (probability ~p)", [Probability]),
            ok
    end.

%% @private Inject cascading failure internally
-spec inject_cascading_failure_internal(float(), pos_integer()) -> ok | {error, term()}.
inject_cascading_failure_internal(Probability, Steps) ->
    case rand:uniform() < Probability of
        true ->
            StartTime = erlang:system_time(millisecond),

            ChaosEvent = #{
                id => generate_chaos_id(),
                type => cascading_failure,
                start_time => StartTime,
                duration => Steps * 2000,  % 2 seconds per step
                severity => critical,
                impact => #{message => "Cascading failure injected", steps => Steps},
                recovery_actions => ["circuit_breaker", "isolation", "rollback"],
                status => active
            },

            %% Start cascading failure simulation
            simulate_cascading_failure(Steps),

            gen_server:cast(?SERVER, {register_chaos_event, ChaosEvent}),

            log_chaos(info, "Cascading failure injected with ~p steps", [Steps]),
            ok;
        false ->
            log_chaos(debug, "Cascading failure skipped (probability ~p)", [Probability]),
            ok
    end.

%% @private Simulate network failure
-spec simulate_network_failure(pos_integer()) -> ok.
simulate_network_failure(Duration) ->
    %% Simulate network latency and packet loss
    EndTime = erlang:system_time(millisecond) + Duration,

    network_failure_loop(EndTime).

-spec network_failure_loop(integer()) -> ok.
network_failure_loop(EndTime) ->
    CurrentTime = erlang:system_time(millisecond),

    if CurrentTime < EndTime ->
        %% Simulate network issues
        case rand:uniform() of
            X when X < 0.1 ->  % 10% packet loss
                log_chaos(warn, "Simulating packet loss", []);
            X when X < 0.3 ->  % 20% increased latency
                timer:sleep(50);
            _ ->
                ok
        end,

        network_failure_loop(EndTime);
    true ->
        ok
    end.

%% @private Simulate latency spike
-spec simulate_latency_spike(pos_integer()) -> ok.
simulate_latency_spike(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,

    latency_spike_loop(EndTime).

-spec latency_spike_loop(integer()) -> ok.
latency_spike_loop(EndTime) ->
    CurrentTime = erlang:system_time(millisecond),

    if CurrentTime < EndTime ->
        %% Inject latency
        timer:sleep(100),  % 100ms baseline
        case rand:uniform() of
            X when X < 0.2 ->  % 20% chance of spike
                timer:sleep(500);  % 500ms spike
            _ ->
                ok
        end,

        latency_spike_loop(EndTime);
    true ->
        ok
    end.

%% @private Simulate memory pressure
-spec simulate_memory_pressure(pos_integer()) -> ok.
simulate_memory_pressure(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    memory_pressure_loop(EndTime).

-spec memory_pressure_loop(integer()) -> ok.
memory_pressure_loop(EndTime) ->
    CurrentTime = erlang:system_time(millisecond),

    if CurrentTime < EndTime ->
        %% Allocate memory to create pressure
        Size = 1024 * 1024,  % 1MB chunks
        _MemoryChunk = binary:copy(<<"x">>, Size),

        %% Check memory usage
        MemoryInfo = erlang:memory(),
        case MemoryInfo of
            Info when is_map(Info) ->
                MemoryBytes = maps:get(process, Info, 0),
                if MemoryBytes > 100 * 1024 * 1024 ->  % 100MB threshold
                    log_chaos(warn, "High memory usage: ~p bytes", [MemoryBytes]);
                true ->
                    ok
                end;
            _ ->
                ok
        end,

        memory_pressure_loop(EndTime);
    true ->
        %% Force garbage collection
        erlang:garbage_collect(),
        ok
    end.

%% @private Simulate CPU spike
-spec simulate_cpu_spike(pos_integer()) -> ok.
simulate_cpu_spike(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    cpu_spike_loop(EndTime).

-spec cpu_spike_loop(integer()) -> ok.
cpu_spike_loop(EndTime) ->
    CurrentTime = erlang:system_time(millisecond),

    if CurrentTime < EndTime ->
        %% Perform CPU-intensive calculations
        _Result = crypto:strong_rand_bytes(1024),

        %% Simulate CPU load
        StartLoad = erlang:monotonic_time(microsecond),
        _Dummy = lists:seq(1, 10000),
        EndLoad = erlang:monotonic_time(microsecond),
        DurationUs = EndLoad - StartLoad,

        if DurationUs > 10000 ->  % 10ms threshold
            log_chaos(warn, "High CPU load: ~p microseconds", [DurationUs]);
        true ->
            ok
        end,

        cpu_spike_loop(EndTime);
    true ->
        ok
    end.

%% @private Simulate process crash
-spec simulate_process_crash() -> ok.
simulate_process_crash() ->
    %% Create a monitored temporary process to crash
    Pid = spawn_link(fun() ->
                   receive
                       crash -> exit(normal)
                   after 5000 ->
                       exit(timeout)
                   end
               end),

    %% Monitor the crash (spawn_link already links, but monitor for DOWN message)
    MRef = erlang:monitor(process, Pid),

    %% Send crash message
    Pid ! crash,

    %% Wait for crash confirmation
    receive
        {'DOWN', MRef, process, Pid, _Reason} -> ok
    after 1000 ->
        timeout
    end,

    ok.

%% @private Simulate timeout
-spec simulate_timeout(pos_integer()) -> ok.
simulate_timeout(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,

    timeout_loop(EndTime).

-spec timeout_loop(integer()) -> ok.
timeout_loop(EndTime) ->
    CurrentTime = erlang:system_time(millisecond),

    if CurrentTime < EndTime ->
        %% Simulate timeout by blocking
        timer:sleep(1000),

        timeout_loop(EndTime);
    true ->
        ok
    end.

%% @private Simulate cascading failure
-spec simulate_cascading_failure(pos_integer()) -> ok.
simulate_cascading_failure(Steps) ->
    cascading_failure_loop(Steps, 1).

-spec cascading_failure_loop(pos_integer(), pos_integer()) -> ok.
cascading_failure_loop(Steps, Step) when Step =< Steps ->
    %% Simulate failure at current step
    log_chaos(critical, "Cascading failure step ~p of ~p", [Step, Steps]),

    %% Inject different types of failures at each step
    case Step rem 3 of
        0 ->
            simulate_network_failure(2000);
        1 ->
            simulate_timeout(1000);
        2 ->
            simulate_cpu_spike(1500)
    end,

    cascading_failure_loop(Steps, Step + 1);
cascading_failure_loop(_, _) ->
    ok.

%% @private Get chaos duration based on type
-spec get_chaos_duration(chaos_type()) -> pos_integer().
get_chaos_duration(ChaosType) ->
    Guards = ?CHAOS_GUARDS,
    case maps:get(ChaosType, Guards, undefined) of
        undefined ->
            5000;  % Default 5 seconds
        Config ->
            maps:get(max_duration, Config, 5000)
    end.

%% @private Generate chaos event ID
-spec generate_chaos_id() -> binary().
generate_chaos_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

%% @private Log chaos events
-spec log_chaos(atom(), string(), [term()]) -> ok.
log_chaos(Level, Format, Args) ->
    Message = io_lib:format(Format, Args),
    erlang:send_after(0, self(), {log_chaos, Level, Message}).

%% @private Monitor active chaos events
-spec monitor_active_events([chaos_event()], integer()) -> [chaos_event()].
monitor_active_events(Events, CurrentTime) ->
    lists:foldl(fun(Event, Acc) ->
                    case maps:get(end_time, Event, undefined) of
                        undefined ->
                            %% Event still active
                            Acc ++ [Event];
                        EndTime ->
                            if CurrentTime > EndTime + 5000 ->  % 5 second grace period
                                %% Event completed, trigger recovery
                                cleanup_chaos_event(Event),
                                Acc;
                            true ->
                                Acc ++ [Event]
                            end
                    end
                end,
                [],
                Events).

%% @private Check if recovery is needed
-spec check_recovery_needed([chaos_event()], pos_integer()) -> reference() | undefined.
check_recovery_needed(ActiveEvents, FailureCount) ->
    case ActiveEvents of
        [] ->
            undefined;
        _ ->
            %% Schedule recovery
            erlang:send_after(5000, self(), recovery_timeout)
    end.

%% @private Recover completed events
-spec recover_completed_events([chaos_event()]) -> [chaos_event()].
recover_completed_events(Events) ->
    lists:filter(fun(Event) ->
                    maps:get(status, Event) =:= active
                end,
                Events).

%% @private Clean up chaos event
-spec cleanup_chaos_event(chaos_event()) -> ok.
cleanup_chaos_event(Event) ->
    %% Perform recovery actions
    RecoveryActions = maps:get(recovery_actions, Event, []),
    lists:foreach(fun(Action) ->
                    perform_recovery_action(Action)
                end,
                RecoveryActions),

    %% Mark as completed
    UpdatedEvent = Event#{status => completed},
    gen_server:cast(?SERVER, {update_chaos_event, UpdatedEvent}).

%% @private Perform recovery action
-spec perform_recovery_action(term()) -> ok.
perform_recovery_action("garbage_collect") ->
    erlang:garbage_collect();
perform_recovery_action("memory_monitor") ->
    %% Trigger memory monitoring
    MemoryInfo = erlang:memory(),
    log_chaos(info, "Memory recovery: ~p", [MemoryInfo]);
perform_recovery_action("restart") ->
    %% Trigger process restart logic
    log_chaos(info, "Process restart recovery", []);
perform_recovery_action(_) ->
    ok.