-module(erlmcp_upgrade_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_upgrade_span/1,
         end_upgrade_span/0,
         record_upgrade_phase/2,
         record_upgrade_metric/2,
         get_upgrade_progress/0,
         get_system_health/0,
         set_upgrade_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
% -include_lib("opentelemetry_api/include/otel_tracer.hrl").
% -include_lib("opentelemetry_api/include/otel_meter.hrl").

%% Stub macros for when opentelemetry is not available
-ifdef(OTEL_AVAILABLE).
-define(start_span(Name, Attrs), otel_tracer:start_span(?TRACER_NAME, Name, Attrs)).
-define(set_attribute(Key, Value), otel_span:set_attribute(otel_tracer:current_span(), Key, Value)).
-define(add_event(Name, Attrs), otel_span:add_event(otel_tracer:current_span(), Name, Attrs)).
-else.
-define(start_span(Name, Attrs), ok).
-define(set_attribute(Key, Value), ok).
-define(add_event(Name, Attrs), ok).
-endif.

-define(SERVER, ?MODULE).
-define(TRACER_NAME, "erlmcp_upgrade").
-define(METER_NAME, "erlmcp_upgrade_metrics").

-record(state, {
    current_span :: opentelemetry:span_ctx() | undefined,
    upgrade_start_time :: erlang:timestamp() | undefined,
    phases_completed = [] :: [{atom(), number()}], % {Phase, Duration_ms}
    current_phase :: atom() | undefined,
    upgrade_status :: idle | in_progress | completed | failed,
    metrics = #{} :: map()
}).

-type upgrade_status() :: idle | in_progress | completed | failed.
-type phase_name() :: atom().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_upgrade_span(binary()) -> ok | {error, term()}.
start_upgrade_span(TargetVersion) ->
    gen_server:call(?SERVER, {start_span, TargetVersion}).

-spec end_upgrade_span() -> ok.
end_upgrade_span() ->
    gen_server:cast(?SERVER, end_span).

-spec record_upgrade_phase(atom(), number()) -> ok.
record_upgrade_phase(PhaseName, DurationMs) ->
    gen_server:cast(?SERVER, {record_phase, PhaseName, DurationMs}).

-spec record_upgrade_metric(atom(), number()) -> ok.
record_upgrade_metric(MetricName, Value) ->
    gen_server:cast(?SERVER, {record_metric, MetricName, Value}).

-spec get_upgrade_progress() -> {ok, map()}.
get_upgrade_progress() ->
    gen_server:call(?SERVER, get_progress).

-spec get_system_health() -> map().
get_system_health() ->
    gen_server:call(?SERVER, get_health).

-spec set_upgrade_status(upgrade_status()) -> ok.
set_upgrade_status(Status) ->
    gen_server:cast(?SERVER, {set_status, Status}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    case code:is_loaded(otel_tracer_provider) of
        false -> ok;
        _ -> otel_tracer_provider:register_tracer(?TRACER_NAME)
    end,
    case code:is_loaded(otel_meter_provider) of
        false -> ok;
        _ -> otel_meter_provider:register_meter(?METER_NAME)
    end,
    State = #state{upgrade_status = idle},
    logger:info("Upgrade monitor started"),
    {ok, State}.

handle_call({start_span, TargetVersion}, _From, State) ->
    ?start_span(erlmcp_upgrade, #{
        target_version => TargetVersion,
        start_time => erlang:system_time(millisecond)
    }),
    StartTime = erlang:timestamp(),
    SpanCtx = case code:is_loaded(otel_tracer) of
        false -> undefined;
        _ -> otel_tracer:start_span(?TRACER_NAME, "erlmcp_upgrade")
    end,
    {reply, ok, State#state{
        current_span = SpanCtx,
        upgrade_start_time = StartTime,
        upgrade_status = in_progress
    }};

handle_call(get_progress, _From, State) ->
    Progress = #{
        status => State#state.upgrade_status,
        current_phase => State#state.current_phase,
        phases_completed => State#state.phases_completed,
        start_time => State#state.upgrade_start_time,
        metrics => State#state.metrics
    },
    {reply, {ok, Progress}, State};

handle_call(get_health, _From, State) ->
    Health = calculate_system_health(),
    {reply, Health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(end_span, State) ->
    case State#state.current_span of
        undefined ->
            {noreply, State};
        SpanCtx ->
            case code:is_loaded(otel_tracer) of
                false -> ok;
                _ -> otel_tracer:end_span(SpanCtx)
            end,
            TotalDuration = case State#state.upgrade_start_time of
                undefined -> 0;
                StartTime -> timer:now_diff(erlang:timestamp(), StartTime) div 1000
            end,
            ?set_attribute("upgrade.duration_ms", TotalDuration),
            ?set_attribute("upgrade.status", State#state.upgrade_status),
            {noreply, State#state{
                current_span = undefined,
                upgrade_status = completed,
                metrics = maps:put(total_duration_ms, TotalDuration, State#state.metrics)
            }}
    end;

handle_cast({record_phase, PhaseName, DurationMs}, State) ->
    ?add_event("upgrade_phase", #{phase => PhaseName, duration_ms => DurationMs}),
    NewPhases = [{PhaseName, DurationMs} | State#state.phases_completed],
    {noreply, State#state{phases_completed = NewPhases}};

handle_cast({record_metric, MetricName, Value}, State) ->
    ?set_attribute(atom_to_list(MetricName), Value),
    NewMetrics = maps:put(MetricName, Value, State#state.metrics),
    {noreply, State#state{metrics = NewMetrics}};

handle_cast({set_status, Status}, State) ->
    ?set_attribute("upgrade.status", Status),
    {noreply, State#state{upgrade_status = Status}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_system_health() ->
    %% Comprehensive health check for upgrade readiness
    HealthChecks = [
        {memory_usage, check_memory_usage()},
        {process_count, check_process_count()},
        {ets_table_count, check_ets_tables()},
        {port_count, check_port_count()},
        {message_queue_depths, check_message_queues()},
        {cpu_usage, check_cpu_usage()},
        {disk_io, check_disk_io()}
    ],

    %% Calculate overall health score
    Score = compute_health_score(HealthChecks),

    Status = case Score of
        S when S >= 0.9 -> healthy;
        S when S >= 0.7 -> degraded;
        _ -> unhealthy
    end,

    #{
        status => Status,
        score => Score,
        checks => maps:from_list(HealthChecks),
        timestamp => erlang:system_time(millisecond)
    }.

check_memory_usage() ->
    Memory = erlang:memory(),
    Total = proplists:get_value(total, Memory),
    SystemTotal = erlang:system_info(total_memory),
    UsageRatio = Total / SystemTotal,
    #{value => UsageRatio, status => case UsageRatio < 0.9 of true -> ok; false -> warning end}.

check_process_count() ->
    ProcessCount = erlang:system_info(process_count),
    MaxProcesses = erlang:system_info(process_limit),
    UsageRatio = ProcessCount / MaxProcesses,
    #{value => UsageRatio, status => case UsageRatio < 0.9 of true -> ok; false -> warning end}.

check_ets_tables() ->
    try
        EtsCount = length(ets:all()),
        #{value => EtsCount, status => ok}
    catch
        _:_ ->
            #{value => 0, status => ok}
    end.

check_port_count() ->
    PortCount = erlang:system_info(port_count),
    MaxPorts = erlang:system_info(port_limit),
    UsageRatio = PortCount / MaxPorts,
    #{value => UsageRatio, status => case UsageRatio < 0.9 of true -> ok; false -> warning end}.

check_message_queues() ->
    %% Check for excessive message queue depths
    RegistryDepth = erlmcp_registry:get_queue_depth(),
    #{value => RegistryDepth, status => case RegistryDepth < 1000 of true -> ok; false -> warning end}.

check_cpu_usage() ->
    %% CPU usage check (simplified)
    #{value => 0.5, status => ok}. % Placeholder

check_disk_io() ->
    %% Disk I/O check (simplified)
    #{value => 0.3, status => ok}. % Placeholder

compute_health_score(HealthChecks) ->
    Scores = [case Status of
        ok -> 1.0;
        warning -> 0.5;
        error -> 0.0
    end || {_Name, #{status := Status}} <- HealthChecks],
    case Scores of
        [] -> 1.0;
        _ -> lists:sum(Scores) / length(Scores)
    end.
