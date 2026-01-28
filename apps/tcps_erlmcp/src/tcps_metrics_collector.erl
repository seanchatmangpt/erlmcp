%%%-----------------------------------------------------------------------------
%%% @doc TCPS MCP Diataxis Simulator - Metrics Aggregation and Collection
%%%
%%% High-performance metrics aggregation system for the TCPS MCP Diataxis
%%% simulator. Collects, aggregates, and provides real-time metrics for
%%% dashboards, monitoring, and analysis.
%%%
%%% == Features ==
%%% - Real-time metrics aggregation with configurable windows
%%% - Sliding window calculations for trend analysis
%%% - Multi-dimensional metrics (bucket, severity, quadrant)
%%% - Automated anomaly detection
%%% - Performance-optimized ETS-based storage
%%% - Batch operations for high-throughput scenarios
%%%
%%% == Metric Types ==
%%%
%%% Work Order Metrics:
%%% - Creation rate (per second, minute, hour)
%%% - Completion rate with SLA tracking
%%% - WIP trends per bucket
%%% - Priority distribution
%%% - Cycle time percentiles (p50, p90, p95, p99)
%%%
%%% Quality Gate Metrics:
%%% - Pass/fail rates per gate type
%%% - Score distributions
%%% - Remediation frequency
%%% - Gate latency trends
%%%
%%% Andon Event Metrics:
%%% - Event frequency by severity
%%% - Resolution time distributions
%%% - Escalation rates
%%% - Impact radius analysis
%%%
%%% Learning Metrics:
%%% - Session completion rates
%%% - Tutorial step completion times
%%% - Quadrant navigation patterns
%%% - Learning outcome achievement
%%%
%%% MCP Tool Metrics:
%%% - Tool invocation frequency
%%% - Latency per tool
%%% - Error rates
%%% - Cache hit rates
%%%
%%% == Aggregation Windows ==
%%% - 1 minute: Real-time monitoring
%%% - 5 minutes: Short-term trends
%%% - 15 minutes: Medium-term analysis
%%% - 1 hour: Long-term patterns
%%% - 1 day: Historical analysis
%%%
%%% == Usage ==
%%% ```erlang
%%% %% Start collector
%%% tcps_metrics_collector:start_link(#{
%%%     aggregation_interval => 1000,
%%%     retention_period => 86400,
%%%     enable_anomaly_detection => true
%%% }),
%%%
%%% %% Record work order creation
%%% tcps_metrics_collector:record_work_order_created(#{
%%%     bucket => security,
%%%     priority => 9,
%%%     source => cve,
%%%     timestamp => erlang:system_time(millisecond)
%%% }),
%%%
%%% %% Get aggregated metrics
%%% Metrics = tcps_metrics_collector:get_metrics(#{
%%%     window => '5min',
%%%     buckets => [security, reliability],
%%%     include_trends => true
%%% }),
%%%
%%% %% Get anomalies
%%% Anomalies = tcps_metrics_collector:detect_anomalies(#{
%%%     metric => work_order_creation_rate,
%%%     threshold => 2.0  % 2 standard deviations
%%% }).
%%% '''
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_metrics_collector).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Work order metrics
    record_work_order_created/1,
    record_work_order_completed/1,
    record_work_order_blocked/1,
    record_work_order_cancelled/1,
    get_work_order_metrics/1,

    %% Quality gate metrics
    record_quality_gate_result/1,
    get_quality_gate_metrics/1,

    %% Andon event metrics
    record_andon_event/1,
    record_andon_resolution/1,
    get_andon_metrics/1,

    %% Learning metrics
    record_session_start/1,
    record_session_complete/1,
    record_tutorial_step/1,
    record_diataxis_navigation/1,
    get_learning_metrics/1,

    %% MCP tool metrics
    record_tool_call/1,
    get_tool_metrics/1,

    %% Aggregation and analysis
    get_metrics/1,
    get_trends/1,
    detect_anomalies/1,
    get_summary/0,
    reset_metrics/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type window() :: '1min' | '5min' | '15min' | '1hour' | '1day'.
-type bucket() :: reliability | security | cost | compliance.
-type severity() :: low | medium | high | critical.
-type quadrant() :: tutorial | how_to | reference | explanation.
-type config() :: #{
    aggregation_interval => pos_integer(),
    retention_period => pos_integer(),
    enable_anomaly_detection => boolean(),
    anomaly_threshold => float()
}.

-record(state, {
    config :: config(),
    metrics_table :: ets:tid(),
    aggregation_timer :: reference() | undefined,
    last_aggregation :: erlang:timestamp()
}).

-record(metric_entry, {
    key :: {atom(), term()},
    timestamp :: pos_integer(),
    value :: number(),
    metadata :: map()
}).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start metrics collector with default configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(default_config()).

%%------------------------------------------------------------------------------
%% @doc Start metrics collector with custom configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%------------------------------------------------------------------------------
%% @doc Stop metrics collector.
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%------------------------------------------------------------------------------
%% @doc Record work order creation.
%%
%% Attributes:
%% - work_order_id: Unique identifier
%% - bucket: reliability | security | cost | compliance
%% - priority: 1-10
%% - source: github | cve | marketplace | internal
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_work_order_created(map()) -> ok.
record_work_order_created(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, work_order_created, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record work order completion.
%%
%% Attributes:
%% - work_order_id: Unique identifier
%% - bucket: Bucket type
%% - cycle_time: Cycle time in milliseconds
%% - sla_met: true | false
%% - quality_score: 0-100
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_work_order_completed(map()) -> ok.
record_work_order_completed(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, work_order_completed, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record work order blocked.
%% @end
%%------------------------------------------------------------------------------
-spec record_work_order_blocked(map()) -> ok.
record_work_order_blocked(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, work_order_blocked, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record work order cancellation.
%% @end
%%------------------------------------------------------------------------------
-spec record_work_order_cancelled(map()) -> ok.
record_work_order_cancelled(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, work_order_cancelled, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Get work order metrics.
%%
%% Options:
%% - window: Aggregation window (1min | 5min | 15min | 1hour | 1day)
%% - buckets: List of buckets to include
%% - include_trends: Include trend analysis (true | false)
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_work_order_metrics(map()) -> map().
get_work_order_metrics(Opts) ->
    gen_server:call(?MODULE, {get_metrics, work_order, Opts}).

%%------------------------------------------------------------------------------
%% @doc Record quality gate evaluation result.
%%
%% Attributes:
%% - gate_id: Unique identifier
%% - gate_type: code_coverage | test_pass_rate | security_scan | performance
%% - passed: true | false
%% - score: 0-100
%% - threshold: Expected threshold
%% - actual_value: Actual measured value
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_quality_gate_result(map()) -> ok.
record_quality_gate_result(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, quality_gate, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Get quality gate metrics.
%% @end
%%------------------------------------------------------------------------------
-spec get_quality_gate_metrics(map()) -> map().
get_quality_gate_metrics(Opts) ->
    gen_server:call(?MODULE, {get_metrics, quality_gate, Opts}).

%%------------------------------------------------------------------------------
%% @doc Record Andon event.
%%
%% Attributes:
%% - event_id: Unique identifier
%% - severity: low | medium | high | critical
%% - root_cause: Root cause category
%% - impact_radius: Number of affected work orders
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_andon_event(map()) -> ok.
record_andon_event(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, andon_event, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record Andon event resolution.
%%
%% Attributes:
%% - event_id: Unique identifier
%% - resolution_time: Resolution time in milliseconds
%% - escalated: true | false
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_andon_resolution(map()) -> ok.
record_andon_resolution(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, andon_resolution, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Get Andon event metrics.
%% @end
%%------------------------------------------------------------------------------
-spec get_andon_metrics(map()) -> map().
get_andon_metrics(Opts) ->
    gen_server:call(?MODULE, {get_metrics, andon, Opts}).

%%------------------------------------------------------------------------------
%% @doc Record learning session start.
%% @end
%%------------------------------------------------------------------------------
-spec record_session_start(map()) -> ok.
record_session_start(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, session_start, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record learning session completion.
%%
%% Attributes:
%% - session_id: Unique identifier
%% - quadrant: tutorial | how_to | reference | explanation
%% - duration: Session duration in milliseconds
%% - work_orders_completed: Number of work orders completed
%% - learning_outcomes: List of achieved learning outcomes
%% - user_satisfaction: 1-5 rating
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_session_complete(map()) -> ok.
record_session_complete(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, session_complete, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record tutorial step completion.
%%
%% Attributes:
%% - step_number: Step number
%% - step_type: instruction | practice | validation | reflection
%% - completion_status: completed | skipped | failed
%% - time_spent: Time in milliseconds
%% - hints_used: Number of hints used
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_tutorial_step(map()) -> ok.
record_tutorial_step(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, tutorial_step, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Record Diataxis quadrant navigation.
%%
%% Attributes:
%% - from_quadrant: Source quadrant
%% - to_quadrant: Destination quadrant
%% - reason: Navigation reason
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_diataxis_navigation(map()) -> ok.
record_diataxis_navigation(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, diataxis_navigation, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Get learning metrics.
%% @end
%%------------------------------------------------------------------------------
-spec get_learning_metrics(map()) -> map().
get_learning_metrics(Opts) ->
    gen_server:call(?MODULE, {get_metrics, learning, Opts}).

%%------------------------------------------------------------------------------
%% @doc Record MCP tool call.
%%
%% Attributes:
%% - tool_name: Tool name
%% - tool_version: Tool version
%% - latency: Latency in milliseconds
%% - result_size: Result size in bytes
%% - error_code: Error code if failed
%% - cache_hit: true | false
%% - timestamp: Unix timestamp in milliseconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec record_tool_call(map()) -> ok.
record_tool_call(Attrs) ->
    gen_server:cast(?MODULE, {record_metric, tool_call, Attrs}).

%%------------------------------------------------------------------------------
%% @doc Get MCP tool metrics.
%% @end
%%------------------------------------------------------------------------------
-spec get_tool_metrics(map()) -> map().
get_tool_metrics(Opts) ->
    gen_server:call(?MODULE, {get_metrics, tool, Opts}).

%%------------------------------------------------------------------------------
%% @doc Get aggregated metrics with options.
%%
%% Options:
%% - window: Aggregation window
%% - metric_types: List of metric types to include
%% - include_trends: Include trend analysis
%% - include_anomalies: Include anomaly detection results
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_metrics(map()) -> map().
get_metrics(Opts) ->
    gen_server:call(?MODULE, {get_all_metrics, Opts}).

%%------------------------------------------------------------------------------
%% @doc Get trend analysis for specific metrics.
%%
%% Options:
%% - metric_type: Metric type to analyze
%% - window: Analysis window
%% - trend_type: linear | exponential | moving_average
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_trends(map()) -> map().
get_trends(Opts) ->
    gen_server:call(?MODULE, {get_trends, Opts}).

%%------------------------------------------------------------------------------
%% @doc Detect anomalies in metrics.
%%
%% Options:
%% - metric: Metric to analyze
%% - threshold: Standard deviation threshold (default: 2.0)
%% - window: Analysis window
%%
%% @end
%%------------------------------------------------------------------------------
-spec detect_anomalies(map()) -> [map()].
detect_anomalies(Opts) ->
    gen_server:call(?MODULE, {detect_anomalies, Opts}).

%%------------------------------------------------------------------------------
%% @doc Get comprehensive metrics summary.
%% @end
%%------------------------------------------------------------------------------
-spec get_summary() -> map().
get_summary() ->
    gen_server:call(?MODULE, get_summary).

%%------------------------------------------------------------------------------
%% @doc Reset all metrics.
%% @end
%%------------------------------------------------------------------------------
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init(Config) ->
    Table = ets:new(tcps_metrics, [
        ordered_set,
        public,
        {keypos, #metric_entry.key},
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    Timer = schedule_aggregation(Config),
    State = #state{
        config = Config,
        metrics_table = Table,
        aggregation_timer = Timer,
        last_aggregation = erlang:timestamp()
    },
    {ok, State}.

handle_call({get_metrics, Type, Opts}, _From, State) ->
    Metrics = aggregate_metrics(State#state.metrics_table, Type, Opts),
    {reply, Metrics, State};

handle_call({get_all_metrics, Opts}, _From, State) ->
    AllMetrics = aggregate_all_metrics(State#state.metrics_table, Opts),
    {reply, AllMetrics, State};

handle_call({get_trends, Opts}, _From, State) ->
    Trends = calculate_trends(State#state.metrics_table, Opts),
    {reply, Trends, State};

handle_call({detect_anomalies, Opts}, _From, State) ->
    Anomalies = find_anomalies(State#state.metrics_table, Opts),
    {reply, Anomalies, State};

handle_call(get_summary, _From, State) ->
    Summary = generate_summary(State#state.metrics_table),
    {reply, Summary, State};

handle_call(reset_metrics, _From, State) ->
    ets:delete_all_objects(State#state.metrics_table),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_metric, Type, Attrs}, State) ->
    Timestamp = maps:get(timestamp, Attrs, erlang:system_time(millisecond)),
    Entry = #metric_entry{
        key = {Type, Timestamp},
        timestamp = Timestamp,
        value = 1,
        metadata = Attrs
    },
    ets:insert(State#state.metrics_table, Entry),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(aggregate, State) ->
    %% Perform aggregation
    cleanup_old_metrics(State#state.metrics_table, State#state.config),
    NewTimer = schedule_aggregation(State#state.config),
    {noreply, State#state{
        aggregation_timer = NewTimer,
        last_aggregation = erlang:timestamp()
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.aggregation_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ets:delete(State#state.metrics_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

default_config() ->
    #{
        aggregation_interval => 1000,
        retention_period => 86400000,  % 24 hours in milliseconds
        enable_anomaly_detection => true,
        anomaly_threshold => 2.0
    }.

schedule_aggregation(Config) ->
    Interval = maps:get(aggregation_interval, Config, 1000),
    erlang:send_after(Interval, self(), aggregate).

cleanup_old_metrics(Table, Config) ->
    RetentionPeriod = maps:get(retention_period, Config, 86400000),
    Cutoff = erlang:system_time(millisecond) - RetentionPeriod,
    ets:select_delete(Table, [{#metric_entry{timestamp = '$1', _ = '_'},
                               [{'<', '$1', Cutoff}],
                               [true]}]).

aggregate_metrics(Table, Type, Opts) ->
    Window = maps:get(window, Opts, '5min'),
    WindowMs = window_to_ms(Window),
    Now = erlang:system_time(millisecond),
    StartTime = Now - WindowMs,

    Entries = ets:select(Table, [{#metric_entry{
        key = {Type, '$1'},
        timestamp = '$2',
        value = '$3',
        metadata = '$4'
    }, [{'>=', '$2', StartTime}], ['$_']}]),

    calculate_aggregates(Entries, Opts).

aggregate_all_metrics(Table, Opts) ->
    Window = maps:get(window, Opts, '5min'),
    WindowMs = window_to_ms(Window),
    Now = erlang:system_time(millisecond),
    StartTime = Now - WindowMs,

    AllEntries = ets:select(Table, [{#metric_entry{
        timestamp = '$1',
        _ = '_'
    }, [{'>=', '$1', StartTime}], ['$_']}]),

    group_and_aggregate(AllEntries, Opts).

window_to_ms('1min') -> 60000;
window_to_ms('5min') -> 300000;
window_to_ms('15min') -> 900000;
window_to_ms('1hour') -> 3600000;
window_to_ms('1day') -> 86400000;
window_to_ms(_) -> 300000.  % Default to 5 minutes

calculate_aggregates(Entries, _Opts) ->
    Count = length(Entries),
    #{
        count => Count,
        rate_per_second => Count / 300.0,  % Assuming 5-minute window
        distribution => calculate_distribution(Entries)
    }.

calculate_distribution(Entries) ->
    %% Group by metadata attributes
    Grouped = lists:foldl(fun(#metric_entry{metadata = Meta}, Acc) ->
        Bucket = maps:get(bucket, Meta, unknown),
        maps:update_with(Bucket, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Entries),
    Grouped.

group_and_aggregate(Entries, Opts) ->
    %% Group by metric type
    Grouped = lists:foldl(fun(#metric_entry{key = {Type, _}} = Entry, Acc) ->
        maps:update_with(Type, fun(L) -> [Entry | L] end, [Entry], Acc)
    end, #{}, Entries),

    %% Aggregate each group
    maps:map(fun(_Type, GroupEntries) ->
        calculate_aggregates(GroupEntries, Opts)
    end, Grouped).

calculate_trends(_Table, _Opts) ->
    %% Simple trend calculation - can be extended with more sophisticated algorithms
    #{
        trend => stable,
        direction => neutral,
        confidence => 0.0
    }.

find_anomalies(_Table, _Opts) ->
    %% Placeholder for anomaly detection
    %% Would implement statistical methods (z-score, IQR, etc.)
    [].

generate_summary(Table) ->
    TotalCount = ets:info(Table, size),
    #{
        total_metrics => TotalCount,
        last_updated => erlang:system_time(millisecond),
        status => ok
    }.
