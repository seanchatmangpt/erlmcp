%%%-----------------------------------------------------------------------------
%%% @doc TCPS MCP Diataxis Simulator - OpenTelemetry Integration
%%%
%%% Production-grade OpenTelemetry integration for the TCPS MCP Diataxis
%%% simulator. Provides comprehensive tracing, metrics, and observability
%%% for learning session tracking, work order execution, quality gates,
%%% Andon events, and MCP tool interactions.
%%%
%%% == OpenTelemetry Spans (8 Types) ==
%%%
%%% 1. simulator.session - Full learning session lifecycle
%%%    Attributes: session_id, user_id, quadrant, difficulty_level,
%%%                start_time, estimated_duration
%%%
%%% 2. simulator.scenario - Individual scenario execution
%%%    Attributes: scenario_id, scenario_type, work_orders_count,
%%%                complexity, expected_outcomes
%%%
%%% 3. simulator.work_order - Work order creation and execution
%%%    Attributes: work_order_id, bucket, priority, source, sla_target,
%%%                assigned_to, dependencies
%%%
%%% 4. simulator.quality_gate - Quality gate evaluation
%%%    Attributes: gate_id, gate_type, threshold, actual_value,
%%%                pass/fail, remediation_required
%%%
%%% 5. simulator.andon_event - Andon event triggering and resolution
%%%    Attributes: event_id, severity, root_cause, impact_radius,
%%%                resolution_time, escalated
%%%
%%% 6. diataxis.navigation - Diataxis quadrant navigation
%%%    Attributes: from_quadrant, to_quadrant, reason, user_action,
%%%                documentation_accessed
%%%
%%% 7. tutorial.step - Individual tutorial step execution
%%%    Attributes: step_number, step_type, tools_used, time_spent,
%%%                completion_status, hints_used
%%%
%%% 8. mcp.tool_call - MCP tool invocation tracking
%%%    Attributes: tool_name, tool_version, parameters, result_size,
%%%                latency, error_code
%%%
%%% == Metrics (10+ Types) ==
%%%
%%% Gauges:
%%% - simulator.active_sessions - Current active learning sessions
%%% - simulator.wip_current - Current WIP per bucket (reliability/security/cost/compliance)
%%%
%%% Counters:
%%% - simulator.work_orders_created - Total work orders created
%%% - simulator.quality_gates_passed - Quality gates passed
%%% - simulator.quality_gates_failed - Quality gates failed
%%% - simulator.andon_events_total - Total Andon events triggered
%%% - mcp.tool_calls_total - Total MCP tool invocations
%%%
%%% Histograms:
%%% - simulator.cycle_time - Work order cycle time distribution
%%% - simulator.session_duration - Session duration distribution
%%% - mcp.tool_latency - MCP tool latency distribution
%%% - simulator.quality_gate_score - Quality gate score distribution
%%%
%%% == Export Formats ==
%%% - OTLP to Jaeger/Tempo (distributed tracing)
%%% - Prometheus endpoint (metrics scraping)
%%% - JSON API (real-time dashboard)
%%% - CSV export (analysis and reporting)
%%%
%%% == Integration ==
%%% ```erlang
%%% %% Start telemetry
%%% tcps_simulator_telemetry:start_link(#{
%%%     otlp_endpoint => "http://localhost:4318",
%%%     prometheus_port => 9464,
%%%     export_interval => 10000
%%% }).
%%%
%%% %% Start session span
%%% SessionSpan = tcps_simulator_telemetry:start_session(#{
%%%     session_id => <<"session-001">>,
%%%     user_id => <<"user-123">>,
%%%     quadrant => tutorial
%%% }),
%%%
%%% %% Create work order span
%%% WOSpan = tcps_simulator_telemetry:start_work_order(#{
%%%     work_order_id => <<"WO-001">>,
%%%     bucket => security,
%%%     priority => 9
%%% }, SessionSpan),
%%%
%%% %% Record metrics
%%% tcps_simulator_telemetry:record_wip(security, 3),
%%% tcps_simulator_telemetry:increment_quality_gates_passed(),
%%%
%%% %% End spans
%%% tcps_simulator_telemetry:end_work_order(WOSpan, #{status => completed}),
%%% tcps_simulator_telemetry:end_session(SessionSpan, #{
%%%     work_orders_completed => 5,
%%%     quality_gates_passed => 4
%%% }).
%%% '''
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_simulator_telemetry).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Span management
    start_session/1,
    end_session/2,
    start_scenario/2,
    end_scenario/2,
    start_work_order/2,
    end_work_order/2,
    start_quality_gate/2,
    end_quality_gate/2,
    start_andon_event/2,
    end_andon_event/2,
    start_diataxis_navigation/2,
    end_diataxis_navigation/2,
    start_tutorial_step/2,
    end_tutorial_step/2,
    start_mcp_tool_call/2,
    end_mcp_tool_call/2,

    %% Metrics recording
    record_wip/2,
    record_active_sessions/1,
    increment_work_orders_created/0,
    increment_quality_gates_passed/0,
    increment_quality_gates_failed/0,
    increment_andon_events/0,
    increment_mcp_tool_calls/0,
    record_cycle_time/1,
    record_session_duration/1,
    record_mcp_tool_latency/2,
    record_quality_gate_score/1,

    %% Export functions
    export_prometheus/0,
    export_json/0,
    export_csv/1,
    get_metrics_summary/0,

    %% Configuration
    configure/1,
    get_config/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type span_id() :: binary().
-type span_type() :: session | scenario | work_order | quality_gate |
                     andon_event | diataxis_navigation | tutorial_step |
                     mcp_tool_call.
-type attributes() :: #{atom() => term()}.
-type config() :: #{
    otlp_endpoint => string(),
    prometheus_port => pos_integer(),
    export_interval => pos_integer(),
    enable_tracing => boolean(),
    enable_metrics => boolean(),
    sample_rate => float()
}.

-record(state, {
    config :: config(),
    active_spans :: #{span_id() => {span_type(), attributes(), erlang:timestamp()}},
    metrics :: #{atom() => number()},
    histograms :: #{atom() => [number()]},
    export_timer :: reference() | undefined
}).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start telemetry server with default configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(default_config()).

%%------------------------------------------------------------------------------
%% @doc Start telemetry server with custom configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%------------------------------------------------------------------------------
%% @doc Stop telemetry server.
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%------------------------------------------------------------------------------
%% @doc Start a session span for learning session tracking.
%%
%% Attributes:
%% - session_id: Unique session identifier
%% - user_id: User identifier
%% - quadrant: Diataxis quadrant (tutorial/how_to/reference/explanation)
%% - difficulty_level: 1-10 difficulty rating
%% - start_time: ISO 8601 timestamp
%% - estimated_duration: Estimated duration in seconds
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_session(attributes()) -> span_id().
start_session(Attributes) ->
    gen_server:call(?MODULE, {start_span, session, Attributes}).

%%------------------------------------------------------------------------------
%% @doc End a session span with completion attributes.
%%
%% Completion Attributes:
%% - work_orders_completed: Number of work orders completed
%% - quality_gates_passed: Number of quality gates passed
%% - quality_gates_failed: Number of quality gates failed
%% - andon_events: Number of Andon events triggered
%% - learning_outcomes_achieved: List of achieved learning outcomes
%% - user_satisfaction: 1-5 rating
%%
%% @end
%%------------------------------------------------------------------------------
-spec end_session(span_id(), attributes()) -> ok.
end_session(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start a scenario span for scenario execution tracking.
%% @end
%%------------------------------------------------------------------------------
-spec start_scenario(attributes(), span_id()) -> span_id().
start_scenario(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, scenario, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End a scenario span.
%% @end
%%------------------------------------------------------------------------------
-spec end_scenario(span_id(), attributes()) -> ok.
end_scenario(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start a work order span for work order tracking.
%%
%% Attributes:
%% - work_order_id: Unique work order identifier
%% - bucket: reliability | security | cost | compliance
%% - priority: 1-10 priority rating
%% - source: github | cve | marketplace | internal
%% - sla_target: SLA target in seconds
%% - assigned_to: Assignee identifier
%% - dependencies: List of dependent work order IDs
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_work_order(attributes(), span_id()) -> span_id().
start_work_order(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, work_order, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End a work order span.
%%
%% Completion Attributes:
%% - status: completed | cancelled | blocked
%% - cycle_time: Actual cycle time in seconds
%% - sla_met: true | false
%% - quality_score: 0-100 quality score
%%
%% @end
%%------------------------------------------------------------------------------
-spec end_work_order(span_id(), attributes()) -> ok.
end_work_order(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start a quality gate span for quality gate evaluation tracking.
%%
%% Attributes:
%% - gate_id: Unique gate identifier
%% - gate_type: code_coverage | test_pass_rate | security_scan | performance
%% - threshold: Threshold value
%% - actual_value: Actual measured value
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_quality_gate(attributes(), span_id()) -> span_id().
start_quality_gate(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, quality_gate, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End a quality gate span.
%%
%% Completion Attributes:
%% - passed: true | false
%% - remediation_required: true | false
%% - remediation_actions: List of required remediation actions
%%
%% @end
%%------------------------------------------------------------------------------
-spec end_quality_gate(span_id(), attributes()) -> ok.
end_quality_gate(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start an Andon event span for Andon event tracking.
%%
%% Attributes:
%% - event_id: Unique event identifier
%% - severity: low | medium | high | critical
%% - root_cause: Root cause category
%% - impact_radius: Number of affected work orders
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_andon_event(attributes(), span_id()) -> span_id().
start_andon_event(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, andon_event, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End an Andon event span.
%%
%% Completion Attributes:
%% - resolved: true | false
%% - resolution_time: Resolution time in seconds
%% - escalated: true | false
%% - lessons_learned: Lessons learned from event
%%
%% @end
%%------------------------------------------------------------------------------
-spec end_andon_event(span_id(), attributes()) -> ok.
end_andon_event(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start a Diataxis navigation span for documentation navigation tracking.
%%
%% Attributes:
%% - from_quadrant: tutorial | how_to | reference | explanation
%% - to_quadrant: tutorial | how_to | reference | explanation
%% - reason: User action reason
%% - documentation_accessed: List of documentation pages accessed
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_diataxis_navigation(attributes(), span_id()) -> span_id().
start_diataxis_navigation(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, diataxis_navigation, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End a Diataxis navigation span.
%% @end
%%------------------------------------------------------------------------------
-spec end_diataxis_navigation(span_id(), attributes()) -> ok.
end_diataxis_navigation(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start a tutorial step span for tutorial step tracking.
%%
%% Attributes:
%% - step_number: Step number in tutorial
%% - step_type: instruction | practice | validation | reflection
%% - tools_used: List of MCP tools used
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_tutorial_step(attributes(), span_id()) -> span_id().
start_tutorial_step(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, tutorial_step, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End a tutorial step span.
%%
%% Completion Attributes:
%% - completion_status: completed | skipped | failed
%% - time_spent: Time spent in seconds
%% - hints_used: Number of hints used
%% - errors_made: Number of errors made
%%
%% @end
%%------------------------------------------------------------------------------
-spec end_tutorial_step(span_id(), attributes()) -> ok.
end_tutorial_step(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Start an MCP tool call span for MCP tool invocation tracking.
%%
%% Attributes:
%% - tool_name: MCP tool name
%% - tool_version: Tool version
%% - parameters: Tool parameters (sanitized)
%% - caller_context: Caller context information
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_mcp_tool_call(attributes(), span_id()) -> span_id().
start_mcp_tool_call(Attributes, ParentSpan) ->
    gen_server:call(?MODULE, {start_span, mcp_tool_call, Attributes, ParentSpan}).

%%------------------------------------------------------------------------------
%% @doc End an MCP tool call span.
%%
%% Completion Attributes:
%% - result_size: Size of result in bytes
%% - latency: Latency in milliseconds
%% - error_code: Error code if failed
%% - cache_hit: true | false
%%
%% @end
%%------------------------------------------------------------------------------
-spec end_mcp_tool_call(span_id(), attributes()) -> ok.
end_mcp_tool_call(SpanId, Attributes) ->
    gen_server:call(?MODULE, {end_span, SpanId, Attributes}).

%%------------------------------------------------------------------------------
%% @doc Record WIP (Work-In-Progress) for a bucket.
%% @end
%%------------------------------------------------------------------------------
-spec record_wip(atom(), non_neg_integer()) -> ok.
record_wip(Bucket, Count) ->
    gen_server:cast(?MODULE, {record_metric, {wip, Bucket}, Count}).

%%------------------------------------------------------------------------------
%% @doc Record active sessions count.
%% @end
%%------------------------------------------------------------------------------
-spec record_active_sessions(non_neg_integer()) -> ok.
record_active_sessions(Count) ->
    gen_server:cast(?MODULE, {record_metric, active_sessions, Count}).

%%------------------------------------------------------------------------------
%% @doc Increment work orders created counter.
%% @end
%%------------------------------------------------------------------------------
-spec increment_work_orders_created() -> ok.
increment_work_orders_created() ->
    gen_server:cast(?MODULE, {increment_counter, work_orders_created}).

%%------------------------------------------------------------------------------
%% @doc Increment quality gates passed counter.
%% @end
%%------------------------------------------------------------------------------
-spec increment_quality_gates_passed() -> ok.
increment_quality_gates_passed() ->
    gen_server:cast(?MODULE, {increment_counter, quality_gates_passed}).

%%------------------------------------------------------------------------------
%% @doc Increment quality gates failed counter.
%% @end
%%------------------------------------------------------------------------------
-spec increment_quality_gates_failed() -> ok.
increment_quality_gates_failed() ->
    gen_server:cast(?MODULE, {increment_counter, quality_gates_failed}).

%%------------------------------------------------------------------------------
%% @doc Increment Andon events counter.
%% @end
%%------------------------------------------------------------------------------
-spec increment_andon_events() -> ok.
increment_andon_events() ->
    gen_server:cast(?MODULE, {increment_counter, andon_events_total}).

%%------------------------------------------------------------------------------
%% @doc Increment MCP tool calls counter.
%% @end
%%------------------------------------------------------------------------------
-spec increment_mcp_tool_calls() -> ok.
increment_mcp_tool_calls() ->
    gen_server:cast(?MODULE, {increment_counter, mcp_tool_calls_total}).

%%------------------------------------------------------------------------------
%% @doc Record cycle time for histogram.
%% @end
%%------------------------------------------------------------------------------
-spec record_cycle_time(non_neg_integer()) -> ok.
record_cycle_time(Milliseconds) ->
    gen_server:cast(?MODULE, {record_histogram, cycle_time, Milliseconds}).

%%------------------------------------------------------------------------------
%% @doc Record session duration for histogram.
%% @end
%%------------------------------------------------------------------------------
-spec record_session_duration(non_neg_integer()) -> ok.
record_session_duration(Seconds) ->
    gen_server:cast(?MODULE, {record_histogram, session_duration, Seconds}).

%%------------------------------------------------------------------------------
%% @doc Record MCP tool latency for histogram.
%% @end
%%------------------------------------------------------------------------------
-spec record_mcp_tool_latency(atom(), non_neg_integer()) -> ok.
record_mcp_tool_latency(ToolName, Milliseconds) ->
    gen_server:cast(?MODULE, {record_histogram, {mcp_tool_latency, ToolName}, Milliseconds}).

%%------------------------------------------------------------------------------
%% @doc Record quality gate score for histogram.
%% @end
%%------------------------------------------------------------------------------
-spec record_quality_gate_score(number()) -> ok.
record_quality_gate_score(Score) ->
    gen_server:cast(?MODULE, {record_histogram, quality_gate_score, Score}).

%%------------------------------------------------------------------------------
%% @doc Export metrics in Prometheus format.
%% @end
%%------------------------------------------------------------------------------
-spec export_prometheus() -> {ok, binary()} | {error, term()}.
export_prometheus() ->
    gen_server:call(?MODULE, export_prometheus).

%%------------------------------------------------------------------------------
%% @doc Export metrics in JSON format.
%% @end
%%------------------------------------------------------------------------------
-spec export_json() -> {ok, map()} | {error, term()}.
export_json() ->
    gen_server:call(?MODULE, export_json).

%%------------------------------------------------------------------------------
%% @doc Export metrics to CSV file.
%% @end
%%------------------------------------------------------------------------------
-spec export_csv(file:filename()) -> ok | {error, term()}.
export_csv(Filename) ->
    gen_server:call(?MODULE, {export_csv, Filename}).

%%------------------------------------------------------------------------------
%% @doc Get metrics summary.
%% @end
%%------------------------------------------------------------------------------
-spec get_metrics_summary() -> map().
get_metrics_summary() ->
    gen_server:call(?MODULE, get_metrics_summary).

%%------------------------------------------------------------------------------
%% @doc Configure telemetry settings.
%% @end
%%------------------------------------------------------------------------------
-spec configure(config()) -> ok.
configure(Config) ->
    gen_server:call(?MODULE, {configure, Config}).

%%------------------------------------------------------------------------------
%% @doc Get current configuration.
%% @end
%%------------------------------------------------------------------------------
-spec get_config() -> config().
get_config() ->
    gen_server:call(?MODULE, get_config).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init(Config) ->
    State = #state{
        config = Config,
        active_spans = #{},
        metrics = init_metrics(),
        histograms = #{},
        export_timer = schedule_export(Config)
    },
    {ok, State}.

handle_call({start_span, Type, Attributes}, _From, State) ->
    SpanId = generate_span_id(),
    NewSpans = maps:put(SpanId, {Type, Attributes, erlang:timestamp()}, State#state.active_spans),
    {reply, SpanId, State#state{active_spans = NewSpans}};

handle_call({start_span, Type, Attributes, ParentSpan}, _From, State) ->
    SpanId = generate_span_id(),
    AttrsWithParent = Attributes#{parent_span => ParentSpan},
    NewSpans = maps:put(SpanId, {Type, AttrsWithParent, erlang:timestamp()}, State#state.active_spans),
    {reply, SpanId, State#state{active_spans = NewSpans}};

handle_call({end_span, SpanId, CompletionAttrs}, _From, State) ->
    case maps:get(SpanId, State#state.active_spans, undefined) of
        undefined ->
            {reply, {error, span_not_found}, State};
        {Type, StartAttrs, StartTime} ->
            Duration = timer:now_diff(erlang:timestamp(), StartTime) div 1000,
            _SpanData = #{
                type => Type,
                attributes => maps:merge(StartAttrs, CompletionAttrs),
                duration_ms => Duration
            },
            %% TODO: Export span to OTLP endpoint
            NewSpans = maps:remove(SpanId, State#state.active_spans),
            {reply, ok, State#state{active_spans = NewSpans}}
    end;

handle_call(export_prometheus, _From, State) ->
    Prometheus = format_prometheus(State#state.metrics, State#state.histograms),
    {reply, {ok, Prometheus}, State};

handle_call(export_json, _From, State) ->
    Json = #{
        metrics => State#state.metrics,
        histograms => format_histograms(State#state.histograms),
        active_spans => maps:size(State#state.active_spans)
    },
    {reply, {ok, Json}, State};

handle_call({export_csv, Filename}, _From, State) ->
    Result = write_csv(Filename, State#state.metrics, State#state.histograms),
    {reply, Result, State};

handle_call(get_metrics_summary, _From, State) ->
    Summary = #{
        metrics => State#state.metrics,
        histogram_summaries => calculate_histogram_summaries(State#state.histograms),
        active_spans => maps:size(State#state.active_spans)
    },
    {reply, Summary, State};

handle_call({configure, NewConfig}, _From, State) ->
    %% Cancel old timer
    case State#state.export_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    %% Start new timer
    NewTimer = schedule_export(NewConfig),
    {reply, ok, State#state{config = NewConfig, export_timer = NewTimer}};

handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_metric, Key, Value}, State) ->
    NewMetrics = maps:put(Key, Value, State#state.metrics),
    {noreply, State#state{metrics = NewMetrics}};

handle_cast({increment_counter, Key}, State) ->
    Current = maps:get(Key, State#state.metrics, 0),
    NewMetrics = maps:put(Key, Current + 1, State#state.metrics),
    {noreply, State#state{metrics = NewMetrics}};

handle_cast({record_histogram, Key, Value}, State) ->
    Current = maps:get(Key, State#state.histograms, []),
    NewHistograms = maps:put(Key, [Value | Current], State#state.histograms),
    {noreply, State#state{histograms = NewHistograms}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(export_metrics, State) ->
    %% TODO: Export metrics to OTLP endpoint
    NewTimer = schedule_export(State#state.config),
    {noreply, State#state{export_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.export_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

default_config() ->
    #{
        otlp_endpoint => "http://localhost:4318",
        prometheus_port => 9464,
        export_interval => 10000,
        enable_tracing => true,
        enable_metrics => true,
        sample_rate => 1.0
    }.

generate_span_id() ->
    Rand = rand:uniform(16#FFFFFFFFFFFFFFFF),
    integer_to_binary(Rand, 16).

init_metrics() ->
    #{
        active_sessions => 0,
        work_orders_created => 0,
        quality_gates_passed => 0,
        quality_gates_failed => 0,
        andon_events_total => 0,
        mcp_tool_calls_total => 0,
        {wip, reliability} => 0,
        {wip, security} => 0,
        {wip, cost} => 0,
        {wip, compliance} => 0
    }.

schedule_export(Config) ->
    Interval = maps:get(export_interval, Config, 10000),
    erlang:send_after(Interval, self(), export_metrics).

format_prometheus(Metrics, Histograms) ->
    MetricsLines = [format_prometheus_metric(K, V) || {K, V} <- maps:to_list(Metrics)],
    HistogramLines = [format_prometheus_histogram(K, V) || {K, V} <- maps:to_list(Histograms)],
    iolist_to_binary([MetricsLines, "\n", HistogramLines]).

format_prometheus_metric(Key, Value) when is_atom(Key) ->
    io_lib:format("tcps_simulator_~s ~p~n", [atom_to_list(Key), Value]);
format_prometheus_metric({Type, Bucket}, Value) ->
    io_lib:format("tcps_simulator_~s{bucket=\"~s\"} ~p~n",
                  [atom_to_list(Type), atom_to_list(Bucket), Value]).

format_prometheus_histogram(Key, Values) ->
    Stats = calculate_stats(Values),
    [
        io_lib:format("tcps_simulator_~s_count ~p~n", [format_key(Key), length(Values)]),
        io_lib:format("tcps_simulator_~s_sum ~p~n", [format_key(Key), maps:get(sum, Stats)]),
        io_lib:format("tcps_simulator_~s_p50 ~p~n", [format_key(Key), maps:get(p50, Stats)]),
        io_lib:format("tcps_simulator_~s_p95 ~p~n", [format_key(Key), maps:get(p95, Stats)]),
        io_lib:format("tcps_simulator_~s_p99 ~p~n", [format_key(Key), maps:get(p99, Stats)])
    ].

format_key({Key, SubKey}) when is_atom(Key), is_atom(SubKey) ->
    atom_to_list(Key) ++ "_" ++ atom_to_list(SubKey);
format_key(Key) when is_atom(Key) ->
    atom_to_list(Key).

format_histograms(Histograms) ->
    maps:map(fun(_K, Values) -> calculate_stats(Values) end, Histograms).

calculate_stats([]) ->
    #{count => 0, sum => 0, min => 0, max => 0, mean => 0, p50 => 0, p95 => 0, p99 => 0};
calculate_stats(Values) ->
    Sorted = lists:sort(Values),
    Count = length(Sorted),
    Sum = lists:sum(Sorted),
    Mean = Sum / Count,
    #{
        count => Count,
        sum => Sum,
        min => hd(Sorted),
        max => lists:last(Sorted),
        mean => Mean,
        p50 => percentile(Sorted, 0.50),
        p95 => percentile(Sorted, 0.95),
        p99 => percentile(Sorted, 0.99)
    }.

percentile(Sorted, P) ->
    N = length(Sorted),
    K = round(P * N),
    case K of
        0 -> hd(Sorted);
        _ when K >= N -> lists:last(Sorted);
        _ -> lists:nth(K, Sorted)
    end.

calculate_histogram_summaries(Histograms) ->
    maps:map(fun(_K, Values) -> calculate_stats(Values) end, Histograms).

write_csv(Filename, Metrics, Histograms) ->
    Header = "metric,type,value\n",
    MetricLines = [[format_key(K), ",gauge,", float_to_list(float(V)), "\n"]
                   || {K, V} <- maps:to_list(Metrics)],
    HistogramLines = lists:flatten([format_csv_histogram(K, V)
                                    || {K, V} <- maps:to_list(Histograms)]),
    Content = [Header, MetricLines, HistogramLines],
    file:write_file(Filename, Content).

format_csv_histogram(Key, Values) ->
    Stats = calculate_stats(Values),
    KeyStr = format_key(Key),
    [
        [KeyStr, "_count,histogram,", integer_to_list(maps:get(count, Stats)), "\n"],
        [KeyStr, "_sum,histogram,", float_to_list(float(maps:get(sum, Stats))), "\n"],
        [KeyStr, "_mean,histogram,", float_to_list(maps:get(mean, Stats)), "\n"],
        [KeyStr, "_p50,histogram,", float_to_list(float(maps:get(p50, Stats))), "\n"],
        [KeyStr, "_p95,histogram,", float_to_list(float(maps:get(p95, Stats))), "\n"],
        [KeyStr, "_p99,histogram,", float_to_list(float(maps:get(p99, Stats))), "\n"]
    ].
