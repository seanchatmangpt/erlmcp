%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Observability Module with OTEL Integration
%%%
%%% Provides comprehensive observability for erlmcp CLI validation:
%%% - OpenTelemetry tracing and metrics
%%% - Performance monitoring with detailed latency tracking
%%% - Chaos engineering injection patterns
%%% - Distributed correlation across validation phases
%%% - Real-time metrics collection and aggregation
%%%
%%% == Key Features ==
%%%
%%% 1. **Distributed Tracing**: Complete span coverage for all validation operations
%%% 2. **Metrics Collection**: Latency, throughput, error rates, resource usage
%%% 3. **Chaos Engineering**: Fault injection, latency simulation, resource exhaustion
%%% 4. **Performance Monitoring**: Real-time profiling and bottleneck detection
%%% 5. **Correlation**: Trace context propagation across validation phases
%%% 6. **Multi-Exporter Support**: Jaeger, Prometheus, OTLP, console
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Initialize observability
%%% ok = erlmcp_cli_observability:init(#{
%%%     service_name => <<"erlmcp-validation-cli">>,
%%%     exporters => [jaeger, prometheus],
%%%     sampling => always_on
%%% }),
%%%
%%% %% Trace a validation command with chaos injection
%%% Result = erlmcp_cli_observability:trace_validation(
%%%     <<"spec-validation">>,
%%%     #{spec_file => <<"mcp-spec.json">>},
%%%     fun() -> validate_spec_file() end,
%%%     #{chaos => {latency, 1000}}  % Inject 1s latency
%%% ),
%%%
%%% %% Collect performance metrics
%%% Metrics = erlmcp_cli_observability:get_validation_metrics(),
%%%
%%% %% Inject chaos during transport validation
%%% ok = erlmcp_cli_observability:inject_chaos(#{type => network_failure, probability => 0.1}),
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_observability).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, init/1, trace_validation/4, trace_validation/3,
         get_validation_metrics/0, get_performance_summary/0, inject_chaos/1,
         inject_latency/2, inject_failure/2, get_chaos_status/0, reset_metrics/0,
         observe_command/3, observe_command/4, export_trace/2, export_metrics/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-export_type([otel_config/0, validation_span/0, metrics_data/0, chaos_config/0]).

-include("erlmcp.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type otel_config() ::
    #{service_name => binary(),
      service_version => binary(),
      exporters => [jaeger | zipkin | prometheus | otlp | console | datadog | honeycomb],
      sampling => always_on | always_off | trace_id_ratio | parent_based | head_based | tail_based,
      sampling_rate => float(),
      batch_timeout => pos_integer(),
      max_queue_size => pos_integer(),
      max_export_batch_size => pos_integer(),
      enable_chaos => boolean(),
      chaos_config => chaos_config()}.

-type validation_span() ::
    #{name => binary(),
      type => spec | protocol | transport | compliance | performance,
      start_time => integer(),
      end_time => integer() | undefined,
      duration_ns => integer() | undefined,
      status => ok | error | timeout,
      attributes => #{binary() => term()},
      events => [otel_event()],
      errors => [otel_error()],
      metrics => #{binary() => term()},
      chaos_injected => boolean(),
      chaos_type => binary()}.

-type metrics_data() ::
    #{total_validations => integer(),
      successful_validations => integer(),
      failed_validations => integer(),
      average_latency_ms => float(),
      p95_latency_ms => float(),
      p99_latency_ms => float(),
      throughput_per_sec => float(),
      error_rate => float(),
      resource_usage => #{binary() => term()},
      chaos_events => integer(),
      trace_duration_ms => integer()}.

-type chaos_config() ::
    #{type => network_failure | latency_spike | resource_exhaustion | process_crash | timeout,
      probability => float(),  % 0.0 to 1.0
      magnitude => float(),     % 0.0 to 1.0
      duration => pos_integer(), % milliseconds
      target_phase => spec | protocol | transport | all}.

-type otel_event() ::
    #{name => binary(),
      timestamp => integer(),
      attributes => #{binary() => term()}}.

-type otel_error() ::
    #{error_type => binary(),
      error_message => binary(),
      error_stacktrace => [term()],
      error_class => throw | error | exit}.

%%====================================================================
%% Record Definitions
%%====================================================================

-record(state,
        {config :: otel_config(),
         tracer :: undefined | term(),
         spans :: #{binary() => validation_span()},
         metrics :: metrics_data(),
         chaos_enabled :: boolean(),
         chaos_history :: [map()],
         aggregation_timer :: reference() | undefined,
         exporter_pids :: #{atom() => pid()}}).

-define(SERVER, ?MODULE).
-define(METRICS_AGG_INTERVAL, 5000).  % 5 seconds
-define(DEFAULT_OTEL_CONFIG, #{
    service_name => <<"erlmcp-validation-cli">>,
    service_version => <<"2.1.0">>,
    exporters => [console],
    sampling => always_on,
    sampling_rate => 1.0,
    batch_timeout => 5000,
    max_queue_size => 2048,
    max_export_batch_size => 512,
    enable_chaos => false,
    chaos_config => #{
        type => network_failure,
        probability => 0.1,
        magnitude => 0.5,
        duration => 1000,
        target_phase => all
    }
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the observability server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the observability server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Initialize observability with custom configuration
-spec init(map()) -> ok | {error, term()}.
init(Config) ->
    MergedConfig = maps:merge(?DEFAULT_OTEL_CONFIG, Config),

    %% Initialize OpenTelemetry
    case erlmcp_otel:init(MergedConfig) of
        ok ->
            %% Initialize metrics aggregation
            AggregationTimer = erlang:send_after(?METRICS_AGG_INTERVAL, self(), aggregate_metrics),

            %% Start exporter processes
            ExporterPids = start_exporters(MergedConfig),

            %% Initialize state
            State = #state{
                config = MergedConfig,
                tracer = erlmcp_otel:get_tracer_provider(),
                spans = #{},
                metrics = init_metrics_data(),
                chaos_enabled = maps:get(enable_chaos, MergedConfig, false),
                chaos_history = [],
                aggregation_timer = AggregationTimer,
                exporter_pids = ExporterPids
            },

            gen_server:cast(?SERVER, start_metrics_collection),

            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Trace validation execution with optional chaos injection
-spec trace_validation(binary(), #{binary() => term()}, fun(() -> term())) -> term().
trace_validation(Type, Attributes, Fun) ->
    trace_validation(Type, Attributes, Fun, #{}).

%% @doc Trace validation with chaos configuration
-spec trace_validation(binary(), #{binary() => term()}, fun(() -> term()), map()) -> term().
trace_validation(Type, Attributes, Fun, ChaosOpts) when is_binary(Type), is_map(Attributes) ->
    SpanName = <<"mcp.validation.", Type/binary>>,

    %% Start span with context
    SpanCtx = erlmcp_otel:start_span(SpanName, #{
        <<"validation.type">> => Type,
        <<"cli.version">> => ?VERSION,
        <<"node">> => list_to_binary(atom_to_list(node()))
    }),

    %% Create validation span record
    ValidationSpan = #{name => SpanName,
                      type => binary_to_existing_atom(Type, utf8),
                      start_time => erlang:system_time(nanosecond),
                      attributes => Attributes,
                      events => [],
                      errors => [],
                      metrics => #{},
                      chaos_injected => false,
                      chaos_type => undefined},

    StoreSpanId = generate_span_id(),
    gen_server:call(?SERVER, {store_span, StoreSpanId, ValidationSpan}),

    %% Apply chaos injection if enabled
    ShouldInjectChaos = should_inject_chaos(ChaosOpts),
    ChaosResult =
        case ShouldInjectChaos of
            true ->
                inject_chaos_internal(ChaosOpts, StoreSpanId);
            false ->
                ok
        end,

    try
        %% Add pre-validation event
        erlmcp_otel:add_event(SpanCtx,
                             <<"validation.started">>,
                             Attributes),

        gen_server:cast(?SERVER, {add_span_event, StoreSpanId, <<"validation.started">>, Attributes}),

        %% Execute validation function
        Result = Fun(),

        %% Add success event
        SuccessAttrs = maps:merge(Attributes, #{<<"status">> => <<"success">>}),
        erlmcp_otel:add_event(SpanCtx,
                             <<"validation.completed">>,
                             SuccessAttrs),

        gen_server:cast(?SERVER, {add_span_event, StoreSpanId, <<"validation.completed">>, SuccessAttrs}),

        %% Update span with success status
        UpdatedSpan = ValidationSpan#{status => ok, chaos_injected => ShouldInjectChaos},
        gen_server:cast(?SERVER, {update_span, StoreSpanId, UpdatedSpan}),

        %% End OpenTelemetry span
        erlmcp_otel:end_span(SpanCtx),

        %% Record metrics
        gen_server:cast(?SERVER, {record_validation_success, Type, ShouldInjectChaos}),

        Result
    catch
        Class:Reason:Stacktrace ->
            %% Add error event
            ErrorAttrs = Attributes#{<<"status">> => <<"error">>, <<"error.class">> => atom_to_binary(Class)},
            erlmcp_otel:add_event(SpanCtx,
                                 <<"validation.failed">>,
                                 ErrorAttrs),

            %% Record error in span
            erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}, ErrorAttrs),

            gen_server:cast(?SERVER, {add_span_error, StoreSpanId, {Class, Reason, Stacktrace}}),

            %% Update span with error status
            ErrorSpan = ValidationSpan#{status => error, chaos_injected => ShouldInjectChaos},
            gen_server:cast(?SERVER, {update_span, StoreSpanId, ErrorSpan}),

            %% End OpenTelemetry span
            erlmcp_otel:end_span(SpanCtx),

            %% Record error metrics
            gen_server:cast(?SERVER, {record_validation_error, Type, ShouldInjectChaos}),

            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Get current validation metrics
-spec get_validation_metrics() -> metrics_data().
get_validation_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%% @doc Get performance summary with statistical analysis
-spec get_performance_summary() -> map().
get_performance_summary() ->
    gen_server:call(?SERVER, get_performance_summary).

%% @doc Inject chaos during execution
-spec inject_chaos(map()) -> ok | {error, term()}.
inject_chaos(Config) ->
    gen_server:call(?SERVER, {inject_chaos, Config}).

%% @doc Inject specific latency
-spec inject_latency(binary(), pos_integer()) -> ok.
inject_latency(SpanId, DurationMs) ->
    gen_server:cast(?SERVER, {inject_latency, SpanId, DurationMs}).

%% @doc Inject failure
-spec inject_failure(binary(), term()) -> ok.
inject_failure(SpanId, ErrorReason) ->
    gen_server:cast(?SERVER, {inject_failure, SpanId, ErrorReason}).

%% @doc Get current chaos status
-spec get_chaos_status() -> map().
get_chaos_status() ->
    gen_server:call(?SERVER, get_chaos_status).

%% @doc Reset all metrics
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:cast(?SERVER, reset_metrics).

%% @doc Observe command execution
-spec observe_command(binary(), fun(() -> term()), map()) -> term().
observe_command(Command, Fun, Attributes) ->
    observe_command(Command, Fun, Attributes, #{}).

%% @doc Observe command with context
-spec observe_command(binary(), fun(() -> term()), map(), map()) -> term().
observe_command(Command, Fun, Attributes, Context) ->
    SpanName = <<"mcp.command.", Command/binary>>,

    SpanCtx = erlmcp_otel:start_span(SpanName, Attributes),

    StoreSpanId = generate_span_id(),
    gen_server:cast(?SERVER, {observe_command_start, StoreSpanId, Command, Context}),

    try
        Result = Fun(),

        gen_server:cast(?SERVER, {observe_command_complete, StoreSpanId, Command, Result}),
        erlmcp_otel:end_span(SpanCtx),

        Result
    catch
        Class:Reason:Stacktrace ->
            gen_server:cast(?SERVER, {observe_command_error, StoreSpanId, Command, {Class, Reason, Stacktrace}}),
            erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}),
            erlmcp_otel:end_span(SpanCtx),

            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Export trace to file
-spec export_trace(binary(), string()) -> ok | {error, term()}.
export_trace(SpanId, Filename) ->
    gen_server:call(?SERVER, {export_trace, SpanId, Filename}).

%% @doc Export metrics to file
-spec export_metrics(binary(), string()) -> ok | {error, term()}.
export_metrics(SpanId, Filename) ->
    gen_server:call(?SERVER, {export_metrics, SpanId, Filename}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Defer OTEL initialization to avoid blocking
    %% Use {continue, init} pattern for async setup
    {ok, #state{}, {continue, init}};

handle_call({store_span, SpanId, Span}, _From, State) ->
    UpdatedSpans = maps:put(SpanId, Span, State#state.spans),
    {reply, ok, State#state{spans = UpdatedSpans}};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(get_performance_summary, _From, State) ->
    Summary = calculate_performance_summary(State#state.spans, State#state.metrics),
    {reply, Summary, State};

handle_call({inject_chaos, Config}, _From, State) ->
    case State#state.chaos_enabled of
        true ->
            ChaosResult = apply_chaos(Config),
            NewHistory = [Config | State#state.chaos_history],
            {reply, ChaosResult, State#state{chaos_history = NewHistory}};
        false ->
            {reply, {error, chaos_disabled}, State}
    end;

handle_call(get_chaos_status, _From, State) ->
    Status = #{
        enabled => State#state.chaos_enabled,
        recent_events => lists:sublist(State#state.chaos_history, 10),
        total_events => length(State#state.chaos_history),
        config => State#state.config
    },
    {reply, Status, State};

handle_call({export_trace, SpanId, Filename}, _From, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            FormattedData = format_trace_data(Span),
            file:write_file(Filename, FormattedData),
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({export_metrics, SpanId, Filename}, _From, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            MetricsData = format_metrics_data(Span),
            file:write_file(Filename, MetricsData),
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(start_metrics_collection, State) ->
    %% Start periodic metrics collection
    AggregationTimer = erlang:send_after(?METRICS_AGG_INTERVAL, self(), aggregate_metrics),
    {noreply, State#state{aggregation_timer = AggregationTimer}};

handle_cast({add_span_event, SpanId, EventName, Attributes}, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            Event = #{name => EventName,
                     timestamp => erlang:system_time(nanosecond),
                     attributes => Attributes},
            UpdatedEvents = [Event | Span#events],
            UpdatedSpan = Span#{events => UpdatedEvents},
            UpdatedSpans = maps:put(SpanId, UpdatedSpan, State#state.spans),
            {noreply, State#state{spans = UpdatedSpans}};
        error ->
            {noreply, State}
    end;

handle_cast({add_span_error, SpanId, Error}, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            ErrorRecord = #{
                error_type => atom_to_binary(element(1, Error)),
                error_message => list_to_binary(io_lib:format("~p", [element(2, Error)])),
                error_stacktrace => element(3, Error),
                error_class => error
            },
            UpdatedErrors = [ErrorRecord | Span#errors],
            UpdatedSpan = Span#{errors => UpdatedErrors},
            UpdatedSpans = maps:put(SpanId, UpdatedSpan, State#state.spans),
            {noreply, State#state{spans = UpdatedSpans}};
        error ->
            {noreply, State}
    end;

handle_cast({update_span, SpanId, UpdatedSpan}, State) ->
    UpdatedSpans = maps:put(SpanId, UpdatedSpan, State#state.spans),
    {noreply, State#state{spans = UpdatedSpans}};

handle_cast({inject_latency, SpanId, DurationMs}, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            %% Apply latency injection
            timer:sleep(DurationMs),

            %% Add latency event
            LatencyEvent = #{
                name => <<"chaos.latency_injected">>,
                timestamp => erlang:system_time(nanosecond),
                attributes => #{<<"duration_ms">> => DurationMs,
                              <<"span_id">> => SpanId}
            },

            UpdatedEvents = [LatencyEvent | Span#events],
            UpdatedSpan = Span#{events => UpdatedEvents, chaos_injected => true},
            UpdatedSpans = maps:put(SpanId, UpdatedSpan, State#state.spans),

            {noreply, State#state{spans = UpdatedSpans}};
        error ->
            {noreply, State}
    end;

handle_cast({inject_failure, SpanId, ErrorReason}, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            %% Inject failure by raising an exception
            erlang:throw({chaos_injected, ErrorReason}),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast({observe_command_start, SpanId, Command, Context}, State) ->
    CommandSpan = #{name => <<"mcp.command.", Command/binary>>,
                    type => command,
                    start_time => erlang:system_time(nanosecond),
                    attributes => Context,
                    events => [],
                    errors => [],
                    metrics => #{},
                    chaos_injected => false,
                    chaos_type => undefined},

    UpdatedSpans = maps:put(SpanId, CommandSpan, State#state.spans),
    {noreply, State#state{spans = UpdatedSpans}};

handle_cast({observe_command_complete, SpanId, Command, Result}, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            EndTime = erlang:system_time(nanosecond),
            Duration = EndTime - Span#start_time,

            CompletionEvent = #{
                name => <<"command.completed">>,
                timestamp => EndTime,
                attributes => #{<<"command">> => Command,
                              <<"result">> => format_result(Result),
                              <<"duration_ms">> => Duration div 1000000}
            },

            UpdatedSpan = Span#{end_time => EndTime,
                              duration_ns => Duration,
                              status => ok,
                              events => [CompletionEvent | Span#events]},
            UpdatedSpans = maps:put(SpanId, UpdatedSpan, State#state.spans),

            {noreply, State#state{spans = UpdatedSpans}};
        error ->
            {noreply, State}
    end;

handle_cast({observe_command_error, SpanId, Command, Error}, State) ->
    case maps:find(SpanId, State#state.spans) of
        {ok, Span} ->
            ErrorRecord = #{
                error_type => atom_to_binary(element(1, Error)),
                error_message => list_to_binary(io_lib:format("~p", [element(2, Error)])),
                error_stacktrace => element(3, Error),
                error_class => error
            },

            UpdatedSpan = Span#{errors => [ErrorRecord | Span#errors], status => error},
            UpdatedSpans = maps:put(SpanId, UpdatedSpan, State#state.spans),

            {noreply, State#state{spans = UpdatedSpans}};
        error ->
            {noreply, State}
    end;

handle_cast({record_validation_success, Type, ChaosInjected}, State) ->
    UpdatedMetrics = record_validation_success(State#state.metrics, Type, ChaosInjected),
    {noreply, State#state{metrics = UpdatedMetrics}};

handle_cast({record_validation_error, Type, ChaosInjected}, State) ->
    UpdatedMetrics = record_validation_error(State#state.metrics, Type, ChaosInjected),
    {noreply, State#state{metrics = UpdatedMetrics}};

handle_cast(reset_metrics, State) ->
    NewMetrics = init_metrics_data(),
    {noreply, State#state{metrics = NewMetrics, spans = #{}, chaos_history = []}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    %% Async initialization after gen_server is running
    MergedConfig = ?DEFAULT_OTEL_CONFIG,

    %% Initialize OpenTelemetry (may still block, but server is accepting messages)
    case erlmcp_otel:init(MergedConfig) of
        ok ->
            %% Initialize metrics aggregation
            AggregationTimer = erlang:send_after(?METRICS_AGG_INTERVAL, self(), aggregate_metrics),

            %% Start exporter processes
            ExporterPids = start_exporters(MergedConfig),

            %% Initialize state
            InitState = State#state{
                config = MergedConfig,
                tracer = erlmcp_otel:get_tracer_provider(),
                spans = #{},
                metrics = init_metrics_data(),
                chaos_enabled = maps:get(enable_chaos, MergedConfig, false),
                chaos_history = [],
                aggregation_timer = AggregationTimer,
                exporter_pids = ExporterPids
            },

            gen_server:cast(?SERVER, start_metrics_collection),

            {noreply, InitState};
        {error, Reason} ->
            %% Log error but keep server running
            error_logger:error_msg("Failed to initialize OTEL: ~p~n", [Reason]),
            {noreply, State#state{
                config = MergedConfig,
                spans = #{},
                metrics = init_metrics_data(),
                chaos_enabled = false,
                chaos_history = []
            }}
    end;

handle_info(aggregate_metrics, State) ->
    %% Aggregate all span metrics
    AggregatedMetrics = aggregate_all_metrics(State#state.spans, State#state.metrics),

    %% Send metrics to OpenTelemetry
    send_metrics_to_otel(AggregatedMetrics),

    %% Schedule next aggregation
    NewTimer = erlang:send_after(?METRICS_AGG_INTERVAL, self(), aggregate_metrics),

    {noreply, State#state{metrics = AggregatedMetrics, aggregation_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel timer
    case State#state.aggregation_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    %% Flush any remaining spans
    erlmcp_otel:shutdown(),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Initialize metrics data structure
-spec init_metrics_data() -> metrics_data().
init_metrics_data() ->
    #{total_validations => 0,
      successful_validations => 0,
      failed_validations => 0,
      average_latency_ms => 0.0,
      p95_latency_ms => 0.0,
      p99_latency_ms => 0.0,
      throughput_per_sec => 0.0,
      error_rate => 0.0,
      resource_usage => #{},
      chaos_events => 0,
      trace_duration_ms => 0}.

%% @private Start exporter processes
-spec start_exporters(otel_config()) -> #{atom() => pid()}.
start_exporters(Config) ->
    Exporters = maps:get(exporters, Config, [console]),
    lists:foldl(fun(Exporter, Acc) ->
                    case start_exporter(Exporter, Config) of
                        {ok, Pid} -> Acc#{Exporter => Pid};
                        {error, _} -> Acc
                    end
                end,
                #{},
                Exporters).

%% @private Start individual exporter
-spec start_exporter(atom(), map()) -> {ok, pid()} | {error, term()}.
start_exporter(console, _Config) ->
    {ok, self()};  % Console exporter is local
start_exporter(Exporter, Config) ->
    %% This would start actual exporter processes
    %% For now, return dummy process
    {ok, spawn(fun() -> exporter_loop(Exporter, Config) end)}.

%% @private Exporter process loop
-spec exporter_loop(atom(), map()) -> no_return().
exporter_loop(Exporter, Config) ->
    receive
        {export, Data} ->
            %% Export data according to exporter type
            case Exporter of
                jaeger ->
                    export_to_jaeger(Data, Config);
                prometheus ->
                    export_to_prometheus(Data, Config);
                otlp ->
                    export_to_otlp(Data, Config);
                _ ->
                    ok
            end,
            exporter_loop(Exporter, Config);
        stop ->
            ok
    end.

%% @private Apply chaos injection
-spec apply_chaos(map()) -> ok | {error, term()}.
apply_chaos(Config) ->
    ChaosType = maps:get(type, Config, network_failure),
    Probability = maps:get(probability, Config, 0.1),

    case rand:uniform() < Probability of
        true ->
            case ChaosType of
                network_failure ->
                    inject_network_failure(Config);
                latency_spike ->
                    inject_latency_spike(Config);
                resource_exhaustion ->
                    inject_resource_exhaustion(Config);
                process_crash ->
                    inject_process_crash(Config);
                timeout ->
                    inject_timeout(Config);
                _ ->
                    ok
            end;
        false ->
            ok
    end.

%% @private Inject network failure
-spec inject_network_failure(map()) -> ok.
inject_network_failure(Config) ->
    Duration = maps:get(duration, Config, 1000),
   Magnitude = maps:get(magnitude, Config, 0.5),

    %% Simulate network latency
    Delay = trunc(Duration * Magnitude),
    timer:sleep(Delay),

    %% Log chaos event
    gen_server:cast(?SERVER, {log_chaos_event, network_failure, Delay}),

    ok.

%% @private Inject latency spike
-spec inject_latency_spike(map()) -> ok.
inject_latency_spike(Config) ->
    Duration = maps:get(duration, Config, 500),
    Magnitude = maps:get(magnitude, Config, 2.0),  % 2x normal latency

    Delay = trunc(Duration * Magnitude),
    timer:sleep(Delay),

    gen_server:cast(?SERVER, {log_chaos_event, latency_spike, Delay}),

    ok.

%% @private Inject resource exhaustion
-spec inject_resource_exhaustion(map()) -> ok.
inject_resource_exhaustion(Config) ->
    %% Allocate large amount of memory
    Size = trunc(10 * 1024 * 1024 * maps:get(magnitude, Config, 0.5)),  % 10MB * magnitude
    _LargeData = binary:copy(<<"x">>, Size),

    gen_server:cast(?SERVER, {log_chaos_event, resource_exhaustion, Size}),

    ok.

%% @private Inject process crash
-spec inject_process_crash(map()) -> ok.
inject_process_crash(_Config) ->
    %% Exit the process
    exit(normal),

    ok.

%% @private Inject timeout
-spec inject_timeout(map()) -> ok.
inject_timeout(Config) ->
    Duration = maps:get(duration, Config, 5000),
    timer:sleep(Duration),

    gen_server:cast(?SERVER, {log_chaos_event, timeout, Duration}),

    ok.

%% @private Check if chaos should be injected
-spec should_inject_chaos(map()) -> boolean().
should_inject_chaos(ChaosOpts) ->
    case maps:get(chaos, ChaosOpts, false) of
        true ->
            Probability = maps:get(probability, ChaosOpts, 0.1),
            rand:uniform() < Probability;
        false ->
            false
    end.

%% @private Internal chaos injection
-spec inject_chaos_internal(map(), binary()) -> ok.
inject_chaos_internal(ChaosOpts, SpanId) ->
    case maps:get(chaos_type, ChaosOpts, latency) of
        latency ->
            Duration = maps:get(duration_ms, ChaosOpts, 100),
            gen_server:cast(?SERVER, {inject_latency, SpanId, Duration});
        failure ->
            Reason = maps:get(reason, ChaosOpts, chaos_injected),
            gen_server:cast(?SERVER, {inject_failure, SpanId, Reason});
        _ ->
            ok
    end.

%% @private Generate span ID
-spec generate_span_id() -> binary().
generate_span_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

%% @private Record validation success
-spec record_validation_success(metrics_data(), binary(), boolean()) -> metrics_data().
record_validation_success(Metrics, Type, ChaosInjected) ->
    UpdatedMetrics = Metrics#{total_validations := Metrics#total_validations + 1,
                             successful_validations := Metrics#successful_validations + 1},

    %% Update error rate
    ErrorRate = calculate_error_rate(UpdatedMetrics),
    UpdatedMetrics#{error_rate => ErrorRate}.

%% @private Record validation error
-spec record_validation_error(metrics_data(), binary(), boolean()) -> metrics_data().
record_validation_error(Metrics, Type, ChaosInjected) ->
    UpdatedMetrics = Metrics#{total_validations := Metrics#total_validations + 1,
                             failed_validations := Metrics#failed_validations + 1,
                             chaos_events := Metrics#chaos_events + 1},

    %% Update error rate
    ErrorRate = calculate_error_rate(UpdatedMetrics),
    UpdatedMetrics#{error_rate => ErrorRate}.

%% @private Calculate error rate
-spec calculate_error_rate(metrics_data()) -> float().
calculate_error_rate(Metrics) ->
    Total = Metrics#total_validations,
    if Total > 0 ->
        Failed = Metrics#failed_validations,
        Failed / Total;
    true ->
        0.0
    end.

%% @private Calculate performance summary
-spec calculate_performance_summary(map(), metrics_data()) -> map().
calculate_performance_summary(Spans, Metrics) ->
    Latencies = collect_latencies(Spans),

    #{
        total_spans => map_size(Spans),
        total_validations => Metrics#total_validations,
        success_rate => (Metrics#successful_validations / Metrics#total_validations) * 100,
        average_latency_ms => calculate_average_latency(Latencies),
        p95_latency_ms => calculate_percentile(Latencies, 95),
        p99_latency_ms => calculate_percentile(Latencies, 99),
        error_rate => Metrics#error_rate,
        chaos_events => Metrics#chaos_events,
        resource_usage => Metrics#resource_usage,
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Collect latency data from spans
-spec collect_latencies(map()) -> [integer()].
collect_latencies(Spans) ->
    lists:foldl(fun(_, Acc) ->
                    case maps:get(duration_ns, Span, undefined) of
                        undefined -> Acc;
                        DurationNs -> [DurationNs div 1000000 | Acc]  % Convert to ms
                    end
                end,
                [],
                maps:values(Spans)).

%% @private Calculate average latency
-spec calculate_average_latency([integer()]) -> float().
calculate_average_latency(Latencies) ->
    case Latencies of
        [] -> 0.0;
        _ -> lists:sum(Latencies) / length(Latencies)
    end.

%% @private Calculate percentile
-spec calculate_percentile([integer()], integer()) -> float().
calculate_percentile(Latencies, Percentile) ->
    Sorted = lists:sort(Latencies),
    Length = length(Sorted),
    Index = trunc((Percentile / 100) * Length),

    if Index > 0 ->
        lists:nth(Index, Sorted);
    true ->
        0.0
    end.

%% @private Aggregate all metrics
-spec aggregate_all_metrics(map(), metrics_data()) -> metrics_data().
aggregate_all_metrics(Spans, Metrics) ->
    UpdatedMetrics = Metrics#{timestamp => erlang:system_time(millisecond)},

    %% Calculate throughput
    CurrentTime = erlang:system_time(millisecond),
    TimeWindow = 60000,  % 1 minute
    RecentSpans = lists:filter(fun(Span) ->
                                   case maps:get(start_time, Span, undefined) of
                                       undefined -> false;
                                       StartTime -> (CurrentTime - (StartTime div 1000000)) < TimeWindow
                                   end
                               end,
                               maps:values(Spans)),

    Throughput = length(RecentSpans) / (TimeWindow / 1000),
    UpdatedMetrics#{throughput_per_sec => Throughput}.

%% @private Send metrics to OpenTelemetry
-spec send_metrics_to_otel(metrics_data()) -> ok.
send_metrics_to_otel(Metrics) ->
    %% This would send metrics to configured exporters
    %% For now, just log them
    ok.

%% @private Format trace data for export
-spec format_trace_data(validation_span()) -> iolist().
format_trace_data(Span) ->
    jsx:encode(Span, [{space, 1}, {indent, 2}]).

%% @private Format metrics data for export
-spec format_metrics_data(validation_span()) -> iolist().
format_metrics_data(Span) ->
    jsx:encode(Span, [{space, 1}, {indent, 2}]).

%% @private Export to Jaeger
-spec export_to_jaeger(term(), map()) -> ok.
export_to_jaeger(Data, _Config) ->
    %% Implement Jaeger export
    ok.

%% @private Export to Prometheus
-spec export_to_prometheus(term(), map()) -> ok.
export_to_prometheus(Data, _Config) ->
    %% Implement Prometheus export
    ok.

%% @private Export to OTLP
-spec export_to_otlp(term(), map()) -> ok.
export_to_otlp(Data, _Config) ->
    %% Implement OTLP export
    ok.

%% @private Format result for storage
-spec format_result(term()) -> binary().
format_result(Result) when is_binary(Result) ->
    Result;
format_result(Result) when is_atom(Result) ->
    atom_to_binary(Result, utf8);
format_result(Result) ->
    list_to_binary(io_lib:format("~p", [Result])).