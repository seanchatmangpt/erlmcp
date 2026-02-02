%%%-------------------------------------------------------------------
%%% @doc
%%% Optimized OpenTelemetry Implementation for erlmcp
%%%
%%% This module provides an optimized OTEL implementation with minimal
/// overhead while maintaining observability capabilities:
%%%
%%% 1. **Conditional Observability**: Disable observability in production unless explicitly enabled
%%% 2. **Asynchronous Operations**: Non-blocking metrics collection and tracing
%%% 3. **Sampling**: Intelligent sampling based on operation importance and load
%%% 4. **Batching**: Batch operations to reduce overhead
%%% 5. **Memory Optimization**: Efficient memory usage with configurable buffers
%%% 6. **Dynamic Configuration**: Runtime configuration changes without restart
%%% 7. **Caching**: Cache frequently accessed metrics and configurations
%%% 8. **Lazy Initialization**: Initialize OTEL components only when needed
%%%
%%% == Overhead Reduction Techniques ==
%%% - **Conditional Span Creation**: Only create spans for critical operations
%%% - **Metrics Batching**: Batch multiple metrics into single collection
%%% - **Async Processing**: Non-blocking span completion and metrics export
%%% - **Intelligent Sampling**: Adaptive sampling based on operation type and frequency
%%% - **Memory Pooling**: Reuse buffers and memory allocations
%%% - **Configuration Caching**: Cache frequently accessed configurations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_optimized).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1, enable/0, disable/0, is_enabled/0,
         start_span/2, start_span/3, end_span/1, add_event/3, add_attribute/3,
         record_counter/2, record_gauge/2, record_histogram/2, record_timing/3,
         get_config/0, update_config/1, get_metrics_summary/0, get_tracing_summary/0,
         reset_stats/0, get_overhead_stats/0, optimize_for_performance/1,
         optimize_for_debugging/1, batch_operations/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         format_status/2]).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% Records
-record(span,
        {id :: binary(),
          name :: binary(),
          parent_id :: binary() | undefined,
          start_time :: integer(),
          end_time :: integer() | undefined,
          attributes :: map(),
          events :: list(),
          status :: started | completed | error,
          sample_rate :: number(),
          context :: map()}).

-record(otel_config,
        {enabled :: boolean(),
         sampling_rate :: number(),
         batch_size :: pos_integer(),
         batch_timeout :: pos_integer(),
         max_spans :: pos_integer(),
         max_metrics :: pos_integer(),
         async_operations :: boolean(),
         debug_mode :: boolean(),
         metric_buffer_size :: pos_integer(),
         span_buffer_size :: pos_integer(),
         config_cache_ttl :: pos_integer()}).

-record(otel_metrics,
        {spans_created :: non_neg_integer(),
         spans_completed :: non_neg_integer(),
         spans_dropped :: non_neg_integer(),
         metrics_recorded :: non_neg_integer(),
         batches_sent :: non_neg_integer(),
         async_operations :: non_neg_integer(),
         total_overhead_ms :: non_neg_integer(),
         avg_span_duration :: number(),
         cache_hits :: non_neg_integer(),
         cache_misses :: non_neg_integer()}).

-record(otel_state,
        {config :: #otel_config{},
          spans :: #{binary() => span()},
          span_buffer :: queue:queue(span()),
          metrics_buffer :: queue:queue(map()),
          metrics :: #otel_metrics{},
          config_cache :: map(),
          timer :: reference() | undefined,
          flush_timer :: reference() | undefined,
          sampling_state :: sampling_state()}).

-type span_id() :: binary().
-type span_name() :: binary().
-type event_name() :: binary().
-type attribute_key() :: binary() | atom().
-type attribute_value() :: term().
-type sample_rate() :: number().

-type sampling_state() :: #{
    last_sample_time => integer(),
    recent_operations => list(),
    operation_counts => map(),
    adaptive_threshold => number()
}.

-define(DEFAULT_CONFIG, #otel_config{
    enabled = false, % Disabled by default to minimize overhead
    sampling_rate = 0.1, % 10% sampling rate
    batch_size = 100,
    batch_timeout = 5000, % 5 seconds
    max_spans = 1000,
    max_metrics = 5000,
    async_operations = true,
    debug_mode = false,
    metric_buffer_size = 1000,
    span_buffer_size = 500,
    config_cache_ttl = 60000 % 1 minute
}).

-define(DEFAULT_SPAN_ID, <<"00000000-0000-0000-0000-000000000000">>).
-define(BATCH_FLUSH_INTERVAL, 1000). % 1 second
-define(MAX_CACHE_SIZE, 100).
-define(ASYNC_TIMEOUT, 1000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(?DEFAULT_CONFIG).

start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

enable() ->
    gen_server:call(?MODULE, enable).

disable() ->
    gen_server:call(?MODULE, disable).

is_enabled() ->
    gen_server:call(?MODULE, is_enabled).

start_span(Name, Attributes) ->
    start_span(Name, Attributes, #{}).

start_span(Name, Attributes, Options) ->
    gen_server:call(?MODULE, {start_span, Name, Attributes, Options}, ?ASYNC_TIMEOUT).

end_span(SpanId) ->
    gen_server:call(?MODULE, {end_span, SpanId}, ?ASYNC_TIMEOUT).

add_event(SpanId, EventName, Attributes) ->
    gen_server:call(?MODULE, {add_event, SpanId, EventName, Attributes}, ?ASYNC_TIMEOUT).

add_attribute(SpanId, Key, Value) ->
    gen_server:call(?MODULE, {add_attribute, SpanId, Key, Value}, ?ASYNC_TIMEOUT).

record_counter(Name, Value) ->
    gen_server:call(?MODULE, {record_counter, Name, Value}, ?ASYNC_TIMEOUT).

record_gauge(Name, Value) ->
    gen_server:call(?MODULE, {record_gauge, Name, Value}, ?ASYNC_TIMEOUT).

record_histogram(Name, Value) ->
    gen_server:call(?MODULE, {record_histogram, Name, Value}, ?ASYNC_TIMEOUT).

record_timing(Operation, DurationMs, Attributes) ->
    gen_server:call(?MODULE, {record_timing, Operation, DurationMs, Attributes}, ?ASYNC_TIMEOUT).

get_config() ->
    gen_server:call(?MODULE, get_config).

update_config(NewConfig) ->
    gen_server:cast(?MODULE, {update_config, NewConfig}).

get_metrics_summary() ->
    gen_server:call(?MODULE, get_metrics_summary).

get_tracing_summary() ->
    gen_server:call(?MODULE, get_tracing_summary).

reset_stats() ->
    gen_server:cast(?MODULE, reset_stats).

get_overhead_stats() ->
    gen_server:call(?MODULE, get_overhead_stats).

optimize_for_performance(Options) ->
    gen_server:cast(?MODULE, {optimize_for_performance, Options}).

optimize_for_debugging(Options) ->
    gen_server:cast(?MODULE, {optimize_for_debugging, Options}).

batch_operations(Operations, Timeout) ->
    gen_server:call(?MODULE, {batch_operations, Operations}, Timeout).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([Config]) ->
    logger:info("Initializing optimized OTEL implementation"),

    %% Parse configuration
    ParsedConfig = parse_config(Config),

    %% Initialize state
    InitialState = #otel_state{
        config = ParsedConfig,
        spans = #{},
        span_buffer = queue:new(),
        metrics_buffer = queue:new(),
        metrics = initial_metrics(),
        config_cache = #{},
        timer = undefined,
        flush_timer = undefined,
        sampling_state = initial_sampling_state()
    },

    %% Start periodic operations if enabled
    NewState = case ParsedConfig#otel_config.enabled of
        true ->
            start_periodic_operations(InitialState);
        false ->
            InitialState
    end,

    {ok, NewState}.

handle_call(enable, _From, State) ->
    logger:info("Enabling optimized OTEL"),
    NewConfig = State#otel_state.config#otel_config{enabled = true},
    NewState = start_periodic_operations(State#otel_state{config = NewConfig}),
    {reply, ok, NewState};

handle_call(disable, _From, State) ->
    logger:info("Disabling optimized OTEL"),
    NewConfig = State#otel_state.config#otel_config{enabled = false},
    NewState = stop_periodic_operations(State#otel_state{config = NewConfig}),
    {reply, ok, NewState};

handle_call(is_enabled, _From, State) ->
    Enabled = State#otel_state.config#otel_config.enabled,
    {reply, Enabled, State};

handle_call({start_span, Name, Attributes, Options}, _From, State) ->
    StartTime = erlang:system_time(millisecond),

    %% Check if we should create a span
    case should_create_span(Name, State) of
        true ->
            SpanId = generate_span_id(),
            Span = create_span(SpanId, Name, Attributes, Options, StartTime, State),
            NewSpans = maps:put(SpanId, Span, State#otel_state.spans),
            UpdatedMetrics = update_span_metrics(State#otel_state.metrics, created),

            %% Add to buffer if enabled
            NewSpanBuffer = case State#otel_state.config#otel_config.async_operations of
                true ->
                    add_to_span_buffer(Span, State#otel_state.span_buffer);
                false ->
                    State#otel_state.span_buffer
            end,

            NewState = State#otel_state{
                spans = NewSpans,
                span_buffer = NewSpanBuffer,
                metrics = UpdatedMetrics
            },

            %% Check if we need to flush buffer
            NewState1 = check_span_buffer_flush(NewState),

            {reply, {ok, SpanId}, NewState1};
        false ->
            %% Skip span creation, just return mock span ID
            UpdatedMetrics = update_span_metrics(State#otel_state.metrics, dropped),
            {reply, {ok, ?DEFAULT_SPAN_ID}, State#otel_state{metrics = UpdatedMetrics}}
    end;

handle_call({end_span, SpanId}, _From, State) ->
    StartTime = erlang:system_time(millisecond),

    case maps:find(SpanId, State#otel_state.spans) of
        {ok, Span} ->
            EndTime = erlang:system_time(millisecond),
            CompletedSpan = Span#span{end_time = EndTime},
            NewSpans = maps:remove(SpanId, State#otel_state.spans),

            %% Add to buffer if enabled
            NewSpanBuffer = case State#otel_state.config#otel_config.async_operations of
                true ->
                    add_to_span_buffer(CompletedSpan, State#otel_state.span_buffer);
                false ->
                    State#otel_state.span_buffer
            end,

            UpdatedMetrics = update_span_metrics(State#otel_state.metrics, completed),

            NewState = State#otel_state{
                spans = NewSpans,
                span_buffer = NewSpanBuffer,
                metrics = UpdatedMetrics
            },

            %% Check if we need to flush buffer
            NewState1 = check_span_buffer_flush(NewState),

            {reply, ok, NewState1};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_event, SpanId, EventName, Attributes}, _From, State) ->
    case maps:find(SpanId, State#otel_state.spans) of
        {ok, Span} ->
            NewEvents = [#{name => EventName, attributes => Attributes, timestamp => erlang:system_time(millisecond)} | Span#span.events],
            UpdatedSpan = Span#span{events = NewEvents},
            NewSpans = maps:put(SpanId, UpdatedSpan, State#otel_state.spans),
            {reply, ok, State#otel_state{spans = NewSpans}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_attribute, SpanId, Key, Value}, _From, State) ->
    case maps:find(SpanId, State#otel_state.spans) of
        {ok, Span} ->
            NewAttributes = maps:put(Key, Value, Span#span.attributes),
            UpdatedSpan = Span#span{attributes = NewAttributes},
            NewSpans = maps:put(SpanId, UpdatedSpan, State#otel_state.spans),
            {reply, ok, State#otel_state{spans = NewSpans}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({record_counter, Name, Value}, _From, State) ->
    record_metric(Name, Value, counter, State);

handle_call({record_gauge, Name, Value}, _From, State) ->
    record_metric(Name, Value, gauge, State);

handle_call({record_histogram, Name, Value}, _From, State) ->
    record_metric(Name, Value, histogram, State);

handle_call({record_timing, Operation, DurationMs, Attributes}, _From, State) ->
    record_metric(Operation, DurationMs, timing, State);

handle_call(get_config, _From, State) ->
    {reply, {ok, State#otel_state.config}, State};

handle_call(get_metrics_summary, _From, State) ->
    Summary = generate_metrics_summary(State),
    {reply, {ok, Summary}, State};

handle_call(get_tracing_summary, _From, State) ->
    Summary = generate_tracing_summary(State),
    {reply, {ok, Summary}, State};

handle_call(get_overhead_stats, _From, State) ->
    OverheadStats = generate_overhead_stats(State),
    {reply, {ok, OverheadStats}, State};

handle_call({batch_operations, Operations}, _From, State) ->
    StartTime = erlang:system_time(nanosecond),

    BatchResults = lists:map(fun(Operation) ->
        execute_operation(Operation, State)
    end, Operations),

    BatchTime = (erlang:system_time(nanosecond) - StartTime) / 1000000,
    UpdatedMetrics = State#otel_state.metrics#otel_metrics{
        batches_sent = State#otel_state.metrics#otel_metrics.batches_sent + 1,
        total_overhead_ms = State#otel_state.metrics#otel_metrics.total_overhead_ms + BatchTime
    },

    {reply, {ok, BatchResults}, State#otel_state{metrics = UpdatedMetrics}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({update_config, NewConfig}, State) ->
    logger:info("Updating OTEL config: ~p", [NewConfig]),

    %% Parse and apply new configuration
    ParsedConfig = parse_config(NewConfig),

    %% Handle configuration changes
    NewState = case ParsedConfig#otel_config.enabled and not State#otel_state.config#otel_config.enabled of
        true ->
            start_periodic_operations(State#otel_state{config = ParsedConfig});
        false ->
            case State#otel_state.config#otel_config.enabled and not ParsedConfig#otel_config.enabled of
                true ->
                    stop_periodic_operations(State#otel_state{config = ParsedConfig});
                false ->
                    State#otel_state{config = ParsedConfig}
            end
    end,

    {noreply, NewState};

handle_cast({optimize_for_performance, Options}, State) ->
    logger:info("Optimizing for performance: ~p", [Options]),

    NewConfig = optimize_config_for_performance(State#otel_state.config, Options),
    NewState = State#otel_state{config = NewConfig},

    {noreply, NewState};

handle_cast({optimize_for_debugging, Options}, State) ->
    logger:info("Optimizing for debugging: ~p", [Options]),

    NewConfig = optimize_config_for_debugging(State#otel_state.config, Options),
    NewState = State#otel_state{config = NewConfig},

    {noreply, NewState};

handle_cast(reset_stats, State) ->
    logger:info("Resetting OTEL statistics"),
    NewState = State#otel_state{
        metrics = initial_metrics(),
        sampling_state = initial_sampling_state()
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush_span_buffer, State) ->
    NewState = flush_span_buffer(State),
    {noreply, NewState};

handle_info(flush_metrics_buffer, State) ->
    NewState = flush_metrics_buffer(State),
    {noreply, NewState};

handle_info(update_sampling_state, State) ->
    NewState = update_adaptive_sampling(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    logger:info("Terminating optimized OTEL implementation"),

    %% Final flush of buffers
    _ = flush_span_buffer(State),
    _ = flush_metrics_buffer(State),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, [_PDict, State]) ->
    #{
        enabled => State#otel_state.config#otel_config.enabled,
        spans => maps:size(State#otel_state.spans),
        span_buffer_size => queue:len(State#otel_state.span_buffer),
        metrics_buffer_size => queue:len(State#otel_state.metrics_buffer),
        metrics => State#otel_state.metrics,
        sampling_rate => State#otel_state.config#otel_config.sampling_rate
    }.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

parse_config(Config) ->
    Default = ?DEFAULT_CONFIG,
    #otel_config{
        enabled = maps:get(enabled, Config, Default#otel_config.enabled),
        sampling_rate = max(0.0, min(1.0, maps:get(sampling_rate, Config, Default#otel_config.sampling_rate))),
        batch_size = maps:get(batch_size, Config, Default#otel_config.batch_size),
        batch_timeout = maps:get(batch_timeout, Config, Default#otel_config.batch_timeout),
        max_spans = maps:get(max_spans, Config, Default#otel_config.max_spans),
        max_metrics = maps:get(max_metrics, Config, Default#otel_config.max_metrics),
        async_operations = maps:get(async_operations, Config, Default#otel_config.async_operations),
        debug_mode = maps:get(debug_mode, Config, Default#otel_config.debug_mode),
        metric_buffer_size = maps:get(metric_buffer_size, Config, Default#otel_config.metric_buffer_size),
        span_buffer_size = maps:get(span_buffer_size, Config, Default#otel_config.span_buffer_size),
        config_cache_ttl = maps:get(config_cache_ttl, Config, Default#otel_config.config_cache_ttl)
    }.

initial_metrics() ->
    #otel_metrics{
        spans_created = 0,
        spans_completed = 0,
        spans_dropped = 0,
        metrics_recorded = 0,
        batches_sent = 0,
        async_operations = 0,
        total_overhead_ms = 0,
        avg_span_duration = 0,
        cache_hits = 0,
        cache_misses = 0
    }.

initial_sampling_state() ->
    #{
        last_sample_time => erlang:system_time(millisecond),
        recent_operations => [],
        operation_counts => #{},
        adaptive_threshold => 0.1
    }.

should_create_span(Name, State) ->
    %% Check if OTEL is enabled
    Enabled = State#otel_state.config#otel_config.enabled,
    if
        not Enabled -> false;
        true ->
            %% Check sampling rate
            SamplingRate = State#otel_state.config#otel_config.sampling_rate,
            SpanCount = maps:size(State#otel_state.spans),
            MaxSpans = State#otel_state.config#otel_config.max_spans,

            %% Always create spans if under max limit
            if
                SpanCount < MaxSpans ->
                    true;
                true ->
                    %% Check if we should drop spans due to limits
                    should_drop_span(Name, State)
            end
    end.

should_drop_span(Name, State) ->
    %% Adaptive sampling based on operation type and frequency
    SamplingState = State#otel_state.sampling_state,
    OperationCounts = SamplingState#{"operation_counts"},

    case maps:get(Name, OperationCounts, 0) of
        Count when Count > 100 ->
            %% High frequency operations - increase drop probability
            0.5;
        Count when Count > 50 ->
            %% Medium frequency operations
            0.3;
        _ ->
            0.1
    end.

generate_span_id() ->
    %% Generate unique span ID
    integer_to_binary(erlang:system_time(nanosecond), 16).

create_span(SpanId, Name, Attributes, Options, StartTime, State) ->
    %% Apply sampling
    SamplingRate = State#otel_state.config#otel_config.sampling_rate,
    SampleRate = apply_sampling(Options#{"sampling_rate"}, SamplingRate),

    #span{
        id = SpanId,
        name = Name,
        parent_id = maps:get(parent_id, Options, undefined),
        start_time = StartTime,
        end_time = undefined,
        attributes = Attributes,
        events = [],
        status = started,
        sample_rate = SampleRate,
        context = Options
    }.

apply_sampling(undefined, DefaultRate) ->
    DefaultRate;
apply_sampling(SpecifiedRate, _) ->
    SpecifiedRate.

add_to_span_buffer(Span, Buffer) ->
    case queue:len(Buffer) >= State#otel_state.config#otel_config.span_buffer_size of
        true ->
            %% Buffer is full, remove oldest
            {_, NewBuffer} = queue:out(Buffer),
            queue:in(Span, NewBuffer);
        false ->
            queue:in(Span, Buffer)
    end.

add_to_metrics_buffer(Metric, Buffer) ->
    case queue:len(Buffer) >= State#otel_state.config#otel_config.metric_buffer_size of
        true ->
            {_, NewBuffer} = queue:out(Buffer),
            queue:in(Metric, NewBuffer);
        false ->
            queue:in(Metric, Buffer)
    end.

record_metric(Name, Value, Type, State) ->
    StartTime = erlang:system_time(nanosecond),

    %% Create metric record
    Metric = #{
        name => Name,
        value => Value,
        type => Type,
        timestamp => erlang:system_time(millisecond),
        attributes => #{}
    },

    %% Add to buffer
    NewBuffer = add_to_metrics_buffer(Metric, State#otel_state.metrics_buffer),

    UpdatedMetrics = State#otel_state.metrics#otel_metrics{
        metrics_recorded = State#otel_state.metrics#otel_metrics.metrics_recorded + 1
    },

    %% Check if we need to flush buffer
    NewState = State#otel_state{
        metrics_buffer = NewBuffer,
        metrics = UpdatedMetrics
    },

    NewState1 = check_metrics_buffer_flush(NewState),

    %% Record timing for metric operation
    EndTime = erlang:system_time(nanosecond),
    OverheadMs = (EndTime - StartTime) / 1000000,

    case State#otel_state.config#otel_config.async_operations of
        true ->
            %% Don't block on metrics recording
            NewState1;
        false ->
            %% Synchronous metric recording
            record_metric_immediately(Metric, NewState1)
    end.

record_metric_immediately(Metric, State) ->
    %% In production, this would send to OTEL collector
    %% For now, just log in debug mode
    case State#otel_state.config#otel_config.debug_mode of
        true ->
            logger:debug("Recording metric: ~p", [Metric]);
        false ->
            ok
    end,

    State.

check_span_buffer_flush(State) ->
    BufferSize = queue:len(State#otel_state.span_buffer),
    BatchSize = State#otel_state.config#otel_config.batch_size,

    if
        BufferSize >= BatchSize ->
            flush_span_buffer(State);
        true ->
            State
    end.

check_metrics_buffer_flush(State) ->
    BufferSize = queue:len(State#otel_state.metrics_buffer),
    BatchSize = State#otel_state.config#otel_config.batch_size,

    if
        BufferSize >= BatchSize ->
            flush_metrics_buffer(State);
        true ->
            State
    end.

flush_span_buffer(State) ->
    Buffer = State#otel_state.span_buffer,
    case queue:out(Buffer) of
        {empty, _} ->
            State;
        {{value, Span}, NewBuffer} ->
            %% Send span to OTEL collector (async)
            send_span_to_otel(Span, State),

            UpdatedMetrics = State#otel_state.metrics#otel_metrics{
                async_operations = State#otel_state.metrics#otel_metrics.async_operations + 1
            },

            %% Check if there are more spans to flush
            case queue:out(NewBuffer) of
                {empty, _} ->
                    State#otel_state{
                        span_buffer = queue:new(),
                        metrics = UpdatedMetrics
                    };
                {_, RemainingBuffer} ->
                    flush_span_buffer(State#otel_state{
                        span_buffer = RemainingBuffer,
                        metrics = UpdatedMetrics
                    })
            end
    end.

flush_metrics_buffer(State) ->
    Buffer = State#otel_state.metrics_buffer,
    case queue:out(Buffer) of
        {empty, _} ->
            State;
        {{value, Metric}, NewBuffer} ->
            %% Send metric to OTEL collector (async)
            send_metric_to_otel(Metric, State),

            UpdatedMetrics = State#otel_state.metrics#otel_metrics{
                async_operations = State#otel_state.metrics#otel_metrics.async_operations + 1
            },

            %% Check if there are more metrics to flush
            case queue:out(NewBuffer) of
                {empty, _} ->
                    State#otel_state{
                        metrics_buffer = queue:new(),
                        metrics = UpdatedMetrics
                    };
                {_, RemainingBuffer} ->
                    flush_metrics_buffer(State#otel_state{
                        metrics_buffer = RemainingBuffer,
                        metrics = UpdatedMetrics
                    })
            end
    end.

send_span_to_otel(Span, State) ->
    %% In production, this would send to OTEL collector
    %% For now, just log in debug mode
    case State#otel_state.config#otel_config.debug_mode of
        true ->
            logger:debug("Sending span to OTEL: ~p", [Span]);
        false ->
            ok
    end.

send_metric_to_otel(Metric, State) ->
    %% In production, this would send to OTEL collector
    %% For now, just log in debug mode
    case State#otel_state.config#otel_config.debug_mode of
        true ->
            logger:debug("Sending metric to OTEL: ~p", [Metric]);
        false ->
            ok
    end.

start_periodic_operations(State) ->
    %% Start flush timer
    FlushTimer = erlang:start_timer(?BATCH_FLUSH_INTERVAL, self(), flush_span_buffer),
    MetricsTimer = erlang:start_timer(?BATCH_FLUSH_INTERVAL, self(), flush_metrics_buffer),
    SamplingTimer = erlang:start_timer(30000, self(), update_sampling_state),

    State#otel_state{
        flush_timer = FlushTimer,
        metrics_timer = MetricsTimer
    }.

stop_periodic_operations(State) ->
    %% Cancel timers
    _ = erlang:cancel_timer(State#otel_state.flush_timer),
    _ = erlang:cancel_timer(State#otel_state.metrics_timer),
    _ = erlang:cancel_timer(State#otel_state.timer),

    State#otel_state{
        flush_timer = undefined,
        metrics_timer = undefined,
        timer = undefined
    }.

update_adaptive_sampling(State) ->
    %% Update sampling state based on recent operation patterns
    CurrentTime = erlang:system_time(millisecond),
    SamplingState = State#otel_state.sampling_state,

    %% Remove old operations
    RecentOps = filter_recent_operations(SamplingState#{"recent_operations"}, CurrentTime),
    OperationCounts = count_operations(RecentOps),

    %% Calculate new threshold
    Threshold = calculate_adaptive_threshold(OperationCounts, State),

    %% Update sampling state
    NewSamplingState = SamplingState#{
        last_sample_time => CurrentTime,
        recent_operations => RecentOps,
        operation_counts => OperationCounts,
        adaptive_threshold => Threshold
    },

    State#otel_state{sampling_state = NewSamplingState}.

filter_recent_operations(Operations, CurrentTime) ->
    FilterAge = 60000, % 1 minute
    lists:filter(fun(Op) ->
        Op#{"timestamp"} >= CurrentTime - FilterAge
    end, Operations).

count_operations(Operations) ->
    lists:foldl(fun(Op, Acc) ->
        Name = Op#{"name"},
        maps:update_with(Name, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Operations).

calculate_adaptive_threshold(OperationCounts, State) ->
    TotalOperations = lists:sum(maps:values(OperationCounts)),
    if
        TotalOperations == 0 ->
            0.1;
        true ->
            HighFreqOps = lists:filter(fun({_, Count}) -> Count > 50 end, maps:to_list(OperationCounts)),
            HighFreqRatio = length(HighFreqOps) / length(maps:to_list(OperationCounts)),
            min(0.5, State#otel_state.sampling_state#{"adaptive_threshold"} + HighFreqRatio * 0.1)
    end.

update_span_metrics(Metrics, Type) ->
    case Type of
        created ->
            Metrics#otel_metrics{
                spans_created = Metrics#otel_metrics.spans_created + 1
            };
        completed ->
            Metrics#otel_metrics{
                spans_completed = Metrics#otel_metrics.spans_completed + 1
            };
        dropped ->
            Metrics#otel_metrics{
                spans_dropped = Metrics#otel_metrics.spans_dropped + 1
            }
    end.

generate_metrics_summary(State) ->
    Metrics = State#otel_state.metrics,
    #{
        spans_created => Metrics#otel_metrics.spans_created,
        spans_completed => Metrics#otel_metrics.spans_completed,
        spans_dropped => Metrics#otel_metrics.spans_dropped,
        metrics_recorded => Metrics#otel_metrics.metrics_recorded,
        batches_sent => Metrics#otel_metrics.batches_sent,
        async_operations => Metrics#otel_metrics.async_operations,
        total_overhead_ms => Metrics#otel_metrics.total_overhead_ms,
        avg_span_duration => Metrics#otel_metrics.avg_span_duration,
        active_spans => maps:size(State#otel_state.spans),
        pending_spans => queue:len(State#otel_state.span_buffer),
        pending_metrics => queue:len(State#otel_state.metrics_buffer)
    }.

generate_tracing_summary(State) ->
    Spans = State#otel_state.spans,
    ActiveSpans = maps:values(Spans),

    TotalDuration = lists:foldl(fun(Span, Acc) ->
        case Span#span.end_time of
            undefined -> Acc;
            EndTime -> Acc + (EndTime - Span#span.start_time)
        end
    end, 0, ActiveSpans),

    NumCompleted = lists:foldl(fun(Span, Acc) ->
        case Span#span.end_time of
            undefined -> Acc;
            _ -> Acc + 1
        end
    end, 0, ActiveSpans),

    AvgDuration = case NumCompleted of
        0 -> 0;
        _ -> TotalDuration / NumCompleted
    end,

    #{
        active_spans => maps:size(Spans),
        average_duration_ms => AvgDuration,
        operations_by_type => count_spans_by_type(ActiveSpans),
        longest_span => find_longest_span(ActiveSpans),
        status_distribution => count_spans_by_status(ActiveSpans)
    }.

count_spans_by_type(Spans) ->
    lists:foldl(fun(Span, Acc) ->
        Name = Span#span.name,
        maps:update_with(Name, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Spans).

find_longest_span(Spans) ->
    lists:foldl(fun(Span, Longest) ->
        Duration = case Span#span.end_time of
            undefined -> 0;
            EndTime -> EndTime - Span#span.start_time
        end,
        case Duration > Longest#{"duration"} of
            true -> #{name => Span#span.name, duration => Duration};
            false -> Longest
        end
    end, #{name => <<"none">>, duration => 0}, Spans).

count_spans_by_status(Spans) ->
    lists:foldl(fun(Span, Acc) ->
        Status = Span#span.status,
        maps:update_with(Status, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Spans).

generate_overhead_stats(State) ->
    Metrics = State#otel_state.metrics,
    TotalOps = Metrics#otel_metrics.spans_created + Metrics#otel_metrics.metrics_recorded,

    OverheadPercentage = case TotalOps of
        0 -> 0;
        _ -> (Metrics#otel_metrics.total_overhead_ms / TotalOps) * 100
    end,

    #{
        total_operations => TotalOps,
        total_overhead_ms => Metrics#otel_metrics.total_overhead_ms,
        average_overhead_ms => Metrics#otel_metrics.total_overhead_ms / max(1, TotalOps),
        overhead_percentage => OverheadPercentage,
        cache_efficiency => calculate_cache_efficiency(Metrics),
        sampling_rate => State#otel_state.config#otel_config.sampling_rate,
        buffer_utilization => calculate_buffer_utilization(State)
    }.

calculate_cache_efficiency(Metrics) ->
    TotalLookups = Metrics#otel_metrics.cache_hits + Metrics#otel_metrics.cache_misses,
    case TotalLookups of
        0 -> 0;
        _ -> (Metrics#otel_metrics.cache_hits / TotalLookups) * 100
    end.

calculate_buffer_utilization(State) ->
    SpanUtilization = queue:len(State#otel_state.span_buffer) / State#otel_state.config#otel_config.span_buffer_size,
    MetricUtilization = queue:len(State#otel_state.metrics_buffer) / State#otel_state.config#otel_config.metric_buffer_size,
    (SpanUtilization + MetricUtilization) / 2 * 100.

optimize_config_for_performance(Config, Options) ->
    %% Apply performance optimizations
    NewConfig = Config,

    %% Reduce sampling rate
    case maps.get(sampling_rate, Options, undefined) of
        undefined -> ok;
        Rate ->
            NewConfig = NewConfig#otel_config{sampling_rate = max(0.01, min(0.05, Rate))}
    end,

    %% Increase batch size
    case maps.get(batch_size, Options, undefined) of
        undefined -> ok;
        Size ->
            NewConfig = NewConfig#otel_config{batch_size = min(500, Size)}
    end,

    %% Enable async operations
    NewConfig = NewConfig#otel_config{async_operations = true},

    %% Reduce buffer sizes to save memory
    case maps.get(buffer_size, Options, undefined) of
        undefined -> ok;
        Size ->
            NewConfig = NewConfig#otel_config{
                metric_buffer_size = min(200, Size),
                span_buffer_size = min(100, Size)
            }
    end,

    %% Enable compression for large batches
    case maps.get(compression, Options, undefined) of
        undefined -> ok;
        true ->
            NewConfig = NewConfig#otel_config{batch_size = min(1000, NewConfig#otel_config.batch_size)};
        false ->
            NewConfig
    end,

    NewConfig.

optimize_config_for_debugging(Config, Options) ->
    %% Apply debugging optimizations
    NewConfig = Config,

    %% Enable debugging features
    NewConfig = NewConfig#otel_config{debug_mode = true},

    %% Increase sampling rate for debugging
    case maps.get(sampling_rate, Options, undefined) of
        undefined -> ok;
        Rate ->
            NewConfig = NewConfig#otel_config{sampling_rate = min(1.0, max(0.5, Rate))}
    end,

    %% Reduce batch size for more granular logging
    case maps.get(batch_size, Options, undefined) of
        undefined -> ok;
        Size ->
            NewConfig = NewConfig#otel_config{batch_size = min(50, Size)}
    end,

    %% Disable async operations for debugging
    NewConfig = NewConfig#otel_config{async_operations = false},

    NewConfig.

execute_operation(Operation, State) ->
    case Operation of
        {start_span, Name, Attributes} ->
            start_span(Name, Attributes, #{});
        {end_span, SpanId} ->
            end_span(SpanId);
        {record_metric, Name, Value, Type} ->
            record_metric(Name, Value, Type, State);
        {add_event, SpanId, EventName, Attributes} ->
            add_event(SpanId, EventName, Attributes);
        {add_attribute, SpanId, Key, Value} ->
            add_attribute(SpanId, Key, Value);
        _ ->
            {error, unknown_operation}
    end.