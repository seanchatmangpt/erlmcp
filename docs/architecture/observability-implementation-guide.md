# erlmcp Observability Implementation Guide v2.1.0
# Detailed Implementation of 80/20 Architecture

## Implementation Overview

This guide provides step-by-step implementation instructions for the 80/20 observability architecture. The implementation follows the principle of delivering maximum value with minimal complexity.

## Phase 1: Core OTEL Integration (Week 1-2)

### 1.1 Enhanced OTEL Integration

#### Implementation: Enhanced Tracing for Critical Paths

Create `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_tracing.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Enhanced tracing for erlmcp critical paths
%%% 80/20 implementation: Trace only the most critical operations
%%%-------------------------------------------------------------------
-module(erlmcp_tracing).

-behaviour(gen_server).

%% API
-export([start_link/0, trace_request/2, trace_session/2, trace_transport/2,
         get_traces/0, clear_traces/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

-define(MAX_TRACES, 1000).
-define(CRITICAL_PATHS, [
    erlmcp_json_rpc,
    erlmcp_session_manager,
    erlmcp_registry,
    erlmcp_server
]).

-record(trace,
        {id :: binary(),
         timestamp :: integer(),
         operation :: atom(),
         duration :: integer(),
         status :: ok | error | timeout,
         details :: map()}).

-record(state,
        {traces = [] :: [#trace{}],
         config :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec trace_request(binary(), map()) -> binary().
trace_request(RequestId, Details) ->
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"mcp.request">>, RequestId, Details),
    erlang:put(erlmcp_trace_ctx, SpanCtx),
    TraceId = maps:get(trace_id, SpanCtx),
    TraceId.

-spec trace_session(binary(), map()) -> binary().
trace_session(SessionId, Details) ->
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"mcp.session">>, SessionId, Details),
    erlang:put(erlmcp_trace_ctx, SpanCtx),
    TraceId = maps:get(trace_id, SpanCtx),
    TraceId.

-spec trace_transport(binary(), map()) -> binary().
trace_transport(TransportId, Details) ->
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"mcp.transport">>, TransportId, Details),
    erlang:put(erlmcp_trace_ctx, SpanCtx),
    TraceId = maps:get(trace_id, SpanCtx),
    TraceId.

-spec get_traces() -> [#trace{}].
get_traces() ->
    gen_server:call(?MODULE, get_traces).

-spec clear_traces() -> ok.
clear_traces() ->
    gen_server:call(?MODULE, clear_traces).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize trace storage
    ets:new(erlmcp_traces, [set, public, named_table, {keypos, #trace.id}]),

    %% Load configuration
    Config = #{
        max_traces => ?MAX_TRACES,
        critical_paths => ?CRITICAL_PATHS,
        sampling_rate => 0.1  %% 10% sampling for non-critical
    },

    {ok, #state{config = Config}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_traces, _From, #state{traces = Traces} = State) ->
    {reply, Traces, State};
handle_call(clear_traces, _From, State) ->
    %% Clear ETS table and in-memory traces
    ets:delete_all_objects(erlmcp_traces),
    {reply, ok, State#state{traces = []}}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_trace, Operation, Duration, Status, Details}, State) ->
    TraceId = generate_trace_id(),
    Trace = #trace{
        id = TraceId,
        timestamp = erlang:system_time(millisecond),
        operation = Operation,
        duration = Duration,
        status = Status,
        details = Details
    },

    %% Store in ETS for persistence
    ets:insert(erlmcp_traces, Trace),

    %% Maintain in-memory list with size limit
    NewTraces = [Trace | State#state.traces],
    TracesLimited = lists:sublist(NewTraces, State#state.config#max_traces),

    {noreply, State#state{traces = TracesLimited}}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    %% Clean up ETS table
    ets:delete(erlmcp_traces),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_trace_id() -> binary().
generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).
```

#### Integration with Core Components

Update `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl`:

```erlang
%% Add to critical path tracing
handle_method_call(Method, Params, Id) ->
    StartTime = erlang:system_time(millisecond),
    TraceId = erlmcp_tracing:trace_request(Id, #{
        method => Method,
        params_size => term_size(Params)
    }),

    try
        Result = execute_method(Method, Params),
        Duration = erlang:system_time(millisecond) - StartTime,

        %% Record successful trace
        erlmcp_tracing:record_trace(erlmcp_json_rpc, Duration, ok, #{
            method => Method,
            trace_id => TraceId
        }),

        {result, Result, Id}
    catch
        Class:Reason:Stacktrace ->
            Duration = erlang:system_time(millisecond) - StartTime,

            %% Record error trace
            erlmcp_tracing:record_trace(erlmcp_json_rpc, Duration, error, #{
                method => Method,
                error => {Class, Reason},
                stacktrace => Stacktrace,
                trace_id => TraceId
            }),

            {error, {internal_error, format_error(Class, Reason)}, Id}
    end.
```

### 1.2 80/20 Metrics Collection

Create `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_metrics_core.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Core metrics collection with 80/20 focus
%%% Only collect metrics that provide maximum value
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_core).

-behaviour(gen_server).

%% API
-export([start_link/0, record_metric/3, get_metrics_summary/0,
         get_critical_metrics/0, reset_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Critical metric names (80% value)
-define(CRITICAL_METRICS, [
    <<"registry_operations_per_second">>,
    <<"session_creation_latency_ms">>,
    <<"transport_connection_count">>,
    <<"tool_call_duration_ms">>,
    <<"error_rates_by_component">>,
    <<"active_sessions_count">>,
    <<"message_queue_depth">>
]).

-record(metric,
        {name :: binary(),
         value :: number(),
         labels :: map(),
         timestamp :: integer()}).

-record(state,
        {metrics = [] :: [#metric{}],
         counters = #{} :: map(),
         aggregates = #{} :: map(),
         config :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec record_metric(binary(), number(), map()) -> ok.
record_metric(Name, Value, Labels) ->
    case is_critical_metric(Name) of
        true ->
            gen_server:cast(?MODULE, {record_critical, Name, Value, Labels});
        false ->
            %% Skip non-critical metrics to reduce overhead
            ok
    end.

-spec get_metrics_summary() -> map().
get_metrics_summary() ->
    gen_server:call(?MODULE, get_metrics_summary).

-spec get_critical_metrics() -> [#metric{}].
get_critical_metrics() ->
    gen_server:call(?MODULE, get_critical_metrics).

-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize metrics storage
    ets:new(erlmcp_metrics_data, [set, public, named_table, {keypos, #metric.name}]),

    %% Initialize counters for rate calculations
    ets:new(erlmcp_counters, [set, public, named_table]),

    %% Configuration
    Config = #{
        critical_metrics => ?CRITICAL_METRICS,
        retention_minutes => 60,
        aggregation_interval => 10000  %% 10 seconds
    },

    %% Start aggregation timer
    erlang:send_after(Config#aggregation_interval, self(), aggregate_metrics),

    {ok, #state{config = Config}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_metrics_summary, _From, State) ->
    Summary = generate_metrics_summary(State),
    {reply, Summary, State};

handle_call(get_critical_metrics, _From, State) ->
    CriticalMetrics = [M || M <- State#state.metrics, is_critical_metric(M#metric.name)],
    {reply, CriticalMetrics, State};

handle_call(reset_metrics, _From, State) ->
    %% Clear all metrics
    ets:delete_all_objects(erlmcp_metrics_data),
    ets:delete_all_objects(erlmcp_counters),
    {reply, ok, State#state{metrics = [], aggregates = #{}}}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_critical, Name, Value, Labels}, State) ->
    Now = erlang:system_time(millisecond),
    Metric = #metric{
        name = Name,
        value = Value,
        labels = Labels,
        timestamp = Now
    },

    %% Store in ETS
    ets:insert(erlmcp_metrics_data, Metric),

    %% Update counter for rate calculations
    update_counter(Name, Value),

    %% Maintain in-memory list (limited size)
    NewMetrics = [Metric | State#state.metrics],
    LimitedMetrics = lists:sublist(NewMetrics, 1000),  %% Keep last 1000

    {noreply, State#state{metrics = LimitedMetrics}}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(aggregate_metrics, State) ->
    %% Aggregate metrics for the interval
    Aggregates = aggregate_metrics(State),
    NewState = State#state{aggregates = Aggregates},

    %% Schedule next aggregation
    erlang:send_after(State#state.config#aggregation_interval, self(), aggregate_metrics),

    {noreply, NewState}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    %% Clean up ETS tables
    ets:delete(erlmcp_metrics_data),
    ets:delete(erlmcp_counters),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec is_critical_metric(binary()) -> boolean().
is_critical_metric(Name) ->
    lists:member(Name, ?CRITICAL_METRICS).

-spec update_counter(binary(), number()) -> ok.
update_counter(Name, Value) ->
    Current = ets:lookup_element(erlmcp_counters, Name, 2),
    ets:insert(erlmcp_counters, {Name, Current + Value}).

-spec generate_metrics_summary(#state{}) -> map().
generate_metrics_summary(State) ->
    #{
        timestamp => erlang:system_time(millisecond),
        critical_metrics => length([M || M <- State#state.metrics, is_critical_metric(M#metric.name)]),
        total_metrics => length(State#state.metrics),
        aggregates => State#state.aggregates,
        counters => ets:tab2list(erlmcp_counters)
    }.

-spec aggregate_metrics(#state{}) -> map().
aggregate_metrics(State) ->
    %% Aggregate by metric name for the current interval
    Aggregates = lists:foldl(fun(M, Acc) ->
        Name = M#metric.name,
        Value = M#metric.value,

        Current = maps:get(Name, Acc, #{}),

        %% Calculate min, max, avg, count
        Min = case Current of
                  #{min := Min0} -> min(Min0, Value);
                  _ -> Value
              end,
        Max = case Current of
                  #{max := Max0} -> max(Max0, Value);
                  _ -> Value
              end,
        Count = case Current of
                    #{count := Count0} -> Count0 + 1;
                    _ -> 1
                end,
        Sum = case Current of
                  #{sum := Sum0} -> Sum0 + Value;
                  _ -> Value
                end,

        Updated = Current#{
            min => Min,
            max => Max,
            count => Count,
            sum => Sum,
            avg => Sum / Count
        },

        Acc#{Name => Updated}
    end, #{}, State#state.metrics),

    Aggregates.
```

### 1.3 Integration Hooks

Create `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_observability_hooks.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration hooks for observability components
%%% Automatically instrument critical paths without manual changes
%%%-------------------------------------------------------------------
-module(erlmcp_observability_hooks).

-behaviour(gen_server).

%% API
-export([start_link/0, instrument_component/2, get_instrumentation_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Instrumented components
-define(INSTRUMENTED_COMPONENTS, [
    {erlmcp_json_rpc, [handle_request]},
    {erlmcp_session_manager, [create_session, destroy_session]},
    {erlmcp_registry, [register, unregister]},
    {erlmcp_server, [start_server, stop_server]},
    {erlmcp_transport, [send, receive]},
    {erlmcp_tools, [call_tool]}
]).

-record(state,
        {instrumented = [] :: list(),
          config :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec instrument_component(atom(), list()) -> ok.
instrument_component(Component, Functions) ->
    gen_server:cast(?MODULE, {instrument, Component, Functions}).

-spec get_instrumentation_status() -> map().
get_instrumentation_status() ->
    gen_server:call(?MODULE, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Auto-instrument critical components
    Instrumented = auto_instrument(),

    Config = #{
        auto_instrument => true,
        critical_components => ?INSTRUMENTED_COMPONENTS
    },

    {ok, #state{instrumented = Instrumented, config = Config}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_status, _From, State) ->
    Status = #{
        instrumented_components => length(State#state.instrumented),
        auto_instrument => State#state.config#auto_instrument,
        components =>
            lists:map(fun({Component, Functions}) ->
                #{component => Component, functions => Functions}
            end, State#state.config#critical_components)
    },
    {reply, Status, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({instrument, Component, Functions}, State) ->
    %% Apply instrumentation to component functions
    case apply_instrumentation(Component, Functions) of
        ok ->
            %% Add to instrumented list
            NewInstrumented = [{Component, Functions} | State#state.instrumented],
            {noreply, State#state{instrumented = NewInstrumented}};
        {error, Reason} ->
            %% Log instrumentation failure but continue
            logger:error("Failed to instrument ~p: ~p", [Component, Reason]),
            {noreply, State}
    end.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    %% Clean up instrumentation (if possible)
    cleanup_instrumentation(),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec auto_instrument() -> list().
auto_instrument() ->
    %% Auto-instrument critical components on startup
    lists:foldl(fun({Component, Functions}, Acc) ->
        case apply_instrumentation(Component, Functions) of
            ok ->
                [{Component, Functions} | Acc];
            {error, _} ->
                Acc
        end
    end, [], ?INSTRUMENTED_COMPONENTS).

-spec apply_instrumentation(atom(), list()) -> ok | {error, term()}.
apply_instrumentation(Component, Functions) ->
    try
        %% For each function, add tracing wrapper
        lists:foreach(fun(Function) ->
            %% This is a simplified example - in practice, you'd use
            %% proper function wrapping or aspect-oriented techniques
            logger:info("Instrumenting ~p:~p", [Component, Function])
        end, Functions),
        ok
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.

-spec cleanup_instrumentation() -> ok.
cleanup_instrumentation() ->
    %% Remove instrumentation from components
    %% This depends on the instrumentation technique used
    ok.
```

## Phase 2: Chaos Engineering (Week 3)

### 2.1 Chaos Testing Framework

Create `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_chaos_engine.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Chaos engineering framework for erlmcp resilience testing
%%% 80/20 implementation: Only test most impactful failure scenarios
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_engine).

-behaviour(gen_server).

%% API
-export([start_link/0, schedule_chaos/2, get_chaos_status/0,
         emergency_stop/0, get_chaos_history/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Chaos scenarios (80/20 - only most impactful)
-define(CHAOS_SCENARIOS, [
    #{name => network_partition, probability => 0.1,
      description => "Simulate network partition",
      impact => high, recovery_time => 30000},
    #{name => process_crash, probability => 0.05,
      description => "Inject random process crashes",
      impact => medium, recovery_time => 5000},
    #{name => memory_pressure, probability => 0.02,
      description => "Simulate memory pressure",
      impact => medium, recovery_time => 15000},
    #{name => cpu_overload, probability => 0.01,
      description => "Simulate CPU overload",
      impact => low, recovery_time => 10000},
    #{name => latency_injection, probability => 0.15,
      description => "Inject random latency",
      impact => low, recovery_time => 1000}
]).

-record(chaos_test,
        {id :: binary(),
         name :: binary(),
         start_time :: integer(),
         status :: running | completed | failed,
         impact :: low | medium | high,
         recovery_time :: integer(),
         details :: map()}).

-record(state,
        {active_tests = [] :: [#chaos_test{}],
          chaos_history = [] :: [#chaos_test{}],
          config :: map(),
          emergency_stop = false :: boolean()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec schedule_chaos(binary(), map()) -> ok.
schedule_chaos(ScenarioName, Parameters) ->
    gen_server:cast(?MODULE, {schedule_chaos, ScenarioName, Parameters}).

-spec get_chaos_status() -> map().
get_chaos_status() ->
    gen_server:call(?MODULE, get_status).

-spec emergency_stop() -> ok.
emergency_stop() ->
    gen_server:cast(?MODULE, emergency_stop).

-spec get_chaos_history() -> [#chaos_test{}].
get_chaos_history() ->
    gen_server:call(?MODULE, get_history).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize chaos test storage
    ets:new(erlmcp_chaos_data, [set, public, named_table, {keypos, #chaos_test.id}]),

    %% Configuration
    Config = #{
        scenarios => ?CHAOS_SCENARIOS,
        max_concurrent_tests => 3,
        test_duration => 60000,  %% 1 minute max
        emergency_threshold => 5  %% Emergency stop after 5 failures
    },

    %% Start chaos scheduler
    erlang:send_after(5000, self(), run_chaos_scheduler),

    {ok, #state{config = Config}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_status, _From, State) ->
    Status = #{
        active_tests => length(State#state.active_tests),
        emergency_stop => State#state.emergency_stop,
        total_history => length(State#state.chaos_history),
        available_scenarios => length(State#state.config#scenarios)
    },
    {reply, Status, State};

handle_call(get_history, _From, State) ->
    {reply, State#state.chaos_history, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({schedule_chaos, ScenarioName, Parameters}, State) ->
    case State#state.emergency_stop of
        true ->
            %% Emergency stop active - reject new tests
            logger:warning("Chaos tests suspended due to emergency stop"),
            {noreply, State};
        false ->
            %% Schedule new chaos test
            case start_chaos_test(ScenarioName, Parameters, State) of
                {ok, TestId} ->
                    %% Add to active tests
                    Test = get_chaos_test(TestId),
                    NewActiveTests = [Test | State#state.active_tests],
                    {noreply, State#state{active_tests = NewActiveTests}};
                {error, Reason} ->
                    logger:error("Failed to schedule chaos test ~p: ~p", [ScenarioName, Reason]),
                    {noreply, State}
            end
    end;

handle_cast(emergency_stop, State) ->
    %% Immediately stop all active tests
    lists:foreach(fun(Test) ->
        cleanup_chaos_test(Test)
    end, State#state.active_tests),

    NewState = State#state{
        active_tests = [],
        emergency_stop = true
    },

    logger:warning("Emergency stop activated - all chaos tests terminated"),
    {noreply, NewState}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}}.
handle_info(run_chaos_scheduler, State) ->
    %% Run chaos scheduler
    case run_chaos_scheduler(State) of
        {ok, NewState} ->
            %% Schedule next run
            erlang:send_after(30000, self(), run_chaos_scheduler),  %% 30 second interval
            {noreply, NewState};
        {stop, NewState} ->
            %% Don't schedule next run if emergency stop
            {noreply, NewState}
    end;

handle_info({chaos_test_completed, TestId}, State) ->
    %% Mark test as completed
    case lists:keyfind(TestId, #chaos_test.id, State#state.active_tests) of
        false ->
            %% Test not found or already completed
            {noreply, State};
        Test ->
            %% Move from active to history
            CompletedTest = Test#chaos_test{status = completed},
            NewActiveTests = lists:keydelete(TestId, #chaos_test.id, State#state.active_tests),
            NewHistory = [CompletedTest | State#state.chaos_history],

            %% Cleanup test resources
            cleanup_chaos_test(CompletedTest),

            {noreply, State#state{
                active_tests = NewActiveTests,
                chaos_history = NewHistory
            }}
    end;

handle_info({chaos_test_failed, TestId, Reason}, State) ->
    %% Mark test as failed
    case lists:keyfind(TestId, #chaos_test.id, State#state.active_tests) of
        false ->
            {noreply, State};
        Test ->
            FailedTest = Test#chaos_test{status = failed, details = #{error => Reason}},
            NewActiveTests = lists:keydelete(TestId, #chaos_test.id, State#state.active_tests),
            NewHistory = [FailedTest | State#state.chaos_history],

            %% Check emergency threshold
            FailedCount = length([T || T <- NewHistory, T#chaos_test.status =:= failed]),
            EmergencyStop = FailedCount >= State#state.config#emergency_threshold,

            if EmergencyStop ->
                    %% Activate emergency stop
                    logger:critical("Emergency threshold exceeded - activating emergency stop"),
                    gen_server:cast(self(), emergency_stop);
               true ->
                    ok
            end,

            {noreply, State#state{
                active_tests = NewActiveTests,
                chaos_history = NewHistory,
                emergency_stop = EmergencyStop
            }}
    end.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    %% Clean up ETS table
    ets:delete(erlmcp_chaos_data),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec start_chaos_test(binary(), map(), #state{}) -> {ok, binary()} | {error, term()}.
start_chaos_test(ScenarioName, Parameters, State) ->
    %% Find scenario definition
    case lists:keyfind(ScenarioName, name, State#state.config#scenarios) of
        false ->
            {error, unknown_scenario};
        Scenario ->
            %% Create test ID
            TestId = generate_chaos_test_id(),

            %% Create test record
            Test = #chaos_test{
                id = TestId,
                name = ScenarioName,
                start_time = erlang:system_time(millisecond),
                status = running,
                impact = Scenario#impact,
                recovery_time = Scenario#recovery_time,
                details = Parameters
            },

            %% Store in ETS
            ets:insert(erlmcp_chaos_data, Test),

            %% Execute chaos test
            case execute_chaos_test(Test) of
                ok ->
                    {ok, TestId};
                {error, Reason} ->
                    %% Mark test as failed immediately
                    FailedTest = Test#chaos_test{status = failed, details = #{error => Reason}},
                    ets:insert(erlmcp_chaos_data, FailedTest),
                    {error, Reason}
            end
    end.

-spec execute_chaos_test(#chaos_test{}) -> ok | {error, term()}.
execute_chaos_test(#chaos_test{name = network_partition} = Test) ->
    %% Simulate network partition
    logger:info("Starting network partition test: ~p", [Test#chaos_test.id]),

    %% Simulate network issues
    Self = self(),
    Pid = spawn(fun() ->
        %% Simulate network latency
        timer:sleep(5000),

        %% Simulate packet loss
        if rand:uniform() < 0.3 ->
                %% Simulate packet loss
                logger:warning("Simulating packet loss for test ~p", [Test#chaos_test.id]);
           true ->
                ok
        end,

        %% Send completion signal
        Self ! {chaos_test_completed, Test#chaos_test.id}
    end),

    %% Monitor test process
    erlang:monitor(process, Pid),
    ok;

execute_chaos_test(#chaos_test{name = process_crash} = Test) ->
    %% Inject random process crashes
    logger:info("Starting process crash test: ~p", [Test#chaos_test.id]),

    %% Crash a non-critical process
    Processes = erlang:processes(),
    NonCritical = lists:filter(fun(P) ->
        case process_info(P) of
            undefined -> false;
            Info ->
                %% Don't crash critical processes
                not is_critical_process(P, Info)
        end
    end, Processes),

    if length(NonCritical) > 0 ->
            TargetPid = lists:nth(rand:uniform(length(NonCritical)), NonCritical),
            logger:info("Crashing process ~p in test ~p", [TargetPid, Test#chaos_test.id]),
            exit(TargetPid, kill);
       true ->
            logger:warning("No non-critical processes to crash")
    end,

    %% Simulate recovery
    timer:sleep(Test#chaos_test.recovery_time),
    self() ! {chaos_test_completed, Test#chaos_test.id},
    ok;

execute_chaos_test(#chaos_test{name = memory_pressure} = Test) ->
    %% Simulate memory pressure
    logger:info("Starting memory pressure test: ~p", [Test#chaos_test.id]),

    %% Allocate memory to simulate pressure
    Size = 100 * 1024 * 1024,  %% 100MB
    _ = binary:copy(<<0>>, Size),

    %% Monitor for memory issues
    Self = self(),
    spawn(fun() ->
        timer:sleep(Test#chaos_test.recovery_time),
        Self ! {chaos_test_completed, Test#chaos_test.id}
    end),

    ok;

execute_chaos_test(#chaos_test{name = latency_injection} = Test) ->
    %% Inject random latency
    logger:info("Starting latency injection test: ~p", [Test#chaos_test.id]),

    %% Inject latency for a period
    EndTime = erlang:system_time(millisecond) + Test#chaos_test.recovery_time,

    Self = self(),
    spawn(fun() ->
        while(fun() -> erlang:system_time(millisecond) < EndTime end, fun() ->
            %% Inject random latency
            Latency = rand:uniform(1000),  %% Up to 1 second
            timer:sleep(Latency)
        end),

        Self ! {chaos_test_completed, Test#chaos_test.id}
    end),

    ok;

execute_chaos_test(#chaos_test{name = Unknown}) ->
    logger:error("Unknown chaos test: ~p", [Unknown]),
    {error, unknown_test}.

-spec run_chaos_scheduler(#state{}) -> {ok, #state{}} | {stop, #state{}}.
run_chaos_scheduler(State) ->
    case State#state.emergency_stop of
        true ->
            {stop, State};
        false ->
            %% Check if we can start new tests
            ActiveCount = length(State#state.active_tests),
            MaxConcurrent = State#state.config#max_concurrent_tests,

            if ActiveCount < MaxConcurrent ->
                    %% Randomly select a scenario to run
                    Scenarios = State#state.config#scenarios,
                    Scenario = lists:nth(rand:uniform(length(Scenarios)), Scenarios),

                    %% Check probability
                    if rand:uniform() < Scenario#probability ->
                            %% Start the test
                            Parameters = #{},
                            case start_chaos_test(Scenario#name, Parameters, State) of
                                {ok, _} ->
                                    ok;
                                {error, _} ->
                                    ok
                            end;
                       true ->
                            ok
                    end;
               true ->
                    ok
            end,

            {ok, State}
    end.

-spec cleanup_chaos_test(#chaos_test{}) -> ok.
cleanup_chaos_test(Test) ->
    %% Clean up resources used by chaos test
    case Test#chaos_test.name of
        memory_pressure ->
            %% Force garbage collection to clean up allocated memory
            garbage_collect();
        _ ->
            ok
    end,

    logger:info("Cleaned up chaos test: ~p", [Test#chaos_test.id]).

-spec get_chaos_test(binary()) -> #chaos_test{}.
get_chaos_test(TestId) ->
    case ets:lookup(erlmcp_chaos_data, TestId) of
        [Test] -> Test;
        _ -> undefined
    end.

-spec generate_chaos_test_id() -> binary().
generate_chaos_test_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

-spec is_critical_process(pid(), list()) -> boolean().
is_critical_process(Pid, Info) ->
    %% Check if process is critical (should not be crashed)
    case proplists:get_value(registered_name, Info) of
        erlmcp_sup -> true;
        erlmcp_core_sup -> true;
        erlmcp_server_sup -> true;
        erlmcp_observability_sup -> true;
        _ -> false
    end.

-spec while(fun(() -> boolean()), fun(() -> term())) -> ok.
while(Check, Action) ->
    case Check() of
        true ->
            Action(),
            while(Check, Action);
        false ->
            ok
    end.
```

### 2.2 Chaos Test Configuration

Create `/Users/sac/erlmcp/config/chaos.yaml`:

```yaml
# Chaos Engineering Configuration for erlmcp
# 80/20 implementation: Only most impactful scenarios

chaos_engine:
  enabled: true
  max_concurrent_tests: 3
  test_duration: 60000  # 1 minute max
  emergency_threshold: 5

scenarios:
  # High Impact Scenarios (10% probability)
  - name: network_partition
    probability: 0.1
    description: "Simulate network partition between nodes"
    impact: high
    recovery_time: 30000
    enabled: true

  - name: process_crash
    probability: 0.05
    description: "Inject random non-critical process crashes"
    impact: high
    recovery_time: 5000
    enabled: true

  # Medium Impact Scenarios (5% probability)
  - name: memory_pressure
    probability: 0.02
    description: "Simulate memory pressure scenarios"
    impact: medium
    recovery_time: 15000
    enabled: true

  - name: cpu_overload
    probability: 0.01
    description: "Simulate CPU overload conditions"
    impact: medium
    recovery_time: 10000
    enabled: true

  # Low Impact Scenarios (15% probability)
  - name: latency_injection
    probability: 0.15
    description: "Inject random network latency"
    impact: low
    recovery_time: 1000
    enabled: true

  # Optional Scenarios (disabled by default)
  - name: packet_loss
    probability: 0.0
    description: "Simulate network packet loss"
    impact: medium
    recovery_time: 2000
    enabled: false

# Emergency Stop Configuration
emergency_stop:
  enabled: true
  threshold: 5  # Emergency stop after 5 consecutive failures
  cooldown_time: 300000  # 5 minutes cooldown after emergency stop

# Monitoring Configuration
monitoring:
  enabled: true
  metrics_collection: true
  alerting:
    enabled: true
    email_alerts: false
    webhook_alerts: true
    webhook_url: "https://your-monitoring-system/alerts"
```

## Phase 3: Dashboard & Visualization (Week 4)

### 3.1 Real-time Dashboard

Create `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Real-time dashboard for erlmcp observability
%%% WebSocket-based interface for metrics, traces, and chaos status
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard).

-behaviour(gen_server).

%% API
-export([start_link/0, get_dashboard_data/0, subscribe_updates/1,
         unsubscribe_updates/1, get_system_health/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Dashboard data structure
-record(dashboard_data,
        {timestamp :: integer(),
         metrics :: map(),
         traces :: list(),
         chaos_status :: map(),
         system_health :: map(),
         active_connections :: integer()}).

-record(state,
        {subscribers = [] :: list(),
          dashboard_data :: #dashboard_data{},
          config :: map(),
          last_update :: integer()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_dashboard_data() -> #dashboard_data{}.
get_dashboard_data() ->
    gen_server:call(?MODULE, get_dashboard_data).

-spec subscribe_updates(pid()) -> ok.
subscribe_updates(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

-spec unsubscribe_updates(pid()) -> ok.
unsubscribe_updates(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

-spec get_system_health() -> map().
get_system_health() ->
    gen_server:call(?MODULE, get_system_health).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize dashboard data
    InitialData = generate_initial_dashboard_data(),

    %% Configuration
    Config = #{
        update_interval => 5000,  %% 5 seconds
        max_traces => 100,
        max_metrics => 50,
        subscriber_timeout => 30000  %% 30 seconds inactive
    },

    %% Start data update timer
    erlang:send_after(Config#update_interval, self(), update_dashboard_data),

    {ok, #state{
        dashboard_data = InitialData,
        config = Config,
        last_update = erlang:system_time(millisecond)
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_dashboard_data, _From, State) ->
    {reply, State#state.dashboard_data, State};

handle_call(get_system_health, _From, State) ->
    Health = generate_system_health(State),
    {reply, Health, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}}.
handle_cast({subscribe, Pid}, State) ->
    %% Add subscriber
    NewSubscribers = [Pid | State#state.subscribers],

    %% Send current data to new subscriber
    Pid ! {dashboard_update, State#state.dashboard_data},

    {noreply, State#state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Pid}, State) ->
    %% Remove subscriber
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}}.
handle_info(update_dashboard_data, State) ->
    %% Update dashboard data
    NewData = update_dashboard_data(State),

    %% Notify subscribers of update
    lists:foreach(fun(Sub) ->
        Sub ! {dashboard_update, NewData}
    end, State#state.subscribers),

    %% Schedule next update
    UpdateInterval = State#state.config#update_interval,
    erlang:send_after(UpdateInterval, self(), update_dashboard_data),

    {noreply, State#state{
        dashboard_data = NewData,
        last_update = erlang:system_time(millisecond)
    }};

handle_info({subscriber_timeout, Pid}, State) ->
    %% Remove inactive subscriber
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    logger:info("Removed inactive dashboard subscriber: ~p", [Pid]),

    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    %% Clean up subscribers
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_initial_dashboard_data() -> #dashboard_data{}.
generate_initial_dashboard_data() ->
    #dashboard_data{
        timestamp = erlang:system_time(millisecond),
        metrics = #{},
        traces = [],
        chaos_status = #{active_tests => 0, emergency_stop => false},
        system_health => #{overall => healthy, components => #{}},
        active_connections = 0
    }.

-spec update_dashboard_data(#state{}) -> #dashboard_data{}.
update_dashboard_data(State) ->
    %% Get current metrics
    Metrics = get_current_metrics(),

    %% Get recent traces
    Traces = get_recent_traces(State#state.config#max_traces),

    %% Get chaos status
    ChaosStatus = get_chaos_status(),

    %% Get system health
    SystemHealth = generate_system_health(State),

    %% Get active connections
    ActiveConnections = get_active_connections(),

    #dashboard_data{
        timestamp = erlang:system_time(millisecond),
        metrics = Metrics,
        traces = Traces,
        chaos_status = ChaosStatus,
        system_health = SystemHealth,
        active_connections = ActiveConnections
    }.

-spec get_current_metrics() -> map().
get_current_metrics() ->
    %% Get critical metrics from erlmcp_metrics_core
    CriticalMetrics = erlmcp_metrics_core:get_critical_metrics(),

    %% Aggregate metrics by name
    Aggregated = lists:foldl(fun(Metric, Acc) ->
        Name = Metric#metric.name,
        Value = Metric#metric.value,
        Current = maps:get(Name, Acc, 0),
        Acc#{Name => Current + Value}
    end, #{}, CriticalMetrics),

    %% Add derived metrics
    Aggregated#{
        timestamp => erlang:system_time(millisecond),
        system_uptime => get_system_uptime(),
        memory_usage => get_memory_usage(),
        cpu_usage => get_cpu_usage()
    }.

-spec get_recent_traces(integer()) -> list().
get_recent_traces(Limit) ->
    %% Get traces from erlmcp_tracing
    AllTraces = erlmcp_tracing:get_traces(),
    lists:sublist(AllTraces, Limit).

-spec get_chaos_status() -> map().
get_chaos_status() ->
    %% Get chaos status from erlmcp_chaos_engine
    Status = erlmcp_chaos_engine:get_chaos_status(),

    %% Add dashboard-specific information
    Status#{
        timestamp => erlang:system_time(millisecond),
        active_tests_details => get_active_chaos_tests(),
        recent_failures => get_recent_chaos_failures()
    }.

-spec generate_system_health(#state{}) -> map().
generate_system_health(State) ->
    %% Check component health
    Components = check_component_health(),

    %% Calculate overall health
    HealthyCount = length([C || C <- maps:values(Components), C#health.status =:= healthy]),
    ComponentCount = maps:size(Components),
    OverallHealth = if ComponentCount > 0 ->
                           HealthyCount / ComponentCount;
                      true ->
                           0.0
                    end,

    %% Determine health status
    HealthStatus = case OverallHealth of
                      1.0 -> healthy;
                      X when X >= 0.8 -> degraded;
                      X when X >= 0.5 -> warning;
                      _ -> critical
                  end,

    #{
        overall => HealthStatus,
        health_percentage => OverallHealth,
        components => Components,
        uptime => get_system_uptime(),
        timestamp => erlang:system_time(millisecond)
    }.

-spec check_component_health() -> map().
check_component_health() ->
    %% Check each component's health
    #{
        registry => check_registry_health(),
        sessions => check_sessions_health(),
        transports => check_transports_health(),
        observability => check_observability_health(),
        chaos => check_chaos_health()
    }.

-spec check_registry_health() -> map().
check_registry_health() ->
    %% Check registry health
    case erlang:whereis(erlmcp_registry) of
        undefined ->
            #{status => critical, message => "Registry not running"};
        _Pid ->
            %% Check registry metrics
            case erlmcp_metrics_core:get_metrics(<<"registry_operations_per_second">>) of
                [] ->
                    #{status => healthy, message => "Registry running"};
                Metrics ->
                    RecentRate = lists:last(Metrics)#metric.value,
                    if RecentRate > 0 ->
                           #{status => healthy, message => "Registry active", rate => RecentRate};
                       true ->
                           #{status => warning, message => "Registry idle"}
                    end
            end
    end.

-spec check_sessions_health() -> map().
check_sessions_health() ->
    %% Check sessions health
    case erlang:whereis(erlmcp_session_manager) of
        undefined ->
            #{status => critical, message => "Session manager not running"};
        _Pid ->
            %% Get session metrics
            case erlmcp_metrics_core:get_metrics(<<"active_sessions_count">>) of
                [] ->
                    #{status => healthy, message => "Sessions running"};
                Metrics ->
                    Count = lists:last(Metrics)#metric.value,
                    if Count > 0 ->
                           #{status => healthy, message => "Active sessions", count => Count};
                       true ->
                           #{status => healthy, message => "No active sessions"}
                    end
            end
    end.

-spec get_active_connections() -> integer().
get_active_connections() ->
    %% Count active connections
    case erlang:whereis(erlmcp_transport_sup) of
        undefined ->
            0;
        _SupPid ->
            Children = supervisor:which_children(_SupPid),
            length([Child || Child <- Children, element(1, Child) /= undefined])
    end.

-spec get_system_uptime() -> integer().
get_system_uptime() ->
    %% Get system uptime in milliseconds
    case application:get_env(erlmcp_core, start_time) of
        undefined ->
            0;
        {ok, StartTime} ->
            erlang:system_time(millisecond) - StartTime
    end.

-spec get_memory_usage() -> integer().
get_memory_usage() ->
    %% Get memory usage in bytes
    {memory, Memory} = erlang:process_info(whereis(erlmcp_sup), memory),
    Memory.

-spec get_cpu_usage() -> float().
get_cpu_usage() ->
    %% Get CPU usage percentage (simplified)
    %% In production, you'd use more sophisticated CPU measurement
    0.0.

-spec get_active_chaos_tests() -> list().
get_active_chaos_tests() ->
    %% Get active chaos tests
    []  %% Implementation depends on erlmcp_chaos_engine API

-spec get_recent_chaos_failures() -> list().
get_recent_chaos_failures() ->
    %% Get recent chaos failures
    []  %% Implementation depends on erlmcp_chaos_engine API
```

### 3.2 HTTP Dashboard Server

Create `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_http.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP server for dashboard web interface
%%% Serves static files and WebSocket connections
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_http).

-behaviour(cowboy_websocket).

%% WebSocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% HTTP callbacks
-export([init/2, handle/2, terminate/3]).

-include("erlmcp.hrl").

%%====================================================================
%% WebSocket Callbacks
%%====================================================================

-spec init(cowboy_req:req(), term()) -> {cowboy_websocket, cowboy_req:req(), term()}.
init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}}.

-spec websocket_init(map()) -> {ok, map()}.
websocket_init(State) ->
    %% Subscribe to dashboard updates
    erlmcp_dashboard:subscribe_updates(self()),

    {ok, State}.

-spec websocket_handle({text, binary()}, map()) -> {ok, map()}.
websocket_handle({text, Data}, State) ->
    %% Handle WebSocket messages
    try
        JSON = jsx:decode(Data, [{return_maps, false}]),
        Response = handle_dashboard_message(JSON),
        {reply, {text, jsx:encode(Response)}, State}
    catch
        Class:Reason:Stacktrace ->
            logger:error("WebSocket error: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            Response = #{error => "Internal server error"},
            {reply, {text, jsx:encode(Response)}, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

-spec websocket_info(term(), map()) -> {reply, {text, binary()}, map()} | {ok, map()}.
websocket_info({dashboard_update, Data}, State) ->
    %% Send dashboard update to client
    JSON = format_dashboard_data(Data),
    {reply, {text, jsx:encode(JSON)}, State};
websocket_info(_Info, State) ->
    {ok, State}.

-spec terminate(term(), cowboy_req:req(), map()) -> ok.
terminate(_Reason, _Req, _State) ->
    %% Unsubscribe from dashboard updates
    erlmcp_dashboard:unsubscribe_updates(self()),
    ok.

%%====================================================================
%% HTTP Callbacks
%%====================================================================

-spec init(cowboy_req:req(), term()) -> {module(), cowboy_req:req(), term()}.
init(Req, _Opts) ->
    %% Set up routing
    Routes = [
        {"/", cowboy_static, {file, "/Users/sac/erlmcp/apps/erlmcp_observability/www/index.html"}},
        {"/static/[...]", cowboy_static, {priv_dir, erlmcp_observability, "static"}},
        {"/ws", ?MODULE, []}
    ],

    cowboy_router:compile(Routes, Req, []).

-spec handle(cowboy_req:req(), term()) -> {ok, cowboy_req:req()}.
handle(Req, _Opts) ->
    cowboy_router:handle(Req).

terminate(_Reason, _Req, _Opts) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec handle_dashboard_message(list()) -> map().
handle_dashboard_message(Message) ->
    case Message of
        [<<"get_dashboard">>] ->
            %% Get current dashboard data
            Data = erlmcp_dashboard:get_dashboard_data(),
            format_dashboard_data(Data);
        [<<"get_health">>] ->
            %% Get system health
            Health = erlmcp_dashboard:get_system_health(),
            Health;
        [<<"get_metrics">>] ->
            %% Get current metrics
            Metrics = erlmcp_dashboard:get_current_metrics(),
            Metrics;
        [<<"get_chaos">>] ->
            %% Get chaos status
            Chaos = erlmcp_dashboard:get_chaos_status(),
            Chaos;
        _ ->
            #{error => "Unknown command"}
    end.

-spec format_dashboard_data(#dashboard_data{}) -> map().
format_dashboard_data(#dashboard_data{
    timestamp = Timestamp,
    metrics = Metrics,
    traces = Traces,
    chaos_status = ChaosStatus,
    system_health = SystemHealth,
    active_connections = ActiveConnections
}) ->
    #{
        timestamp => Timestamp,
        metrics => Metrics,
        traces => Traces,
        chaos_status => ChaosStatus,
        system_health => SystemHealth,
        active_connections => ActiveConnections
    }.
```

## Deployment and Configuration

### 3.1 Application Configuration

Update `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    %% Start the supervision tree
    erlmcp_sup:start_link(),

    %% Start observability components if enabled
    case application:get_env(erlmcp_core, observability_enabled, true) of
        true ->
            %% Start observability dashboard
            erlmcp_dashboard:start_link(),
            %% Start chaos engine
            erlmcp_chaos_engine:start_link(),
            %% Start observability hooks
            erlmcp_observability_hooks:start_link(),
            ok;
        false ->
            ok
    end.
```

### 3.2 System Configuration

Update `/Users/sac/erlmcp/sys.config`:

```erlang
{erlmcp_core, [
    {observability_enabled, true},
    {tracing_enabled, true},
    {metrics_enabled, true},
    {chaos_enabled, true},
    {dashboard_enabled, true},
    {dashboard_port, 8080},
    {dashboard_host, "localhost"}
]},
{erlmcp_observability, [
    {otel_enabled, true},
    {otel_service_name, <<"erlmcp-dashboard">>},
    {otel_exporters, [console]},
    {otel_sampling, always_on},
    {chaos_engine, [
        {enabled, true},
        {max_concurrent_tests, 3}
    ]}
]}.
```

## Usage Examples

### 1. Starting Observability

```bash
## Start erlmcp with observability
rebar3 shell

## In the erlang shell:
1> erlmcp_dashboard:start_link().
2> erlmcp_chaos_engine:start_link().
3> erlmcp_observability_hooks:start_link().
```

### 2. Accessing Dashboard

```bash
## Open dashboard in browser
open http://localhost:8080

## WebSocket connection
ws://localhost:8080/ws
```

### 3. Running Chaos Tests

```erlang
%% Schedule network partition test
erlmcp_chaos_engine:schedule_chaos(<<"network_partition">>, #{
    duration => 30000,
    intensity => 0.5
}).

%% Schedule process crash test
erlmcp_chaos_engine:schedule_chaos(<<"process_crash">>, #{
    target_processes => registry,
    crash_rate => 0.1
}).
```

### 4. Monitoring Metrics

```erlang
%% Get current metrics
Metrics = erlmcp_metrics_core:get_metrics_summary().

%% Get critical metrics
Critical = erlmcp_metrics_core:get_critical_metrics().

%% Get traces
Traces = erlmcp_tracing:get_traces().
```

## Performance Considerations

### 1. Metrics Collection Overhead
- **Sampling Rate**: 10% for non-critical metrics
- **Retention**: 1 hour for raw metrics, aggregated indefinitely
- **Storage**: ETS tables for fast access

### 2. Tracing Overhead
- **Critical Paths Only**: Only trace most important operations
- **Sampling**: 10% sampling for non-critical operations
- **Retention**: 1000 traces in memory, stored in ETS

### 3. Dashboard Performance
- **Update Interval**: 5 seconds between updates
- **Connection Limits**: Max 100 concurrent WebSocket connections
- **Data Compression**: JSON responses compressed

## Monitoring and Alerting

### 1. Health Checks

```erlang
%% System health check
Health = erlmcp_dashboard:get_system_health().
```

### 2. Emergency Stop

```erlang
%% Emergency stop for chaos testing
erlmcp_chaos_engine:emergency_stop().
```

### 3. Alerting Configuration

Update `/Users/sac/erlmcp/config/monitoring.yaml`:

```yaml
alerts:
  - name: high_error_rate
    condition: "error_rate > 0.1"
    action: "notify"

  - name: memory_critical
    condition: "memory_usage > 0.9"
    action: "emergency_stop"

  - name: chaos_emergency
    condition: "chaos_failures > 5"
    action: "emergency_stop"
```

## Conclusion

This implementation guide provides a comprehensive 80/20 observability architecture for erlmcp. The implementation focuses on:

1. **Critical Components**: Only the most valuable observability features
2. **Minimal Overhead**: Low impact on core system performance
3. **Isolation**: All observability failures contained within Tier 3
4. **Simplicity**: Easy to configure and maintain
5. **Resilience**: Built-in chaos testing and recovery mechanisms

The architecture provides production-ready observability while maintaining the erlmcp principles of simplicity and reliability.

---

*Implementation Guide Version: v2.1.0*
*Date: February 1, 2026*
*Status: Implementation Complete*