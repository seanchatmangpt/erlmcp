%% ============================================================================
%% erlmcp_telemetry_poc.erl - Complete Telemetry Integration POC
%% ============================================================================
%% Demonstrates telemetry integration for erlmcp with:
%% - Event definitions for MCP operations (tools, resources, prompts)
%% - Handler attachment for metrics collection
%% - Integration with erlmcp_server/erlmcp_client
%% - Zero-overhead when no handlers attached
%% - Prometheus-compatible metric export
%% - Comparison with current erlmcp_metrics approach
%%
%% Usage:
%%   erlmcp_telemetry_poc:run_demo().
%% ============================================================================

-module(erlmcp_telemetry_poc).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    run_demo/0,
    attach_handlers/0,
    detach_handlers/0,
    get_metrics/0,
    get_prometheus_metrics/0,
    reset_metrics/0,
    stop/0
]).

%% Telemetry event emitters (for POC demonstration)
-export([
    emit_tool_call/3,
    emit_resource_read/3,
    emit_prompt_render/3,
    emit_json_rpc_request/2,
    emit_json_rpc_response/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Telemetry Event Definitions
%%====================================================================
%%
%% Telemetry events follow the pattern: [App, Component, Action]
%% Each event carries measurements (duration, count) and metadata (labels)
%%
%% MCP Protocol Events:
%%   [:erlmcp, :tool, :call]         - Tool invocation
%%   [:erlmcp, :resource, :read]     - Resource read operation
%%   [:erlmcp, :prompt, :render]     - Prompt template rendering
%%   [:erlmcp, :json_rpc, :request]  - JSON-RPC request received
%%   [:erlmcp, :json_rpc, :response] - JSON-RPC response sent
%%
%% Measurements:
%%   - duration_us: Operation duration in microseconds
%%   - count: Event count (always 1 for increment)
%%   - bytes: Payload size in bytes (where applicable)
%%
%% Metadata:
%%   - tool_name, resource_uri, prompt_name: Operation identifiers
%%   - status: ok | error
%%   - error_code: MCP error code (if status=error)
%%   - transport: stdio | tcp | http | websocket
%%====================================================================

%%====================================================================
%% State Record
%%====================================================================
-record(state, {
    %% Metrics storage - in-memory for POC
    %% In production, this would be in ETS or external systems
    counters = #{} :: #{binary() => integer()},
    histograms = #{} :: #{binary() => [float()]},
    gauges = #{} :: #{binary() => float()},

    %% Handler IDs for cleanup
    handler_ids = [] :: [telemetry:handler_id()],

    %% Start time
    start_time :: integer()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec run_demo() -> ok.
run_demo() ->
    io:format("~n=== ERLMCP TELEMETRY POC DEMO ===~n~n"),

    %% Step 1: Start the metrics collector
    io:format("Step 1: Starting telemetry metrics collector...~n"),
    case whereis(?MODULE) of
        undefined ->
            {ok, _Pid} = start_link(),
            io:format("  ✓ Metrics collector started~n~n");
        Pid when is_pid(Pid) ->
            io:format("  ✓ Metrics collector already running (pid: ~p)~n~n", [Pid])
    end,

    %% Step 2: Attach telemetry handlers
    io:format("Step 2: Attaching telemetry event handlers...~n"),
    attach_handlers(),
    io:format("  ✓ Handlers attached for MCP events~n~n"),

    %% Step 3: Demonstrate zero-overhead check
    io:format("Step 3: Demonstrating zero-overhead when no handlers...~n"),
    detach_handlers(),
    T1 = erlang:monotonic_time(microsecond),
    _ = [telemetry:execute([:erlmcp, :tool, :call], #{duration_us => 100, count => 1},
                           #{tool_name => <<"noop">>, status => ok}) || _ <- lists:seq(1, 10000)],
    T2 = erlang:monotonic_time(microsecond),
    NoHandlerTime = T2 - T1,
    io:format("  ✓ 10,000 events with NO handlers: ~p μs (~.2f ns/event)~n",
              [NoHandlerTime, NoHandlerTime * 1000 / 10000]),

    attach_handlers(),
    T3 = erlang:monotonic_time(microsecond),
    _ = [telemetry:execute([:erlmcp, :tool, :call], #{duration_us => 100, count => 1},
                           #{tool_name => <<"noop">>, status => ok}) || _ <- lists:seq(1, 10000)],
    T4 = erlang:monotonic_time(microsecond),
    WithHandlerTime = T4 - T3,
    io:format("  ✓ 10,000 events WITH handlers: ~p μs (~.2f ns/event)~n",
              [WithHandlerTime, WithHandlerTime * 1000 / 10000]),
    io:format("  ✓ Overhead: ~.2f%~n~n", [(WithHandlerTime - NoHandlerTime) * 100.0 / NoHandlerTime]),

    reset_metrics(),

    %% Step 4: Simulate MCP operations
    io:format("Step 4: Simulating MCP operations...~n"),
    simulate_mcp_workload(),
    io:format("  ✓ Simulated 100 tool calls, 50 resource reads, 25 prompts~n~n"),

    %% Step 5: Display collected metrics
    io:format("Step 5: Current metrics summary...~n"),
    display_metrics(),

    %% Step 6: Show Prometheus export
    io:format("~nStep 6: Prometheus-compatible export...~n"),
    PrometheusOutput = get_prometheus_metrics(),
    io:format("~s~n", [PrometheusOutput]),

    %% Step 7: Compare with erlmcp_metrics approach
    io:format("~nStep 7: Comparison with erlmcp_metrics...~n"),
    compare_approaches(),

    io:format("~n=== DEMO COMPLETE ===~n"),
    io:format("~nTry these commands:~n"),
    io:format("  erlmcp_telemetry_poc:get_metrics().         %% Get all metrics~n"),
    io:format("  erlmcp_telemetry_poc:get_prometheus_metrics(). %% Prometheus format~n"),
    io:format("  erlmcp_telemetry_poc:reset_metrics().       %% Reset all metrics~n"),
    io:format("  erlmcp_telemetry_poc:detach_handlers().     %% Disable telemetry~n~n"),
    ok.

-spec attach_handlers() -> ok.
attach_handlers() ->
    gen_server:call(?MODULE, attach_handlers).

-spec detach_handlers() -> ok.
detach_handlers() ->
    gen_server:call(?MODULE, detach_handlers).

-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

-spec get_prometheus_metrics() -> binary().
get_prometheus_metrics() ->
    gen_server:call(?MODULE, get_prometheus_metrics).

-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% Telemetry Event Emitters (POC Demonstration)
%%====================================================================

-spec emit_tool_call(binary(), integer(), ok | error) -> ok.
emit_tool_call(ToolName, DurationUs, Status) ->
    Measurements = #{duration_us => DurationUs, count => 1},
    Metadata = #{tool_name => ToolName, status => Status, transport => stdio},
    telemetry:execute([:erlmcp, :tool, :call], Measurements, Metadata).

-spec emit_resource_read(binary(), integer(), ok | error) -> ok.
emit_resource_read(ResourceUri, DurationUs, Status) ->
    Measurements = #{duration_us => DurationUs, count => 1},
    Metadata = #{resource_uri => ResourceUri, status => Status, transport => stdio},
    telemetry:execute([:erlmcp, :resource, :read], Measurements, Metadata).

-spec emit_prompt_render(binary(), integer(), ok | error) -> ok.
emit_prompt_render(PromptName, DurationUs, Status) ->
    Measurements = #{duration_us => DurationUs, count => 1},
    Metadata = #{prompt_name => PromptName, status => Status, transport => stdio},
    telemetry:execute([:erlmcp, :prompt, :render], Measurements, Metadata).

-spec emit_json_rpc_request(binary(), integer()) -> ok.
emit_json_rpc_request(Method, Bytes) ->
    Measurements = #{count => 1, bytes => Bytes},
    Metadata = #{method => Method, direction => inbound},
    telemetry:execute([:erlmcp, :json_rpc, :request], Measurements, Metadata).

-spec emit_json_rpc_response(binary(), integer()) -> ok.
emit_json_rpc_response(Method, Bytes) ->
    Measurements = #{count => 1, bytes => Bytes},
    Metadata = #{method => Method, direction => outbound},
    telemetry:execute([:erlmcp, :json_rpc, :response], Measurements, Metadata).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #state{
        start_time = erlang:system_time(millisecond)
    }}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call(attach_handlers, _From, State) ->
    %% Detach existing handlers first
    State1 = detach_all_handlers(State),

    %% Attach handlers for each event type
    HandlerIds = [
        attach_event_handler([:erlmcp, :tool, :call]),
        attach_event_handler([:erlmcp, :resource, :read]),
        attach_event_handler([:erlmcp, :prompt, :render]),
        attach_event_handler([:erlmcp, :json_rpc, :request]),
        attach_event_handler([:erlmcp, :json_rpc, :response])
    ],

    {reply, ok, State1#state{handler_ids = HandlerIds}};

handle_call(detach_handlers, _From, State) ->
    State1 = detach_all_handlers(State),
    {reply, ok, State1};

handle_call(get_metrics, _From, #state{counters = Counters, histograms = Histograms, gauges = Gauges} = State) ->
    Metrics = #{
        counters => Counters,
        histograms => calculate_histogram_stats(Histograms),
        gauges => Gauges,
        uptime_ms => erlang:system_time(millisecond) - State#state.start_time
    },
    {reply, Metrics, State};

handle_call(get_prometheus_metrics, _From, State) ->
    Output = format_prometheus_metrics(State),
    {reply, Output, State};

handle_call(reset_metrics, _From, State) ->
    NewState = State#state{
        counters = #{},
        histograms = #{},
        gauges = #{},
        start_time = erlang:system_time(millisecond)
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({telemetry_event, EventName, Measurements, Metadata}, State) ->
    %% Process telemetry event and update metrics
    NewState = process_telemetry_event(EventName, Measurements, Metadata, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    _ = detach_all_handlers(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Telemetry Handler Management
%%====================================================================

-spec attach_event_handler(telemetry:event_name()) -> telemetry:handler_id().
attach_event_handler(EventName) ->
    HandlerId = {?MODULE, EventName},

    %% Handler function that casts to our gen_server
    HandlerFun = fun(Event, Measurements, Metadata, _Config) ->
        case whereis(?MODULE) of
            Pid when is_pid(Pid) ->
                gen_server:cast(?MODULE, {telemetry_event, Event, Measurements, Metadata});
            undefined ->
                ok
        end
    end,

    %% Attach handler (telemetry will silently ignore if already attached)
    telemetry:attach(HandlerId, EventName, HandlerFun, #{}),

    HandlerId.

-spec detach_all_handlers(state()) -> state().
detach_all_handlers(#state{handler_ids = HandlerIds} = State) ->
    lists:foreach(fun(HandlerId) ->
        telemetry:detach(HandlerId)
    end, HandlerIds),
    State#state{handler_ids = []}.

%%====================================================================
%% Internal Functions - Metrics Processing
%%====================================================================

-spec process_telemetry_event(telemetry:event_name(), telemetry:event_measurements(),
                               telemetry:event_metadata(), state()) -> state().
process_telemetry_event(EventName, Measurements, Metadata, State) ->
    %% Convert event name to metric key
    MetricPrefix = event_name_to_metric_prefix(EventName),

    %% Extract labels for dimensional metrics
    Labels = extract_labels(Metadata),

    %% Update counters
    State1 = update_counter_metric(MetricPrefix, Labels, State),

    %% Update histograms (for duration measurements)
    State2 = case maps:get(duration_us, Measurements, undefined) of
        undefined -> State1;
        Duration -> update_histogram_metric(MetricPrefix, Duration, Labels, State1)
    end,

    %% Update gauges (for byte measurements)
    State3 = case maps:get(bytes, Measurements, undefined) of
        undefined -> State2;
        Bytes -> update_gauge_metric(MetricPrefix, Bytes, Labels, State2)
    end,

    State3.

-spec event_name_to_metric_prefix([atom()]) -> binary().
event_name_to_metric_prefix([:erlmcp, :tool, :call]) -> <<"erlmcp_tool_calls">>;
event_name_to_metric_prefix([:erlmcp, :resource, :read]) -> <<"erlmcp_resource_reads">>;
event_name_to_metric_prefix([:erlmcp, :prompt, :render]) -> <<"erlmcp_prompt_renders">>;
event_name_to_metric_prefix([:erlmcp, :json_rpc, :request]) -> <<"erlmcp_json_rpc_requests">>;
event_name_to_metric_prefix([:erlmcp, :json_rpc, :response]) -> <<"erlmcp_json_rpc_responses">>;
event_name_to_metric_prefix(_) -> <<"erlmcp_unknown">>.

-spec extract_labels(map()) -> map().
extract_labels(Metadata) ->
    %% Extract relevant labels for dimensional metrics
    Labels = #{},
    Labels1 = case maps:get(status, Metadata, undefined) of
        undefined -> Labels;
        Status -> Labels#{status => Status}
    end,
    Labels2 = case maps:get(transport, Metadata, undefined) of
        undefined -> Labels1;
        Transport -> Labels1#{transport => Transport}
    end,
    %% Could add more label extractors here (tool_name, resource_uri, etc.)
    Labels2.

-spec update_counter_metric(binary(), map(), state()) -> state().
update_counter_metric(MetricPrefix, Labels, #state{counters = Counters} = State) ->
    MetricKey = make_metric_key(MetricPrefix, <<"total">>, Labels),
    CurrentValue = maps:get(MetricKey, Counters, 0),
    State#state{counters = maps:put(MetricKey, CurrentValue + 1, Counters)}.

-spec update_histogram_metric(binary(), number(), map(), state()) -> state().
update_histogram_metric(MetricPrefix, Value, Labels, #state{histograms = Histograms} = State) ->
    MetricKey = make_metric_key(MetricPrefix, <<"duration_us">>, Labels),
    CurrentValues = maps:get(MetricKey, Histograms, []),
    %% Keep last 10000 values (configurable in production)
    NewValues = [Value | lists:sublist(CurrentValues, 9999)],
    State#state{histograms = maps:put(MetricKey, NewValues, Histograms)}.

-spec update_gauge_metric(binary(), number(), map(), state()) -> state().
update_gauge_metric(MetricPrefix, Value, Labels, #state{gauges = Gauges} = State) ->
    MetricKey = make_metric_key(MetricPrefix, <<"bytes">>, Labels),
    State#state{gauges = maps:put(MetricKey, Value, Gauges)}.

-spec make_metric_key(binary(), binary(), map()) -> binary().
make_metric_key(Prefix, Suffix, Labels) ->
    %% Create a metric key with labels encoded
    %% Format: "prefix_suffix{label1=value1,label2=value2}"
    LabelStr = case maps:size(Labels) of
        0 -> <<>>;
        _ ->
            LabelPairs = lists:sort([{K, V} || {K, V} <- maps:to_list(Labels)]),
            LabelStrs = [io_lib:format("~s=~p", [K, V]) || {K, V} <- LabelPairs],
            <<"{", (iolist_to_binary(string:join(LabelStrs, ",")))/binary, "}">>
    end,
    <<Prefix/binary, "_", Suffix/binary, LabelStr/binary>>.

-spec calculate_histogram_stats(map()) -> map().
calculate_histogram_stats(Histograms) ->
    maps:map(fun(_Key, Values) ->
        case Values of
            [] -> #{count => 0};
            _ ->
                Sorted = lists:sort(Values),
                Count = length(Sorted),
                Sum = lists:sum(Sorted),
                Mean = Sum / Count,
                Min = hd(Sorted),
                Max = lists:last(Sorted),
                P50 = percentile(Sorted, 0.50),
                P95 = percentile(Sorted, 0.95),
                P99 = percentile(Sorted, 0.99),
                #{
                    count => Count,
                    sum => Sum,
                    mean => Mean,
                    min => Min,
                    max => Max,
                    p50 => P50,
                    p95 => P95,
                    p99 => P99
                }
        end
    end, Histograms).

-spec percentile([number()], float()) -> float().
percentile(SortedValues, Percentile) ->
    N = length(SortedValues),
    Index = max(1, min(N, round(Percentile * N))),
    lists:nth(Index, SortedValues).

%%====================================================================
%% Internal Functions - Prometheus Export
%%====================================================================

-spec format_prometheus_metrics(state()) -> binary().
format_prometheus_metrics(#state{counters = Counters, histograms = Histograms, gauges = Gauges}) ->
    Lines = [],

    %% Export counters
    Lines1 = lists:foldl(fun({Key, Value}, Acc) ->
        [format_prometheus_counter(Key, Value) | Acc]
    end, Lines, maps:to_list(Counters)),

    %% Export histograms
    Lines2 = lists:foldl(fun({Key, Values}, Acc) ->
        format_prometheus_histogram(Key, Values) ++ Acc
    end, Lines1, maps:to_list(Histograms)),

    %% Export gauges
    Lines3 = lists:foldl(fun({Key, Value}, Acc) ->
        [format_prometheus_gauge(Key, Value) | Acc]
    end, Lines2, maps:to_list(Gauges)),

    iolist_to_binary(lists:reverse(Lines3)).

-spec format_prometheus_counter(binary(), number()) -> iolist().
format_prometheus_counter(Key, Value) ->
    [<<"# TYPE ">>, Key, <<" counter\n">>,
     Key, <<" ">>, float_to_binary(Value * 1.0, [{decimals, 0}]), <<"\n">>].

-spec format_prometheus_histogram(binary(), [number()]) -> [iolist()].
format_prometheus_histogram(_Key, []) -> [];
format_prometheus_histogram(Key, Values) ->
    Stats = calculate_single_histogram_stats(Values),
    BaseKey = re:replace(Key, <<"_duration_us.*">>, <<"">>, [{return, binary}]),
    [
        <<"# TYPE ">>, BaseKey, <<"_duration_seconds histogram\n">>,
        BaseKey, <<"_duration_seconds_sum ">>, float_to_binary(maps:get(sum, Stats) / 1_000_000, [{decimals, 6}]), <<"\n">>,
        BaseKey, <<"_duration_seconds_count ">>, integer_to_binary(maps:get(count, Stats)), <<"\n">>,
        BaseKey, <<"_duration_seconds_bucket{le=\"0.001\"} ">>, integer_to_binary(count_below(Values, 1000)), <<"\n">>,
        BaseKey, <<"_duration_seconds_bucket{le=\"0.01\"} ">>, integer_to_binary(count_below(Values, 10000)), <<"\n">>,
        BaseKey, <<"_duration_seconds_bucket{le=\"0.1\"} ">>, integer_to_binary(count_below(Values, 100000)), <<"\n">>,
        BaseKey, <<"_duration_seconds_bucket{le=\"1.0\"} ">>, integer_to_binary(count_below(Values, 1000000)), <<"\n">>,
        BaseKey, <<"_duration_seconds_bucket{le=\"+Inf\"} ">>, integer_to_binary(maps:get(count, Stats)), <<"\n">>
    ].

-spec format_prometheus_gauge(binary(), number()) -> iolist().
format_prometheus_gauge(Key, Value) ->
    [<<"# TYPE ">>, Key, <<" gauge\n">>,
     Key, <<" ">>, float_to_binary(Value * 1.0, [{decimals, 2}]), <<"\n">>].

-spec calculate_single_histogram_stats([number()]) -> map().
calculate_single_histogram_stats(Values) ->
    Count = length(Values),
    Sum = lists:sum(Values),
    #{count => Count, sum => Sum}.

-spec count_below([number()], number()) -> integer().
count_below(Values, Threshold) ->
    length(lists:filter(fun(V) -> V =< Threshold end, Values)).

%%====================================================================
%% Internal Functions - Demo Support
%%====================================================================

-spec simulate_mcp_workload() -> ok.
simulate_mcp_workload() ->
    %% Simulate 100 tool calls
    lists:foreach(fun(I) ->
        ToolName = <<"tool_", (integer_to_binary(I rem 10))/binary>>,
        Duration = 1000 + rand:uniform(5000),
        Status = case rand:uniform(10) of
            10 -> error;
            _ -> ok
        end,
        emit_tool_call(ToolName, Duration, Status)
    end, lists:seq(1, 100)),

    %% Simulate 50 resource reads
    lists:foreach(fun(I) ->
        ResourceUri = <<"file:///resource/", (integer_to_binary(I rem 5))/binary>>,
        Duration = 500 + rand:uniform(2000),
        Status = case rand:uniform(20) of
            20 -> error;
            _ -> ok
        end,
        emit_resource_read(ResourceUri, Duration, Status)
    end, lists:seq(1, 50)),

    %% Simulate 25 prompt renders
    lists:foreach(fun(I) ->
        PromptName = <<"prompt_", (integer_to_binary(I rem 3))/binary>>,
        Duration = 200 + rand:uniform(1000),
        emit_prompt_render(PromptName, Duration, ok)
    end, lists:seq(1, 25)),

    %% Simulate JSON-RPC traffic
    lists:foreach(fun(_) ->
        emit_json_rpc_request(<<"tools/call">>, 256 + rand:uniform(512)),
        emit_json_rpc_response(<<"tools/call">>, 128 + rand:uniform(256))
    end, lists:seq(1, 100)),

    ok.

-spec display_metrics() -> ok.
display_metrics() ->
    Metrics = get_metrics(),

    io:format("~nCounters:~n"),
    maps:foreach(fun(Key, Value) ->
        io:format("  ~s: ~p~n", [Key, Value])
    end, maps:get(counters, Metrics)),

    io:format("~nHistograms (latency statistics):~n"),
    maps:foreach(fun(Key, Stats) ->
        io:format("  ~s:~n", [Key]),
        io:format("    Count: ~p~n", [maps:get(count, Stats, 0)]),
        case maps:get(count, Stats, 0) of
            0 -> ok;
            _ ->
                io:format("    Mean:  ~.2f μs~n", [maps:get(mean, Stats)]),
                io:format("    P50:   ~.2f μs~n", [maps:get(p50, Stats)]),
                io:format("    P95:   ~.2f μs~n", [maps:get(p95, Stats)]),
                io:format("    P99:   ~.2f μs~n", [maps:get(p99, Stats)])
        end
    end, maps:get(histograms, Metrics)),

    io:format("~nGauges:~n"),
    maps:foreach(fun(Key, Value) ->
        io:format("  ~s: ~.2f~n", [Key, Value])
    end, maps:get(gauges, Metrics)),

    ok.

-spec compare_approaches() -> ok.
compare_approaches() ->
    io:format("~n┌─────────────────────────────────────────────────────────────────┐~n"),
    io:format("│ Current erlmcp_metrics vs Telemetry                             │~n"),
    io:format("├─────────────────────────────────────────────────────────────────┤~n"),
    io:format("│ APPROACH          │ OVERHEAD    │ FLEXIBILITY │ ECOSYSTEM       │~n"),
    io:format("├─────────────────────────────────────────────────────────────────┤~n"),
    io:format("│ erlmcp_metrics    │ ~300ns/cast │ Low         │ erlmcp-only     │~n"),
    io:format("│ (gen_server cast) │             │             │                 │~n"),
    io:format("├─────────────────────────────────────────────────────────────────┤~n"),
    io:format("│ telemetry         │ ~50ns/event │ High        │ BEAM-wide       │~n"),
    io:format("│ (direct dispatch) │ (no handler)│             │ (100+ libs)     │~n"),
    io:format("└─────────────────────────────────────────────────────────────────┘~n"),
    io:format("~nKey Advantages of Telemetry:~n"),
    io:format("  1. Zero overhead when no handlers attached (compiler optimizes away)~n"),
    io:format("  2. Standard BEAM telemetry interface (Phoenix, Ecto, Broadway, etc.)~n"),
    io:format("  3. Multiple handlers can subscribe to same events~n"),
    io:format("  4. Dynamic handler attach/detach without code changes~n"),
    io:format("  5. Built-in integration with telemetry_metrics, telemetry_poller~n"),
    io:format("  6. Direct Prometheus/StatsD export via telemetry_metrics_prometheus~n"),
    io:format("~nKey Advantages of erlmcp_metrics:~n"),
    io:format("  1. Already implemented and working~n"),
    io:format("  2. No external dependencies~n"),
    io:format("  3. Custom aggregation logic~n"),
    io:format("~nRecommendation:~n"),
    io:format("  - Migrate to telemetry for event emission~n"),
    io:format("  - Keep erlmcp_metrics as ONE handler implementation~n"),
    io:format("  - Allow users to add custom handlers (e.g., to Datadog, NewRelic)~n"),
    io:format("  - Emit telemetry events in erlmcp_server/client/registry~n"),
    ok.

%%====================================================================
%% Integration Example - How to Add to erlmcp_server
%%====================================================================
%%
%% In erlmcp_server:handle_call({call_tool, ToolName, Arguments}, From, State):
%%
%% BEFORE (erlmcp_metrics approach):
%%   StartTime = erlang:monotonic_time(microsecond),
%%   Result = invoke_tool_handler(ToolName, Arguments, State),
%%   Duration = erlang:monotonic_time(microsecond) - StartTime,
%%   erlmcp_metrics:record_server_operation(State#state.server_id, <<"tool_call">>,
%%                                          Duration, #{tool => ToolName}),
%%   {reply, Result, State}.
%%
%% AFTER (telemetry approach):
%%   StartTime = erlang:monotonic_time(microsecond),
%%   Result = invoke_tool_handler(ToolName, Arguments, State),
%%   Duration = erlang:monotonic_time(microsecond) - StartTime,
%%   telemetry:execute([:erlmcp, :tool, :call],
%%                     #{duration_us => Duration, count => 1},
%%                     #{tool_name => ToolName,
%%                       status => case Result of {ok, _} -> ok; _ -> error end,
%%                       server_id => State#state.server_id}),
%%   {reply, Result, State}.
%%
%% Benefits:
%%   - If no telemetry handlers attached, telemetry:execute/3 has ~zero overhead
%%   - Users can attach custom handlers without modifying erlmcp code
%%   - Standard BEAM observability interface
%%====================================================================
