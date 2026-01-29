%%%-------------------------------------------------------------------
%% @doc MCP+ Envelope Enforcement - Bounded Behavior
%%
%% Implements envelope enforcement with:
%% - Resource limits (duration, memory, CPU, I/O)
%% - Rate limiting (requests per second/minute)
%% - Scope limits (payload size, recursion depth)
%% - Fail-closed policy under uncertainty
%% - Operation whitelisting/blacklisting
%%
%% All violations result in deterministic refusals with remediation hints.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_envelope).

-include("erlmcp_governance.hrl").
-include("erlmcp_refusal.hrl").

%% API
-export([
    %% Envelope Creation
    create/1,
    create_default/0,

    %% Enforcement
    enforce/2,
    check_limits/2,
    check_operation/2,

    %% Individual Checks
    check_duration/2,
    check_memory/2,
    check_cpu/2,
    check_io/2,
    check_network/2,
    check_rate/2,
    check_concurrent/2,
    check_payload/2,
    check_recursion/2,

    %% Fail-Closed Policy
    check_uncertainty/2,
    should_fail_closed/2,

    %% Metrics
    record_usage/3,
    get_usage/1
]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0]).

%%====================================================================
%% Types
%%====================================================================

-type envelope_opts() :: #{
    max_duration_ms => pos_integer(),
    max_memory_bytes => pos_integer(),
    max_cpu_ms => pos_integer(),
    max_io_bytes => pos_integer(),
    max_network_bytes => pos_integer(),
    max_requests_per_sec => pos_integer(),
    max_requests_per_min => pos_integer(),
    max_concurrent => pos_integer(),
    max_payload_bytes => pos_integer(),
    max_response_bytes => pos_integer(),
    max_recursion_depth => pos_integer(),
    allowed_operations => [binary()],
    denied_operations => [binary()],
    fail_closed => boolean(),
    uncertainty_threshold => float()
}.

-type enforcement_result() :: ok | {refused, #refusal{}}.
-type check_result() :: ok | {error, {pos_integer(), binary(), map()}}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
    %% Usage tracking per envelope
    usage :: #{binary() => usage_record()},
    %% Rate limiting windows
    rate_windows :: #{binary() => rate_window()}
}).

-record(usage_record, {
    envelope_id :: binary(),
    total_duration_us :: pos_integer(),
    total_memory_bytes :: pos_integer(),
    total_cpu_us :: pos_integer(),
    total_io_bytes :: pos_integer(),
    total_network_bytes :: pos_integer(),
    request_count :: pos_integer(),
    last_updated :: timestamp_ms()
}).

-record(rate_window, {
    envelope_id :: binary(),
    second_count :: pos_integer(),
    second_start :: timestamp_ms(),
    minute_count :: pos_integer(),
    minute_start :: timestamp_ms(),
    concurrent :: pos_integer()
}).

-type usage_record() :: #usage_record{}.
-type rate_window() :: #rate_window{}.

%%====================================================================
%% Default Limits
%%====================================================================

-define(DEFAULT_MAX_DURATION_MS, 30000).         % 30 seconds
-define(DEFAULT_MAX_MEMORY_BYTES, 536870912).    % 512 MB
-define(DEFAULT_MAX_CPU_MS, 10000).              % 10 seconds CPU
-define(DEFAULT_MAX_IO_BYTES, 104857600).        % 100 MB
-define(DEFAULT_MAX_NETWORK_BYTES, 52428800).    % 50 MB
-define(DEFAULT_MAX_REQUESTS_SEC, 100).
-define(DEFAULT_MAX_REQUESTS_MIN, 5000).
-define(DEFAULT_MAX_CONCURRENT, 50).
-define(DEFAULT_MAX_PAYLOAD_BYTES, 16777216).    % 16 MB
-define(DEFAULT_MAX_RESPONSE_BYTES, 16777216).   % 16 MB
-define(DEFAULT_MAX_RECURSION, 10).
-define(DEFAULT_UNCERTAINTY_THRESHOLD, 0.8).

%%====================================================================
%% API - Envelope Creation
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create an envelope with specified limits.
-spec create(envelope_opts()) -> #mcp_envelope{}.
create(Opts) when is_map(Opts) ->
    Id = generate_envelope_id(),
    #mcp_envelope{
        id = Id,
        contract_id = maps:get(contract_id, Opts, <<>>),
        max_duration_ms = maps:get(max_duration_ms, Opts, ?DEFAULT_MAX_DURATION_MS),
        max_memory_bytes = maps:get(max_memory_bytes, Opts, ?DEFAULT_MAX_MEMORY_BYTES),
        max_cpu_ms = maps:get(max_cpu_ms, Opts, ?DEFAULT_MAX_CPU_MS),
        max_io_bytes = maps:get(max_io_bytes, Opts, ?DEFAULT_MAX_IO_BYTES),
        max_network_bytes = maps:get(max_network_bytes, Opts, ?DEFAULT_MAX_NETWORK_BYTES),
        max_requests_per_sec = maps:get(max_requests_per_sec, Opts, ?DEFAULT_MAX_REQUESTS_SEC),
        max_requests_per_min = maps:get(max_requests_per_min, Opts, ?DEFAULT_MAX_REQUESTS_MIN),
        max_concurrent = maps:get(max_concurrent, Opts, ?DEFAULT_MAX_CONCURRENT),
        max_payload_bytes = maps:get(max_payload_bytes, Opts, ?DEFAULT_MAX_PAYLOAD_BYTES),
        max_response_bytes = maps:get(max_response_bytes, Opts, ?DEFAULT_MAX_RESPONSE_BYTES),
        max_recursion_depth = maps:get(max_recursion_depth, Opts, ?DEFAULT_MAX_RECURSION),
        allowed_operations = maps:get(allowed_operations, Opts, []),
        denied_operations = maps:get(denied_operations, Opts, []),
        fail_closed = maps:get(fail_closed, Opts, true),
        uncertainty_threshold = maps:get(uncertainty_threshold, Opts, ?DEFAULT_UNCERTAINTY_THRESHOLD),
        metrics_enabled = maps:get(metrics_enabled, Opts, true),
        trace_enabled = maps:get(trace_enabled, Opts, false)
    }.

%% @doc Create an envelope with default limits.
-spec create_default() -> #mcp_envelope{}.
create_default() ->
    create(#{}).

%%====================================================================
%% API - Enforcement
%%====================================================================

%% @doc Enforce all envelope constraints on a request.
-spec enforce(#mcp_envelope{}, map()) -> enforcement_result().
enforce(#mcp_envelope{} = Envelope, Context) when is_map(Context) ->
    Checks = [
        fun() -> check_operation(Envelope, Context) end,
        fun() -> check_rate(Envelope, Context) end,
        fun() -> check_concurrent(Envelope, Context) end,
        fun() -> check_payload(Envelope, Context) end,
        fun() -> check_recursion(Envelope, Context) end,
        fun() -> check_uncertainty(Envelope, Context) end
    ],
    run_enforcement_checks(Checks).

%% @doc Check resource limits during execution.
-spec check_limits(#mcp_envelope{}, map()) -> enforcement_result().
check_limits(#mcp_envelope{} = Envelope, Metrics) when is_map(Metrics) ->
    Checks = [
        fun() -> check_duration(Envelope, Metrics) end,
        fun() -> check_memory(Envelope, Metrics) end,
        fun() -> check_cpu(Envelope, Metrics) end,
        fun() -> check_io(Envelope, Metrics) end,
        fun() -> check_network(Envelope, Metrics) end
    ],
    run_enforcement_checks(Checks).

%% @doc Check if operation is allowed.
-spec check_operation(#mcp_envelope{}, map()) -> check_result().
check_operation(#mcp_envelope{} = Envelope, Context) ->
    Operation = maps:get(operation, Context, <<>>),

    %% Check denied list first (blacklist)
    case lists:member(Operation, Envelope#mcp_envelope.denied_operations) of
        true ->
            {error, {?REFUSAL_ENVELOPE_OPERATION_DENIED,
                     <<"Operation explicitly denied">>,
                     #{operation => Operation}}};
        false ->
            %% Check allowed list if specified (whitelist)
            case Envelope#mcp_envelope.allowed_operations of
                [] -> ok;  % No whitelist = all allowed
                AllowedOps ->
                    case lists:member(Operation, AllowedOps) of
                        true -> ok;
                        false ->
                            {error, {?REFUSAL_ENVELOPE_OPERATION_DENIED,
                                     <<"Operation not in allowed list">>,
                                     #{operation => Operation}}}
                    end
            end
    end.

%%====================================================================
%% API - Individual Checks
%%====================================================================

%% @doc Check duration limit.
-spec check_duration(#mcp_envelope{}, map()) -> check_result().
check_duration(#mcp_envelope{max_duration_ms = MaxMs}, Metrics) ->
    DurationMs = maps:get(duration_ms, Metrics, 0),
    case DurationMs > MaxMs of
        true ->
            {error, {?REFUSAL_ENVELOPE_DURATION_EXCEEDED,
                     <<"Execution duration exceeded">>,
                     #{limit_ms => MaxMs, actual_ms => DurationMs,
                       hint => <<"Reduce operation complexity or increase timeout">>}}};
        false -> ok
    end.

%% @doc Check memory limit.
-spec check_memory(#mcp_envelope{}, map()) -> check_result().
check_memory(#mcp_envelope{max_memory_bytes = MaxBytes}, Metrics) ->
    MemoryBytes = maps:get(memory_bytes, Metrics, 0),
    case MemoryBytes > MaxBytes of
        true ->
            {error, {?REFUSAL_ENVELOPE_MEMORY_EXCEEDED,
                     <<"Memory limit exceeded">>,
                     #{limit_bytes => MaxBytes, actual_bytes => MemoryBytes,
                       hint => <<"Reduce data size or use streaming">>}}};
        false -> ok
    end.

%% @doc Check CPU limit.
-spec check_cpu(#mcp_envelope{}, map()) -> check_result().
check_cpu(#mcp_envelope{max_cpu_ms = MaxMs}, Metrics) ->
    CpuMs = maps:get(cpu_ms, Metrics, 0),
    case CpuMs > MaxMs of
        true ->
            {error, {?REFUSAL_ENVELOPE_CPU_EXCEEDED,
                     <<"CPU time limit exceeded">>,
                     #{limit_ms => MaxMs, actual_ms => CpuMs,
                       hint => <<"Optimize algorithm or reduce computation">>}}};
        false -> ok
    end.

%% @doc Check I/O limit.
-spec check_io(#mcp_envelope{}, map()) -> check_result().
check_io(#mcp_envelope{max_io_bytes = MaxBytes}, Metrics) ->
    IoBytes = maps:get(io_bytes, Metrics, 0),
    case IoBytes > MaxBytes of
        true ->
            {error, {?REFUSAL_ENVELOPE_IO_EXCEEDED,
                     <<"I/O limit exceeded">>,
                     #{limit_bytes => MaxBytes, actual_bytes => IoBytes,
                       hint => <<"Reduce file operations or use pagination">>}}};
        false -> ok
    end.

%% @doc Check network limit.
-spec check_network(#mcp_envelope{}, map()) -> check_result().
check_network(#mcp_envelope{max_network_bytes = MaxBytes}, Metrics) ->
    NetworkBytes = maps:get(network_bytes, Metrics, 0),
    case NetworkBytes > MaxBytes of
        true ->
            {error, {?REFUSAL_ENVELOPE_NETWORK_EXCEEDED,
                     <<"Network transfer limit exceeded">>,
                     #{limit_bytes => MaxBytes, actual_bytes => NetworkBytes,
                       hint => <<"Reduce payload size or use compression">>}}};
        false -> ok
    end.

%% @doc Check rate limits.
-spec check_rate(#mcp_envelope{}, map()) -> check_result().
check_rate(#mcp_envelope{} = Envelope, _Context) ->
    gen_server:call(?MODULE, {check_rate, Envelope}).

%% @doc Check concurrent request limit.
-spec check_concurrent(#mcp_envelope{}, map()) -> check_result().
check_concurrent(#mcp_envelope{} = Envelope, _Context) ->
    gen_server:call(?MODULE, {check_concurrent, Envelope}).

%% @doc Check payload size limit.
-spec check_payload(#mcp_envelope{}, map()) -> check_result().
check_payload(#mcp_envelope{max_payload_bytes = MaxBytes}, Context) ->
    PayloadBytes = maps:get(payload_bytes, Context, 0),
    case PayloadBytes > MaxBytes of
        true ->
            {error, {?REFUSAL_ENVELOPE_PAYLOAD_EXCEEDED,
                     <<"Payload size limit exceeded">>,
                     #{limit_bytes => MaxBytes, actual_bytes => PayloadBytes,
                       hint => <<"Reduce payload size or split into chunks">>}}};
        false -> ok
    end.

%% @doc Check recursion depth limit.
-spec check_recursion(#mcp_envelope{}, map()) -> check_result().
check_recursion(#mcp_envelope{max_recursion_depth = MaxDepth}, Context) ->
    Depth = maps:get(recursion_depth, Context, 0),
    case Depth > MaxDepth of
        true ->
            {error, {?REFUSAL_ENVELOPE_RECURSION_EXCEEDED,
                     <<"Recursion depth limit exceeded">>,
                     #{limit => MaxDepth, actual => Depth,
                       hint => <<"Reduce nesting or use iteration">>}}};
        false -> ok
    end.

%%====================================================================
%% API - Fail-Closed Policy
%%====================================================================

%% @doc Check uncertainty threshold.
-spec check_uncertainty(#mcp_envelope{}, map()) -> check_result().
check_uncertainty(#mcp_envelope{fail_closed = false}, _Context) ->
    ok;
check_uncertainty(#mcp_envelope{} = Envelope, Context) ->
    Confidence = maps:get(confidence, Context, 1.0),
    case should_fail_closed(Envelope, Confidence) of
        true ->
            {error, {?REFUSAL_ENVELOPE_UNCERTAINTY,
                     <<"Confidence below threshold - fail closed">>,
                     #{threshold => Envelope#mcp_envelope.uncertainty_threshold,
                       actual => Confidence,
                       hint => <<"Increase confidence or adjust threshold">>}}};
        false -> ok
    end.

%% @doc Determine if should fail closed based on confidence.
-spec should_fail_closed(#mcp_envelope{}, float()) -> boolean().
should_fail_closed(#mcp_envelope{fail_closed = false}, _Confidence) ->
    false;
should_fail_closed(#mcp_envelope{uncertainty_threshold = Threshold}, Confidence) ->
    Confidence < Threshold.

%%====================================================================
%% API - Metrics
%%====================================================================

%% @doc Record resource usage.
-spec record_usage(#mcp_envelope{}, atom(), pos_integer()) -> ok.
record_usage(#mcp_envelope{id = EnvelopeId}, MetricType, Value) ->
    gen_server:cast(?MODULE, {record_usage, EnvelopeId, MetricType, Value}).

%% @doc Get usage statistics for an envelope.
-spec get_usage(binary()) -> {ok, usage_record()} | {error, not_found}.
get_usage(EnvelopeId) ->
    gen_server:call(?MODULE, {get_usage, EnvelopeId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = #state{
        usage = #{},
        rate_windows = #{}
    },
    {ok, State}.

handle_call({check_rate, #mcp_envelope{} = Envelope}, _From, State) ->
    EnvelopeId = Envelope#mcp_envelope.id,
    Now = erlang:system_time(millisecond),

    Window = maps:get(EnvelopeId, State#state.rate_windows, new_rate_window(EnvelopeId, Now)),
    {Result, NewWindow} = do_check_rate(Envelope, Window, Now),

    NewRateWindows = maps:put(EnvelopeId, NewWindow, State#state.rate_windows),
    {reply, Result, State#state{rate_windows = NewRateWindows}};

handle_call({check_concurrent, #mcp_envelope{} = Envelope}, _From, State) ->
    EnvelopeId = Envelope#mcp_envelope.id,
    MaxConcurrent = Envelope#mcp_envelope.max_concurrent,

    Window = maps:get(EnvelopeId, State#state.rate_windows, new_rate_window(EnvelopeId, 0)),
    Concurrent = Window#rate_window.concurrent,

    case Concurrent >= MaxConcurrent of
        true ->
            {reply, {error, {?REFUSAL_ENVELOPE_CONCURRENT_EXCEEDED,
                             <<"Concurrent request limit exceeded">>,
                             #{limit => MaxConcurrent, current => Concurrent}}}, State};
        false ->
            NewWindow = Window#rate_window{concurrent = Concurrent + 1},
            NewRateWindows = maps:put(EnvelopeId, NewWindow, State#state.rate_windows),
            {reply, ok, State#state{rate_windows = NewRateWindows}}
    end;

handle_call({get_usage, EnvelopeId}, _From, State) ->
    case maps:get(EnvelopeId, State#state.usage, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Usage -> {reply, {ok, Usage}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_usage, EnvelopeId, MetricType, Value}, State) ->
    Now = erlang:system_time(millisecond),
    Usage = maps:get(EnvelopeId, State#state.usage, new_usage_record(EnvelopeId)),
    NewUsage = update_usage(Usage, MetricType, Value, Now),
    NewUsageMap = maps:put(EnvelopeId, NewUsage, State#state.usage),
    {noreply, State#state{usage = NewUsageMap}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_envelope_id() -> binary().
generate_envelope_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    base64:encode(Bytes).

-spec run_enforcement_checks([fun(() -> check_result())]) -> enforcement_result().
run_enforcement_checks([]) ->
    ok;
run_enforcement_checks([CheckFun | Rest]) ->
    case CheckFun() of
        ok -> run_enforcement_checks(Rest);
        {error, {Code, Message, Details}} ->
            Refusal = #refusal{
                code = Code,
                http_status = 429,  % Too Many Requests or similar
                message = Message,
                hint = maps:get(hint, Details, <<>>),
                severity = error,
                details = Details,
                timestamp = erlang:system_time(millisecond)
            },
            {refused, Refusal}
    end.

-spec new_rate_window(binary(), timestamp_ms()) -> rate_window().
new_rate_window(EnvelopeId, Now) ->
    #rate_window{
        envelope_id = EnvelopeId,
        second_count = 0,
        second_start = Now,
        minute_count = 0,
        minute_start = Now,
        concurrent = 0
    }.

-spec do_check_rate(#mcp_envelope{}, rate_window(), timestamp_ms()) ->
    {check_result(), rate_window()}.
do_check_rate(Envelope, Window, Now) ->
    MaxPerSec = Envelope#mcp_envelope.max_requests_per_sec,
    MaxPerMin = Envelope#mcp_envelope.max_requests_per_min,

    %% Reset second window if needed
    {SecCount, SecStart} = case Now - Window#rate_window.second_start > 1000 of
        true -> {0, Now};
        false -> {Window#rate_window.second_count, Window#rate_window.second_start}
    end,

    %% Reset minute window if needed
    {MinCount, MinStart} = case Now - Window#rate_window.minute_start > 60000 of
        true -> {0, Now};
        false -> {Window#rate_window.minute_count, Window#rate_window.minute_start}
    end,

    %% Check limits
    case SecCount >= MaxPerSec of
        true ->
            {{error, {?REFUSAL_ENVELOPE_RATE_EXCEEDED,
                      <<"Per-second rate limit exceeded">>,
                      #{limit => MaxPerSec, current => SecCount}}}, Window};
        false ->
            case MinCount >= MaxPerMin of
                true ->
                    {{error, {?REFUSAL_ENVELOPE_RATE_EXCEEDED,
                              <<"Per-minute rate limit exceeded">>,
                              #{limit => MaxPerMin, current => MinCount}}}, Window};
                false ->
                    NewWindow = Window#rate_window{
                        second_count = SecCount + 1,
                        second_start = SecStart,
                        minute_count = MinCount + 1,
                        minute_start = MinStart
                    },
                    {ok, NewWindow}
            end
    end.

-spec new_usage_record(binary()) -> usage_record().
new_usage_record(EnvelopeId) ->
    #usage_record{
        envelope_id = EnvelopeId,
        total_duration_us = 0,
        total_memory_bytes = 0,
        total_cpu_us = 0,
        total_io_bytes = 0,
        total_network_bytes = 0,
        request_count = 0,
        last_updated = erlang:system_time(millisecond)
    }.

-spec update_usage(usage_record(), atom(), pos_integer(), timestamp_ms()) -> usage_record().
update_usage(Usage, duration, Value, Now) ->
    Usage#usage_record{
        total_duration_us = Usage#usage_record.total_duration_us + Value,
        request_count = Usage#usage_record.request_count + 1,
        last_updated = Now
    };
update_usage(Usage, memory, Value, Now) ->
    Usage#usage_record{total_memory_bytes = Usage#usage_record.total_memory_bytes + Value, last_updated = Now};
update_usage(Usage, cpu, Value, Now) ->
    Usage#usage_record{total_cpu_us = Usage#usage_record.total_cpu_us + Value, last_updated = Now};
update_usage(Usage, io, Value, Now) ->
    Usage#usage_record{total_io_bytes = Usage#usage_record.total_io_bytes + Value, last_updated = Now};
update_usage(Usage, network, Value, Now) ->
    Usage#usage_record{total_network_bytes = Usage#usage_record.total_network_bytes + Value, last_updated = Now};
update_usage(Usage, _Unknown, _Value, _Now) ->
    Usage.
