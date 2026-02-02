%%%-------------------------------------------------------------------
%%% @doc erlmcp_distributed_tracer - Distributed Tracing for MCP Clustering
%%%
%%% Implements distributed tracing correlation across cluster nodes,
%%% integrating with OTP 28 tracing improvements and erlmcp_otel.
%%%
%%% == OTP 28 Features ==
%%% - Better distributed tracing with trace correlation
%%% - Request ID propagation across nodes
%%% - Trace aggregation from all nodes
%%%
%%% == Architecture ==
%%% - Trace ID injection: Add request ID to cross-node messages
%%% - Trace ID extraction: Get trace ID from incoming messages
%%% - Trace correlation: Link spans across nodes
%%% - Span propagation: Pass span context through cluster
%%% - Trace aggregation: Collect and merge traces from all nodes
%%%
%%% == Integration ==
%%% - erlmcp_otel: Local span creation and management
%%% - erlmcp_cluster: Cross-node RPC calls
%%% - erlmcp_session_affinity: Session routing traces
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distributed_tracer).

-behaviour(gen_server).

%% API
-export([start_link/0,
         inject_trace_id/2,
         extract_trace_id/1,
         correlate_traces/1,
         propagate_span/3,
         get_trace_summary/1,
         get_cluster_trace/1,
         aggregate_trace_spans/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(TRACE_TABLE, erlmcp_distributed_traces).
-define(TRACE_AGGREGATION_TABLE, erlmcp_trace_aggregation).

%%====================================================================
%% Types
%%====================================================================

-type trace_id() :: binary().
-type span_id() :: binary().
-type trace_context() ::
    #{trace_id => trace_id(),
      span_id => span_id(),
      parent_span_id => span_id() | undefined,
      node => node(),
      timestamp => integer()}.
-type trace_summary() ::
    #{trace_id => trace_id(),
      span_count => non_neg_integer(),
      nodes => [node()],
      duration_ns => non_neg_integer(),
      status => ok | error}.
-type cluster_trace() ::
    #{trace_id => trace_id(),
      spans => [map()],
      summary => trace_summary()}.

-record(trace_span,
        {trace_id :: trace_id(),
         span_id :: span_id(),
         parent_span_id :: span_id() | undefined,
         node :: node(),
         module :: atom(),
         function :: atom(),
         args :: list() | undefined,
         result :: term() | undefined,
         status :: ok | error | timeout,
         start_time :: integer(),
         end_time :: integer() | undefined,
         attributes :: map()}).

-record(state,
        {trace_buffer :: #{trace_id => [#trace_span{}]},
        aggregation_buffer :: #{trace_id => [trace_context()]}}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Inject trace ID into a message for cross-node propagation
%% Returns the message with trace context embedded
-spec inject_trace_id(map(), term()) -> {trace_context(), term()}.
inject_trace_id(Message, TraceId) when is_binary(TraceId) ->
    inject_trace_id(Message, #{trace_id => TraceId});
inject_trace_id(Message, TraceCtx) when is_map(TraceCtx) ->
    %% Get current trace context from erlmcp_otel
    CurrentTraceCtx = get_current_trace_context(),

    %% Merge with provided context
    MergedCtx = merge_trace_contexts(CurrentTraceCtx, TraceCtx),

    %% Inject into message
    TraceMsg = {mcp_trace, MergedCtx, Message},
    {MergedCtx, TraceMsg}.

%% @doc Extract trace ID from an incoming message
%% Returns the trace ID and the original message
-spec extract_trace_id(term()) -> {trace_id() | undefined, term()}.
extract_trace_id({mcp_trace, TraceCtx, Message}) when is_map(TraceCtx) ->
    TraceId = maps:get(trace_id, TraceCtx, undefined),
    %% Restore trace context in erlmcp_otel
    restore_trace_context(TraceCtx),
    {TraceId, Message};
extract_trace_id(Message) ->
    %% No trace context in message
    {undefined, Message}.

%% @doc Correlate traces from multiple nodes
%% Aggregates all spans for a trace ID across the cluster
-spec correlate_traces(trace_id()) -> {ok, cluster_trace()} | {error, not_found}.
correlate_traces(TraceId) ->
    gen_server:call(?SERVER, {correlate_traces, TraceId}, 10000).

%% @doc Propagate span context to another node
%% Used when making RPC calls across the cluster
-spec propagate_span(trace_id(), span_id(), node()) -> ok.
propagate_span(TraceId, SpanId, TargetNode) ->
    gen_server:cast(?SERVER, {propagate_span, TraceId, SpanId, TargetNode}),
    ok.

%% @doc Get summary of a trace
-spec get_trace_summary(trace_id()) -> {ok, trace_summary()} | {error, not_found}.
get_trace_summary(TraceId) ->
    gen_server:call(?SERVER, {get_trace_summary, TraceId}, 5000).

%% @doc Get complete trace data from all nodes in cluster
-spec get_cluster_trace(trace_id()) -> {ok, cluster_trace()} | {error, term()}.
get_cluster_trace(TraceId) ->
    gen_server:call(?SERVER, {get_cluster_trace, TraceId}, 30000).

%% @doc Aggregate trace spans from multiple nodes
%% Collects spans from all cluster nodes and merges them
-spec aggregate_trace_spans(trace_id()) -> {ok, [#trace_span{}]} | {error, term()}.
aggregate_trace_spans(TraceId) ->
    %% Get spans from local node
    LocalSpans = get_local_spans(TraceId),

    %% Get spans from remote nodes
    ClusterNodes = nodes(),
    RemoteSpans = get_remote_spans(TraceId, ClusterNodes),

    %% Merge all spans
    AllSpans = lists:merge(LocalSpans, RemoteSpans),
    {ok, AllSpans}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS tables for trace storage
    ets:new(?TRACE_TABLE, [set, public, named_table, {read_concurrency, true}]),
    ets:new(?TRACE_AGGREGATION_TABLE, [bag, public, named_table]),

    State = #state{trace_buffer = #{},
                   aggregation_buffer = #{}},

    ?LOG_INFO("Starting erlmcp_distributed_tracer"),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({correlate_traces, TraceId}, _From, State) ->
    %% Aggregate spans from all nodes
    case aggregate_trace_spans(TraceId) of
        {ok, []} ->
            {reply, {error, not_found}, State, hibernate};
        {ok, Spans} ->
            ClusterTrace = build_cluster_trace(TraceId, Spans),
            {reply, {ok, ClusterTrace}, State, hibernate}
    end;

handle_call({get_trace_summary, TraceId}, _From, State) ->
    case ets:lookup(?TRACE_TABLE, TraceId) of
        [] ->
            {reply, {error, not_found}, State, hibernate};
        [{_, TraceSpans}] ->
            Summary = calculate_trace_summary(TraceId, TraceSpans),
            {reply, {ok, Summary}, State, hibernate}
    end;

handle_call({get_cluster_trace, TraceId}, _From, State) ->
    %% Get spans from all nodes
    case aggregate_trace_spans(TraceId) of
        {ok, []} ->
            {reply, {error, not_found}, State, hibernate};
        {ok, Spans} ->
            ClusterTrace = build_cluster_trace(TraceId, Spans),
            {reply, {ok, ClusterTrace}, State, hibernate}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast({propagate_span, TraceId, SpanId, TargetNode}, State) ->
    %% Propagate span context to target node
    TraceCtx = #{trace_id => TraceId,
                 span_id => SpanId,
                 node => node(),
                 timestamp => erlang:system_time(nanosecond)},

    %% Send to target node via RPC
    case rpc:call(TargetNode, ?MODULE, handle_incoming_span, [TraceCtx], 1000) of
        ok ->
            %% Add to aggregation buffer
            Buffer = State#state.aggregation_buffer,
            Existing = maps:get(TraceId, Buffer, []),
            NewBuffer = Buffer#{TraceId => [TraceCtx | Existing]},
            {noreply, State#state{aggregation_buffer = NewBuffer}, hibernate};
        {error, Reason} ->
            ?LOG_WARNING("Failed to propagate span to ~p: ~p", [TargetNode, Reason]),
            {noreply, State, hibernate}
    end;

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("erlmcp_distributed_tracer terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get current trace context from erlmcp_otel
-spec get_current_trace_context() -> trace_context().
get_current_trace_context() ->
    case erlang:get(erlmcp_otel_current_context) of
        undefined ->
            %% Generate new trace ID
            #{trace_id => generate_trace_id(),
              span_id => generate_span_id(),
              parent_span_id => undefined,
              node => node(),
              timestamp => erlang:system_time(nanosecond)};
        Ctx ->
            %% Extract relevant fields
            #{trace_id => maps:get(trace_id, Ctx),
              span_id => maps:get(span_id, Ctx),
              parent_span_id => maps:get(parent_span_id, Ctx, undefined),
              node => node(),
              timestamp => erlang:system_time(nanosecond)}
    end.

%% @doc Restore trace context in erlmcp_otel
-spec restore_trace_context(trace_context()) -> ok.
restore_trace_context(TraceCtx) ->
    %% Convert to erlmcp_otel format
    OtelCtx = #{
        trace_id => maps:get(trace_id, TraceCtx),
        span_id => maps:get(span_id, TraceCtx),
        parent_span_id => maps:get(parent_span_id, TraceCtx, undefined),
        trace_flags => 1,
        trace_state => <<>>,
        baggage => #{},
        start_time => maps:get(timestamp, TraceCtx),
        attributes => #{},
        events => [],
        status => ok,
        otel_span => undefined
    },
    erlang:put(erlmcp_otel_current_context, OtelCtx),
    ok.

%% @doc Merge two trace contexts
-spec merge_trace_contexts(trace_context() | undefined, map()) -> trace_context().
merge_trace_contexts(undefined, TraceCtx) ->
    TraceCtx;
merge_trace_contexts(CurrentCtx, NewCtx) ->
    %% Merge, with NewCtx taking precedence
    maps:merge(CurrentCtx, NewCtx).

%% @doc Get local spans for a trace ID
-spec get_local_spans(trace_id()) -> [#trace_span{}].
get_local_spans(TraceId) ->
    case ets:lookup(?TRACE_TABLE, TraceId) of
        [] -> [];
        [{_, Spans}] -> Spans
    end.

%% @doc Get remote spans from cluster nodes
-spec get_remote_spans(trace_id(), [node()]) -> [#trace_span{}].
get_remote_spans(TraceId, Nodes) ->
    lists:flatmap(fun(Node) ->
        case rpc:call(Node, ?MODULE, get_local_spans, [TraceId], 2000) of
            {ok, Spans} when is_list(Spans) -> Spans;
            _ -> []
        end
    end, Nodes).

%% @doc Build cluster trace from spans
-spec build_cluster_trace(trace_id(), [#trace_span{}]) -> cluster_trace().
build_cluster_trace(TraceId, Spans) ->
    Summary = calculate_trace_summary(TraceId, Spans),
    #{trace_id => TraceId,
      spans => spans_to_map(Spans),
      summary => Summary}.

%% @doc Calculate trace summary
-spec calculate_trace_summary(trace_id(), [#trace_span{}]) -> trace_summary().
calculate_trace_summary(_TraceId, Spans) ->
    SpanCount = length(Spans),
    Nodes = lists:usort([S#trace_span.node || S <- Spans]),

    %% Calculate duration (from first span start to last span end)
    StartTime = lists:min([S#trace_span.start_time || S <- Spans]),
    EndTime = lists:max([case S#trace_span.end_time of
                             undefined -> S#trace_span.start_time;
                             T -> T
                         end || S <- Spans]),
    Duration = EndTime - StartTime,

    %% Determine overall status
    Status = case lists:any(fun(S) -> S#trace_span.status =:= error end, Spans) of
                 true -> error;
                 false -> ok
             end,

    #{trace_id => _TraceId,
      span_count => SpanCount,
      nodes => Nodes,
      duration_ns => Duration,
      status => Status}.

%% @doc Convert spans to map format
-spec spans_to_map([#trace_span{}]) -> [map()].
spans_to_map(Spans) ->
    [#{trace_id => S#trace_span.trace_id,
       span_id => S#trace_span.span_id,
       parent_span_id => S#trace_span.parent_span_id,
       node => S#trace_span.node,
       module => S#trace_span.module,
       function => S#trace_span.function,
       status => S#trace_span.status,
       start_time => S#trace_span.start_time,
       end_time => S#trace_span.end_time,
       attributes => S#trace_span.attributes} || S <- Spans].

%% @doc Generate trace ID
-spec generate_trace_id() -> trace_id().
generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

%% @doc Generate span ID
-spec generate_span_id() -> span_id().
generate_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    integer_to_binary(Id, 16).

%% @doc Handle incoming span from remote node (called via RPC)
-spec handle_incoming_span(trace_context()) -> ok.
handle_incoming_span(TraceCtx) ->
    TraceId = maps:get(trace_id, TraceCtx),
    %% Store in ETS for aggregation
    case ets:lookup(?TRACE_AGGREGATION_TABLE, TraceId) of
        [] ->
            ets:insert(?TRACE_AGGREGATION_TABLE, {TraceId, [TraceCtx]});
        [{_, Existing}] ->
            ets:insert(?TRACE_AGGREGATION_TABLE, {TraceId, [TraceCtx | Existing]})
    end,
    ok.
