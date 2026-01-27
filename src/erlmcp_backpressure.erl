%%%-------------------------------------------------------------------
%% @doc Multi-Level Backpressure Management
%%
%% Sophisticated backpressure system to prevent cascading failures.
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_backpressure).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    stop/0,
    check_rate_limit/2,
    update_latency/2,
    check_handler_queue/2,
    global_circuit_status/0,
    shed_message/2,
    get_stats/0,
    reset_client/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type client_id() :: term().
-type message_priority() :: p0 | p1 | p2 | p3.
-type token_bucket() :: {float(), integer(), float()}.

%% Server state
-record(state, {
    config :: #{atom() => any()},
    clients :: ets:table(),
    handler_queues :: ets:table(),
    metrics :: ets:table(),
    circuit_breaker :: atom(),
    circuit_open_time :: integer() | undefined,
    cleanup_timer :: reference() | undefined
}).

-define(ETS_CLIENTS, backpressure_clients).
-define(ETS_HANDLER_QUEUES, backpressure_handler_queues).
-define(ETS_METRICS, backpressure_metrics).
-define(CLEANUP_INTERVAL, 30000).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec check_rate_limit(client_id(), integer()) -> 
    {ok, {non_neg_integer(), boolean()}} | {error, rate_limited, pos_integer()}.
check_rate_limit(ClientId, TimeNowMs) ->
    gen_server:call(?MODULE, {check_rate_limit, ClientId, TimeNowMs}, 5000).

-spec update_latency(client_id(), non_neg_integer()) -> ok.
update_latency(ClientId, LatencyMs) ->
    gen_server:cast(?MODULE, {update_latency, ClientId, LatencyMs}).

-spec check_handler_queue(atom(), #{atom() => integer()}) -> 
    {ok, queue_ok} | {error, atom(), term()}.
check_handler_queue(HandlerName, QueueStats) ->
    gen_server:call(?MODULE, {check_handler_queue, HandlerName, QueueStats}, 5000).

-spec global_circuit_status() -> {ok, atom()}.
global_circuit_status() ->
    gen_server:call(?MODULE, {global_circuit_status}, 5000).

-spec shed_message(message_priority(), pos_integer()) -> 
    {ok, shed} | {error, not_enough_priority}.
shed_message(Priority, ByteSize) ->
    gen_server:call(?MODULE, {shed_message, Priority, ByteSize}, 5000).

-spec get_stats() -> #{atom() => any()}.
get_stats() ->
    gen_server:call(?MODULE, {get_stats}, 5000).

-spec reset_client(client_id()) -> ok.
reset_client(ClientId) ->
    gen_server:call(?MODULE, {reset_client, ClientId}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    Config = load_config(),
    Clients = ets:new(?ETS_CLIENTS, [set, public, {read_concurrency, true}]),
    HandlerQueues = ets:new(?ETS_HANDLER_QUEUES, [set, public, {read_concurrency, true}]),
    Metrics = ets:new(?ETS_METRICS, [set, public, {read_concurrency, true}]),
    
    init_metrics(Metrics),
    
    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),

    logger:info("Backpressure manager started", []),

    State = #state{
        config = Config,
        clients = Clients,
        handler_queues = HandlerQueues,
        metrics = Metrics,
        circuit_breaker = closed,
        circuit_open_time = undefined,
        cleanup_timer = CleanupTimer
    },
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({check_rate_limit, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_messages_per_sec, State#state.config, 500),
    Result = check_rate_limit_internal(ClientId, MaxRate, TimeNowMs, State),
    {reply, Result, State};

handle_call({check_handler_queue, HandlerName, QueueStats}, _From, State) ->
    Result = check_handler_queue_internal(HandlerName, QueueStats, State),
    {reply, Result, State};

handle_call({global_circuit_status}, _From, State) ->
    {reply, {ok, State#state.circuit_breaker}, State};

handle_call({shed_message, _Priority, _ByteSize}, _From, State) ->
    {reply, {error, not_enough_priority}, State};

handle_call({get_stats}, _From, State) ->
    Stats = get_stats_internal(State),
    {reply, Stats, State};

handle_call({reset_client, ClientId}, _From, State) ->
    ets:delete(State#state.clients, ClientId),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({update_latency, _ClientId, _LatencyMs}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(cleanup_expired, State) ->
    NewState = cleanup_expired_clients(State),
    NewCleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {noreply, NewState#state{cleanup_timer = NewCleanupTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ets:delete(State#state.clients),
    ets:delete(State#state.handler_queues),
    ets:delete(State#state.metrics),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec load_config() -> #{atom() => any()}.
load_config() ->
    DefaultConfig = #{
        max_messages_per_sec => 500,
        burst_multiplier => 2.0,
        adaptive_enabled => true,
        latency_threshold_ms => 100,
        rate_reduction_percent => 10,
        queue_depth_threshold_percent => 80
    },

    case application:get_env(erlmcp, backpressure) of
        {ok, Config} when is_map(Config) ->
            maps:merge(DefaultConfig, Config);
        {ok, Config} when is_list(Config) ->
            maps:merge(DefaultConfig, maps:from_list(Config));
        _ ->
            DefaultConfig
    end.

-spec init_metrics(ets:table()) -> ok.
init_metrics(Metrics) ->
    TimeNowMs = erlang:system_time(millisecond),
    ets:insert(Metrics, {p95_latency_ms, 0}),
    ets:insert(Metrics, {error_rate_percent, 0.0}),
    ets:insert(Metrics, {total_messages, 0}),
    ets:insert(Metrics, {total_errors, 0}),
    ets:insert(Metrics, {messages_shed, 0}),
    ets:insert(Metrics, {last_update, TimeNowMs}),
    ok.

-spec check_rate_limit_internal(client_id(), float(), integer(), #state{}) -> 
    {ok, {non_neg_integer(), boolean()}} | {error, rate_limited, pos_integer()}.
check_rate_limit_internal(ClientId, MaxRate, TimeNowMs, State) ->
    case ets:lookup(State#state.clients, ClientId) of
        [{_, ClientState}] ->
            case ClientState of
                #{message_bucket := Bucket, backpressure_active := true} ->
                    RetryAfterMs = max(50, erlang:system_time(millisecond) rem 500),
                    {error, rate_limited, RetryAfterMs};
                #{message_bucket := Bucket} ->
                    {ok, NewBucket, TokensRemaining} = refill_and_consume(Bucket, MaxRate, TimeNowMs),
                    NewClientState = ClientState#{message_bucket => NewBucket},
                    ets:insert(State#state.clients, {ClientId, NewClientState}),
                    {ok, {TokensRemaining, true}}
            end;
        [] ->
            NewBucket = create_token_bucket(MaxRate),
            {ok, RefilledBucket, TokensRemaining} = refill_and_consume(NewBucket, MaxRate, TimeNowMs),
            NewClientState = #{
                message_bucket => RefilledBucket,
                latency_ms => 0,
                backpressure_active => false,
                violations => 0,
                last_seen => TimeNowMs
            },
            ets:insert(State#state.clients, {ClientId, NewClientState}),
            {ok, {TokensRemaining, true}}
    end.

-spec refill_and_consume(token_bucket(), float(), integer()) ->
    {ok, token_bucket(), non_neg_integer()}.
refill_and_consume(Bucket, Capacity, TimeNowMs) ->
    {Tokens, LastRefillMs, AdaptiveRate} = Bucket,
    ElapsedMs = TimeNowMs - LastRefillMs,
    ActualRate = Capacity * AdaptiveRate,
    TokensToAdd = (ActualRate * ElapsedMs) / 1000.0,
    NewTokens = min(Tokens + TokensToAdd, Capacity * 2.0),

    case NewTokens >= 1.0 of
        true ->
            {ok, {NewTokens - 1.0, TimeNowMs, AdaptiveRate}, round(NewTokens - 1.0)};
        false ->
            {ok, {NewTokens, TimeNowMs, AdaptiveRate}, 0}
    end.

-spec create_token_bucket(float()) -> token_bucket().
create_token_bucket(Capacity) ->
    TimeNowMs = erlang:system_time(millisecond),
    {float(Capacity), TimeNowMs, 1.0}.

-spec check_handler_queue_internal(atom(), #{atom() => integer()}, #state{}) ->
    {ok, queue_ok} | {error, atom(), term()}.
check_handler_queue_internal(HandlerName, QueueStats, State) ->
    Config = State#state.config,
    QueueThresholdPercent = maps:get(queue_depth_threshold_percent, Config, 80),

    case QueueStats of
        #{current_depth := Current, max_capacity := Max} when Max > 0 ->
            UtilizationPercent = (Current / Max) * 100,
            case UtilizationPercent >= QueueThresholdPercent of
                true ->
                    logger:warning("Handler ~p queue depth ~.1f%", 
                                 [HandlerName, UtilizationPercent]),
                    {error, backpressure_signal, QueueStats};
                false ->
                    {ok, queue_ok}
            end;
        _ ->
            {ok, queue_ok}
    end.

-spec get_stats_internal(#state{}) -> #{atom() => any()}.
get_stats_internal(State) ->
    Metrics = State#state.metrics,
    [{p95_latency_ms, P95Latency}] = ets:lookup(Metrics, p95_latency_ms),
    [{error_rate_percent, ErrorRate}] = ets:lookup(Metrics, error_rate_percent),
    [{total_messages, TotalMessages}] = ets:lookup(Metrics, total_messages),
    [{total_errors, TotalErrors}] = ets:lookup(Metrics, total_errors),
    [{messages_shed, MessagesShed}] = ets:lookup(Metrics, messages_shed),

    ClientCount = ets:info(State#state.clients, size),

    #{
        circuit_status => State#state.circuit_breaker,
        p95_latency_ms => P95Latency,
        error_rate_percent => ErrorRate,
        total_messages => TotalMessages,
        total_errors => TotalErrors,
        messages_shed => MessagesShed,
        active_clients => ClientCount
    }.

-spec cleanup_expired_clients(#state{}) -> #state{}.
cleanup_expired_clients(State) ->
    TimeNowMs = erlang:system_time(millisecond),
    MaxInactiveMs = 5 * 60 * 1000,

    ets:foldl(fun({ClientId, ClientState}, _) ->
        case ClientState of
            #{last_seen := LastSeen} when (TimeNowMs - LastSeen) > MaxInactiveMs ->
                ets:delete(State#state.clients, ClientId);
            _ ->
                ok
        end
    end, ok, State#state.clients),

    State.
