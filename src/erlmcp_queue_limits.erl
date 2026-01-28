%%%-------------------------------------------------------------------
%% @doc Queue Limits Management for Per-Connection and Per-Tenant Backpressure
%%
%% Enforces hard queue limits with deterministic backpressure to prevent
%% memory exhaustion and latency spirals under sustained overload.
%%
%% Features:
%% - Per-connection message count limits (default: 1000 messages)
%% - Per-connection byte size limits (default: 50MB)
%% - Per-tenant aggregated limits
%% - Deterministic refusal/drop/disconnect strategies
%% - Real-time queue depth metrics
%% - Automatic cleanup of expired entries
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_queue_limits).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    stop/0,
    check_queue_limit/2,
    record_message/3,
    remove_message/2,
    get_connection_stats/1,
    get_tenant_stats/1,
    reset_connection/1,
    get_limits/0,
    update_limits/1,
    get_all_stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type connection_id() :: term().
-type tenant_id() :: term().
-type message_id() :: term().
-type backpressure_action() :: refuse | drop | disconnect.

-record(queue_limits, {
    max_messages :: pos_integer(),
    max_bytes :: pos_integer(),
    cleanup_interval_ms :: pos_integer(),
    backpressure_action :: backpressure_action(),
    enable_tenant_limits :: boolean()
}).

-record(connection_state, {
    connection_id :: connection_id(),
    tenant_id :: tenant_id(),
    message_count :: non_neg_integer(),
    byte_count :: non_neg_integer(),
    messages :: [message_id()],  % For tracking individual message sizes
    message_sizes :: #{message_id() => pos_integer()},
    last_updated :: integer(),
    created_at :: integer()
}).

-record(tenant_state, {
    tenant_id :: tenant_id(),
    total_connections :: non_neg_integer(),
    total_messages :: non_neg_integer(),
    total_bytes :: non_neg_integer(),
    connection_ids :: sets:set(connection_id()),
    last_updated :: integer()
}).

-record(state, {
    limits :: #queue_limits{},
    connections :: ets:table(),
    tenants :: ets:table(),
    cleanup_timer :: reference() | undefined,
    metrics :: #{atom() => any()}
}).

-define(ETS_CONNECTIONS, erlmcp_queue_limits_connections).
-define(ETS_TENANTS, erlmcp_queue_limits_tenants).
-define(CLEANUP_INTERVAL, 30000).
-define(INACTIVITY_TIMEOUT_MS, 5 * 60 * 1000).  % 5 minutes

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

%% @doc Check if message can be enqueued. Returns {ok, accepted} or
%% {error, backpressure_action(), Details}
-spec check_queue_limit(connection_id(), pos_integer()) ->
    {ok, accepted} | {error, backpressure_action(), #{atom() => any()}}.
check_queue_limit(ConnectionId, MessageSize) ->
    gen_server:call(?MODULE, {check_queue_limit, ConnectionId, MessageSize}, 5000).

%% @doc Record a message in the queue for tracking
-spec record_message(connection_id(), message_id(), pos_integer()) -> ok.
record_message(ConnectionId, MessageId, MessageSize) ->
    gen_server:cast(?MODULE, {record_message, ConnectionId, MessageId, MessageSize}).

%% @doc Remove a message from tracking (after dequeue)
-spec remove_message(connection_id(), message_id()) -> ok.
remove_message(ConnectionId, MessageId) ->
    gen_server:cast(?MODULE, {remove_message, ConnectionId, MessageId}).

%% @doc Get statistics for a single connection
-spec get_connection_stats(connection_id()) -> #{atom() => any()} | {error, not_found}.
get_connection_stats(ConnectionId) ->
    gen_server:call(?MODULE, {get_connection_stats, ConnectionId}, 5000).

%% @doc Get aggregated statistics for a tenant
-spec get_tenant_stats(tenant_id()) -> #{atom() => any()} | {error, not_found}.
get_tenant_stats(TenantId) ->
    gen_server:call(?MODULE, {get_tenant_stats, TenantId}, 5000).

%% @doc Reset queue counters for a connection (e.g., when buffer drains)
-spec reset_connection(connection_id()) -> ok.
reset_connection(ConnectionId) ->
    gen_server:call(?MODULE, {reset_connection, ConnectionId}, 5000).

%% @doc Get current queue limit configuration
-spec get_limits() -> #{atom() => any()}.
get_limits() ->
    gen_server:call(?MODULE, {get_limits}, 5000).

%% @doc Update queue limit configuration at runtime
-spec update_limits(#{atom() => any()}) -> {ok, #{atom() => any()}} | {error, term()}.
update_limits(NewLimits) ->
    gen_server:call(?MODULE, {update_limits, NewLimits}, 5000).

%% @doc Get statistics for all connections and tenants
-spec get_all_stats() -> #{atom() => any()}.
get_all_stats() ->
    gen_server:call(?MODULE, {get_all_stats}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    Limits = load_queue_limits_config(),
    Connections = ets:new(?ETS_CONNECTIONS, [set, public, {read_concurrency, true}]),
    Tenants = ets:new(?ETS_TENANTS, [set, public, {read_concurrency, true}]),

    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),

    logger:info("Queue limits manager started", [
        {max_messages, Limits#queue_limits.max_messages},
        {max_bytes_mb, Limits#queue_limits.max_bytes div (1024 * 1024)},
        {backpressure_action, Limits#queue_limits.backpressure_action}
    ]),

    State = #state{
        limits = Limits,
        connections = Connections,
        tenants = Tenants,
        cleanup_timer = CleanupTimer,
        metrics = #{
            total_checks => 0,
            total_acceptances => 0,
            total_refusals => 0,
            total_drops => 0,
            total_disconnects => 0
        }
    },
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({check_queue_limit, ConnectionId, MessageSize}, _From, State) ->
    Result = check_queue_limit_internal(ConnectionId, MessageSize, State),
    {reply, Result, State};

handle_call({get_connection_stats, ConnectionId}, _From, State) ->
    Result = case ets:lookup(State#state.connections, ConnectionId) of
        [] -> {error, not_found};
        [ConnState] -> connection_to_stats(ConnState)
    end,
    {reply, Result, State};

handle_call({get_tenant_stats, TenantId}, _From, State) ->
    Result = case ets:lookup(State#state.tenants, TenantId) of
        [] -> {error, not_found};
        [TenantState] -> tenant_to_stats(TenantState)
    end,
    {reply, Result, State};

handle_call({reset_connection, ConnectionId}, _From, State) ->
    case ets:lookup(State#state.connections, ConnectionId) of
        [ConnState] ->
            TimeNowMs = erlang:system_time(millisecond),
            NewConnState = ConnState#connection_state{
                message_count = 0,
                byte_count = 0,
                messages = [],
                message_sizes = #{},
                last_updated = TimeNowMs
            },
            ets:insert(State#state.connections, NewConnState);
        [] ->
            ok
    end,
    {reply, ok, State};

handle_call({get_limits}, _From, State) ->
    Limits = State#state.limits,
    Reply = #{
        max_messages => Limits#queue_limits.max_messages,
        max_bytes => Limits#queue_limits.max_bytes,
        max_bytes_mb => Limits#queue_limits.max_bytes div (1024 * 1024),
        backpressure_action => Limits#queue_limits.backpressure_action,
        enable_tenant_limits => Limits#queue_limits.enable_tenant_limits,
        cleanup_interval_ms => Limits#queue_limits.cleanup_interval_ms
    },
    {reply, Reply, State};

handle_call({update_limits, NewLimits}, _From, State) ->
    OldLimits = State#state.limits,
    UpdatedLimits = #queue_limits{
        max_messages = maps:get(max_messages, NewLimits, OldLimits#queue_limits.max_messages),
        max_bytes = maps:get(max_bytes, NewLimits, OldLimits#queue_limits.max_bytes),
        cleanup_interval_ms = maps:get(cleanup_interval_ms, NewLimits, OldLimits#queue_limits.cleanup_interval_ms),
        backpressure_action = maps:get(backpressure_action, NewLimits, OldLimits#queue_limits.backpressure_action),
        enable_tenant_limits = maps:get(enable_tenant_limits, NewLimits, OldLimits#queue_limits.enable_tenant_limits)
    },
    Reply = #{
        max_messages => UpdatedLimits#queue_limits.max_messages,
        max_bytes => UpdatedLimits#queue_limits.max_bytes,
        backpressure_action => UpdatedLimits#queue_limits.backpressure_action
    },
    {reply, {ok, Reply}, State#state{limits = UpdatedLimits}};

handle_call({get_all_stats}, _From, State) ->
    ConnectionStats = collect_connection_stats(State#state.connections),
    TenantStats = collect_tenant_stats(State#state.tenants),
    Metrics = State#state.metrics,

    Reply = #{
        metrics => Metrics,
        connection_count => ets:info(State#state.connections, size),
        tenant_count => ets:info(State#state.tenants, size),
        connections => ConnectionStats,
        tenants => TenantStats
    },
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({record_message, ConnectionId, MessageId, MessageSize}, State) ->
    TimeNowMs = erlang:system_time(millisecond),

    case ets:lookup(State#state.connections, ConnectionId) of
        [ConnState] ->
            TenantId = ConnState#connection_state.tenant_id,
            OldMessageCount = ConnState#connection_state.message_count,
            OldByteCount = ConnState#connection_state.byte_count,
            NewMessageCount = OldMessageCount + 1,
            NewByteCount = OldByteCount + MessageSize,

            NewConnState = ConnState#connection_state{
                message_count = NewMessageCount,
                byte_count = NewByteCount,
                messages = [MessageId | ConnState#connection_state.messages],
                message_sizes = maps:put(MessageId, MessageSize, ConnState#connection_state.message_sizes),
                last_updated = TimeNowMs
            },
            ets:insert(State#state.connections, NewConnState),

            % Update tenant stats if enabled
            case State#state.limits#queue_limits.enable_tenant_limits of
                true -> update_tenant_stats(TenantId, ConnectionId, NewMessageCount - OldMessageCount, MessageSize, State#state.tenants, TimeNowMs);
                false -> ok
            end;
        [] ->
            ok
    end,
    {noreply, State};

handle_cast({remove_message, ConnectionId, MessageId}, State) ->
    TimeNowMs = erlang:system_time(millisecond),

    case ets:lookup(State#state.connections, ConnectionId) of
        [ConnState] ->
            TenantId = ConnState#connection_state.tenant_id,
            MessageSizes = ConnState#connection_state.message_sizes,
            MessageSize = maps:get(MessageId, MessageSizes, 0),

            NewMessageCount = max(0, ConnState#connection_state.message_count - 1),
            NewByteCount = max(0, ConnState#connection_state.byte_count - MessageSize),
            NewMessages = lists:delete(MessageId, ConnState#connection_state.messages),
            NewMessageSizes = maps:remove(MessageId, MessageSizes),

            NewConnState = ConnState#connection_state{
                message_count = NewMessageCount,
                byte_count = NewByteCount,
                messages = NewMessages,
                message_sizes = NewMessageSizes,
                last_updated = TimeNowMs
            },
            ets:insert(State#state.connections, NewConnState),

            % Update tenant stats if enabled
            case State#state.limits#queue_limits.enable_tenant_limits of
                true -> update_tenant_stats_remove(TenantId, ConnectionId, 1, MessageSize, State#state.tenants, TimeNowMs);
                false -> ok
            end;
        [] ->
            ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(cleanup_expired, State) ->
    TimeNowMs = erlang:system_time(millisecond),

    % Remove inactive connections
    ets:foldl(fun(ConnState, _Acc) ->
        case (TimeNowMs - ConnState#connection_state.last_updated) > ?INACTIVITY_TIMEOUT_MS of
            true -> ets:delete(State#state.connections, ConnState#connection_state.connection_id);
            false -> ok
        end
    end, ok, State#state.connections),

    % Remove inactive tenants
    ets:foldl(fun(TenantState, _Acc) ->
        case (TimeNowMs - TenantState#tenant_state.last_updated) > ?INACTIVITY_TIMEOUT_MS of
            true -> ets:delete(State#state.tenants, TenantState#tenant_state.tenant_id);
            false -> ok
        end
    end, ok, State#state.tenants),

    NewCleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {noreply, State#state{cleanup_timer = NewCleanupTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ets:delete(State#state.connections),
    ets:delete(State#state.tenants),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec load_queue_limits_config() -> #queue_limits{}.
load_queue_limits_config() ->
    DefaultConfig = #{
        max_messages => 1000,
        max_bytes => 50 * 1024 * 1024,  % 50 MB
        backpressure_action => refuse,
        enable_tenant_limits => false,
        cleanup_interval_ms => 30000
    },

    case application:get_env(erlmcp, queue_limits) of
        {ok, Config} when is_map(Config) ->
            MergedConfig = maps:merge(DefaultConfig, Config),
            config_to_record(MergedConfig);
        {ok, Config} when is_list(Config) ->
            MergedConfig = maps:merge(DefaultConfig, maps:from_list(Config)),
            config_to_record(MergedConfig);
        _ ->
            config_to_record(DefaultConfig)
    end.

-spec config_to_record(#{atom() => any()}) -> #queue_limits{}.
config_to_record(Config) ->
    #queue_limits{
        max_messages = maps:get(max_messages, Config, 1000),
        max_bytes = maps:get(max_bytes, Config, 50 * 1024 * 1024),
        backpressure_action = maps:get(backpressure_action, Config, refuse),
        enable_tenant_limits = maps:get(enable_tenant_limits, Config, false),
        cleanup_interval_ms = maps:get(cleanup_interval_ms, Config, 30000)
    }.

-spec check_queue_limit_internal(connection_id(), pos_integer(), #state{}) ->
    {ok, accepted} | {error, backpressure_action(), #{atom() => any()}}.
check_queue_limit_internal(ConnectionId, MessageSize, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    Limits = State#state.limits,

    ConnState = case ets:lookup(State#state.connections, ConnectionId) of
        [] ->
            % Create new connection entry (assume default tenant if not provided)
            #connection_state{
                connection_id = ConnectionId,
                tenant_id = default,
                message_count = 0,
                byte_count = 0,
                messages = [],
                message_sizes = #{},
                last_updated = TimeNowMs,
                created_at = TimeNowMs
            };
        [Existing] ->
            Existing
    end,

    % Check message count limit
    case ConnState#connection_state.message_count >= Limits#queue_limits.max_messages of
        true ->
            update_metrics(State#state.metrics, Limits#queue_limits.backpressure_action),
            {error, Limits#queue_limits.backpressure_action, #{
                reason => message_count_exceeded,
                current_messages => ConnState#connection_state.message_count,
                max_messages => Limits#queue_limits.max_messages
            }};
        false ->
            % Check byte size limit
            case (ConnState#connection_state.byte_count + MessageSize) > Limits#queue_limits.max_bytes of
                true ->
                    update_metrics(State#state.metrics, Limits#queue_limits.backpressure_action),
                    {error, Limits#queue_limits.backpressure_action, #{
                        reason => byte_count_exceeded,
                        current_bytes => ConnState#connection_state.byte_count,
                        max_bytes => Limits#queue_limits.max_bytes,
                        message_size => MessageSize
                    }};
                false ->
                    % Ensure connection state is inserted
                    ets:insert(State#state.connections, ConnState),
                    {ok, accepted}
            end
    end.

-spec update_metrics(#{atom() => any()}, backpressure_action()) -> ok.
update_metrics(Metrics, Action) ->
    case Action of
        refuse -> maps:put(total_refusals, maps:get(total_refusals, Metrics, 0) + 1, Metrics);
        drop -> maps:put(total_drops, maps:get(total_drops, Metrics, 0) + 1, Metrics);
        disconnect -> maps:put(total_disconnects, maps:get(total_disconnects, Metrics, 0) + 1, Metrics)
    end,
    ok.

-spec connection_to_stats(#connection_state{}) -> #{atom() => any()}.
connection_to_stats(ConnState) ->
    #{
        connection_id => ConnState#connection_state.connection_id,
        tenant_id => ConnState#connection_state.tenant_id,
        message_count => ConnState#connection_state.message_count,
        byte_count => ConnState#connection_state.byte_count,
        byte_count_mb => ConnState#connection_state.byte_count / (1024 * 1024),
        messages_pending => ConnState#connection_state.message_count,
        last_updated => ConnState#connection_state.last_updated,
        uptime_ms => erlang:system_time(millisecond) - ConnState#connection_state.created_at
    }.

-spec tenant_to_stats(#tenant_state{}) -> #{atom() => any()}.
tenant_to_stats(TenantState) ->
    #{
        tenant_id => TenantState#tenant_state.tenant_id,
        total_connections => TenantState#tenant_state.total_connections,
        total_messages => TenantState#tenant_state.total_messages,
        total_bytes => TenantState#tenant_state.total_bytes,
        total_bytes_mb => TenantState#tenant_state.total_bytes / (1024 * 1024),
        last_updated => TenantState#tenant_state.last_updated
    }.

-spec update_tenant_stats(tenant_id(), connection_id(), non_neg_integer(), pos_integer(), ets:table(), integer()) -> ok.
update_tenant_stats(TenantId, ConnectionId, MessageDelta, ByteDelta, TenantTable, TimeNowMs) ->
    case ets:lookup(TenantTable, TenantId) of
        [TenantState] ->
            ConnectionSet = TenantState#tenant_state.connection_ids,
            NewConnectionSet = sets:add_element(ConnectionId, ConnectionSet),
            NewTenantState = TenantState#tenant_state{
                total_connections = sets:size(NewConnectionSet),
                total_messages = TenantState#tenant_state.total_messages + MessageDelta,
                total_bytes = TenantState#tenant_state.total_bytes + ByteDelta,
                connection_ids = NewConnectionSet,
                last_updated = TimeNowMs
            },
            ets:insert(TenantTable, NewTenantState);
        [] ->
            NewTenantState = #tenant_state{
                tenant_id = TenantId,
                total_connections = 1,
                total_messages = MessageDelta,
                total_bytes = ByteDelta,
                connection_ids = sets:from_list([ConnectionId]),
                last_updated = TimeNowMs
            },
            ets:insert(TenantTable, NewTenantState)
    end,
    ok.

-spec update_tenant_stats_remove(tenant_id(), connection_id(), non_neg_integer(), pos_integer(), ets:table(), integer()) -> ok.
update_tenant_stats_remove(TenantId, _ConnectionId, MessageDelta, ByteDelta, TenantTable, TimeNowMs) ->
    case ets:lookup(TenantTable, TenantId) of
        [TenantState] ->
            NewTenantState = TenantState#tenant_state{
                total_messages = max(0, TenantState#tenant_state.total_messages - MessageDelta),
                total_bytes = max(0, TenantState#tenant_state.total_bytes - ByteDelta),
                last_updated = TimeNowMs
            },
            ets:insert(TenantTable, NewTenantState);
        [] ->
            ok
    end,
    ok.

-spec collect_connection_stats(ets:table()) -> #{connection_id() => #{atom() => any()}}.
collect_connection_stats(ConnectionTable) ->
    ets:foldl(fun(ConnState, Acc) ->
        Stats = connection_to_stats(ConnState),
        maps:put(ConnState#connection_state.connection_id, Stats, Acc)
    end, #{}, ConnectionTable).

-spec collect_tenant_stats(ets:table()) -> #{tenant_id() => #{atom() => any()}}.
collect_tenant_stats(TenantTable) ->
    ets:foldl(fun(TenantState, Acc) ->
        Stats = tenant_to_stats(TenantState),
        maps:put(TenantState#tenant_state.tenant_id, Stats, Acc)
    end, #{}, TenantTable).
