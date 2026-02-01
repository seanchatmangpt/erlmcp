%%%-------------------------------------------------------------------
%%% @doc Connection Limiter - Prevents FD Exhaustion
%%%
%%% This module implements bounded connection limiting to prevent
%%% file descriptor exhaustion at high connection counts. It provides:
%%%
%%% - Global connection limits (default: 10,000)
%%% - Per-server connection tracking
%%% - Graceful rejection before resource exhaustion
%%% - Monitoring and alerting at 70% capacity
%%% - gproc-based distributed counters
%%%
%%% Configuration via sys.config:
%%% ```erlang
%%% {erlmcp, [
%%%     {connection_limiting, #{
%%%         max_connections => 10000,
%%%         alert_threshold => 0.7,
%%%         enabled => true
%%%     }}
%%% ]}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_limiter).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, accept_connection/1, release_connection/1, get_connection_count/0,
         get_connection_count/1, set_limit/1, get_limit/0, get_stats/0, is_limit_enabled/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(DEFAULT_MAX_CONNECTIONS, 10000).
-define(DEFAULT_ALERT_THRESHOLD, 0.7).
-define(GPROC_KEY, {c, l, erlmcp_connection_count}).
-define(ALERT_COOLDOWN, 60000).  % 1 minute between alerts

%% Types
-type server_id() :: atom() | binary().
-type limit_result() :: accept | {error, too_many_connections}.
-type stats() ::
    #{current_connections => non_neg_integer(),
      max_connections => non_neg_integer(),
      alert_threshold => float(),
      last_alert => integer() | undefined}.

%% Server state
-record(state,
        {max_connections :: pos_integer(),
         alert_threshold :: float(),
         last_alert :: integer() | undefined,
         server_counts :: #{server_id() => non_neg_integer()}}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the connection limiter
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the connection limiter
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Ensure counter is registered (called during init)
%% @private
-spec ensure_counter_registered() -> ok.
ensure_counter_registered() ->
    try
        gproc:reg_shared(?GPROC_KEY),
        ok
    catch
        error:badarg ->
            %% Counter already exists
            ok
    end.

%% @doc Check if connection should be accepted
%% Returns 'accept' or '{error, too_many_connections}'
-spec accept_connection(server_id()) -> limit_result().
accept_connection(ServerId) ->
    case is_limit_enabled() of
        false ->
            %% Track but don't enforce - still use call for synchronization
            gen_server:call(?MODULE, {accept_connection, ServerId});
        true ->
            %% Check and potentially accept
            case gen_server:call(?MODULE, {check_accept, ServerId}) of
                {ok, Count} ->
                    accept;
                {error, too_many_connections, Count} ->
                    logger:warning("Connection limit exceeded: ~p for server ~p",
                                   [Count, ServerId]),
                    {error, too_many_connections}
            end
    end.

%% @doc Release a connection slot
-spec release_connection(server_id()) -> ok.
release_connection(ServerId) ->
    %% Use cast for release since we don't need to wait for confirmation
    gen_server:cast(?MODULE, {release_connection, ServerId}),
    ok.

%% @doc Get current global connection count
-spec get_connection_count() -> non_neg_integer().
get_connection_count() ->
    %% Always return actual count, even when limiting disabled
    gen_server:call(?MODULE, get_connection_count).

%% @doc Get connection count for a specific server
-spec get_connection_count(server_id()) -> non_neg_integer().
get_connection_count(ServerId) ->
    %% Always return actual count, even when limiting disabled
    try
        gen_server:call(?MODULE, {get_connection_count, ServerId})
    catch
        _:_ ->
            0
    end.

%% @doc Set the maximum connection limit
-spec set_limit(pos_integer()) -> ok.
set_limit(Limit) when is_integer(Limit), Limit > 0 ->
    gen_server:call(?MODULE, {set_limit, Limit}).

%% @doc Get the current maximum connection limit
-spec get_limit() -> pos_integer() | infinity.
get_limit() ->
    case is_limit_enabled() of
        false ->
            infinity;
        true ->
            gen_server:call(?MODULE, get_limit)
    end.

%% @doc Get connection limiter statistics
-spec get_stats() -> stats().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Check if connection limiting is enabled
-spec is_limit_enabled() -> boolean().
is_limit_enabled() ->
    case application:get_env(erlmcp_core, connection_limiting) of
        {ok, Config} when is_map(Config) ->
            maps:get(enabled, Config, true);
        {ok, Config} when is_list(Config) ->
            proplists:get_value(enabled, Config, true);
        _ ->
            true
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Connection limiter init starting"),
    %% Load configuration
    MaxConnections = load_config(max_connections, ?DEFAULT_MAX_CONNECTIONS),
    AlertThreshold = load_config(alert_threshold, ?DEFAULT_ALERT_THRESHOLD),

    logger:info("Loaded config: max=~p, alert_threshold=~p", [MaxConnections, AlertThreshold]),

    %% Register gproc counter for global count
    ensure_counter_registered(),

    {ok,
     #state{max_connections = MaxConnections,
            alert_threshold = AlertThreshold,
            last_alert = undefined,
            server_counts = #{}}}.

handle_call(get_connection_count, _From, State) ->
    Count = get_counter_value(),
    {reply, Count, State};
handle_call({get_connection_count, ServerId}, _From, State) ->
    Count = maps:get(ServerId, State#state.server_counts, 0),
    {reply, Count, State};
handle_call({check_accept, ServerId}, _From, State) ->
    %% Increment counters
    increment_counter(),
    NewServerCounts = maps:update_with(ServerId, fun(V) -> V + 1 end, 1, State#state.server_counts),

    CurrentCount = get_counter_value(),
    MaxConnections = State#state.max_connections,

    case CurrentCount =< MaxConnections of
        true ->
            %% Check for alert threshold
            check_alert_threshold(CurrentCount, MaxConnections),
            {reply, {ok, CurrentCount}, State#state{server_counts = NewServerCounts}};
        false ->
            %% Rollback the increment since we're rejecting
            decrement_counter(),
            {reply, {error, too_many_connections, CurrentCount}, State}
    end;
handle_call({accept_connection, ServerId}, _From, State) ->
    %% Increment counters without checking limit (when limiting disabled)
    increment_counter(),
    NewServerCounts = maps:update_with(ServerId, fun(V) -> V + 1 end, 1, State#state.server_counts),
    {reply, accept, State#state{server_counts = NewServerCounts}};
handle_call({set_limit, Limit}, _From, State) when is_integer(Limit), Limit > 0 ->
    logger:info("Connection limit updated: ~p -> ~p", [State#state.max_connections, Limit]),
    {reply, ok, State#state{max_connections = Limit}};
handle_call(get_limit, _From, State) ->
    {reply, State#state.max_connections, State};
handle_call(get_stats, _From, State) ->
    Count = get_counter_value(),
    Stats =
        #{current_connections => Count,
          max_connections => State#state.max_connections,
          alert_threshold => State#state.alert_threshold,
          last_alert => State#state.last_alert},
    {reply, Stats, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({release_connection, ServerId}, State) ->
    %% Decrement counters
    decrement_counter(),
    NewServerCounts = maps:update_with(ServerId, fun(V) -> V - 1 end, 0, State#state.server_counts),
    {noreply, State#state{server_counts = NewServerCounts}};
handle_cast({check_alert, CurrentCount, MaxConnections}, State) ->
    NewState = handle_alert(CurrentCount, MaxConnections, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({check_alert, CurrentCount, MaxConnections}, State) ->
    NewState = handle_alert(CurrentCount, MaxConnections, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    try
        gproc:unreg(?GPROC_KEY)
    catch
        error:badarg ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Load configuration value
-spec load_config(atom(), term()) -> term().
load_config(Key, Default) ->
    case application:get_env(erlmcp_core, connection_limiting) of
        {ok, Config} when is_map(Config) ->
            maps:get(Key, Config, Default);
        {ok, Config} when is_list(Config) ->
            proplists:get_value(Key, Config, Default);
        _ ->
            Default
    end.

%% @private Get current counter value with fallback
-spec get_counter_value() -> non_neg_integer().
get_counter_value() ->
    try
        %% Use get_value for counters (type c), not lookup_value
        %% lookup_value only works for types: n, a, rc
        case gproc:get_value(?GPROC_KEY) of
            Value when is_integer(Value) ->
                logger:debug("Counter value retrieved: ~p", [Value]),
                Value;
            _ ->
                logger:warning("Counter exists but has non-integer value"),
                0
        end
    catch
        error:badarg ->
            %% Counter doesn't exist, try to create it
            logger:warning("Counter not found, attempting to register"),
            try
                gproc:reg(?GPROC_KEY, 0),
                0
            catch
                error:badarg ->
                    %% Still can't register (race condition or already exists)
                    logger:error("Failed to register counter after badarg"),
                    0
            end
    end.

%% @private Increment global counter
-spec increment_counter() -> ok.
increment_counter() ->
    try
        NewValue = gproc:update_counter(?GPROC_KEY, 1),
        logger:debug("Counter incremented to: ~p", [NewValue])
    catch
        error:badarg ->
            %% Counter doesn't exist, create it with initial value 0, then increment
            logger:warning("Counter not registered, creating now"),
            gproc:reg(?GPROC_KEY, 0),
            gproc:update_counter(?GPROC_KEY, 1)
    end,
    ok.

%% @private Decrement global counter
-spec decrement_counter() -> ok.
decrement_counter() ->
    try
        gproc:update_counter(?GPROC_KEY, -1)
    catch
        error:badarg ->
            ok
    end,
    ok.

%% @private Check if alert threshold is exceeded
-spec check_alert_threshold(non_neg_integer(), pos_integer()) -> ok.
check_alert_threshold(CurrentCount, MaxConnections) ->
    Threshold =
        case application:get_env(erlmcp_core, connection_limiting) of
            {ok, Config} when is_map(Config) ->
                maps:get(alert_threshold, Config, ?DEFAULT_ALERT_THRESHOLD);
            {ok, Config} when is_list(Config) ->
                proplists:get_value(alert_threshold, Config, ?DEFAULT_ALERT_THRESHOLD);
            _ ->
                ?DEFAULT_ALERT_THRESHOLD
        end,

    UsageRatio = CurrentCount / MaxConnections,

    case UsageRatio >= Threshold of
        true ->
            gen_server:cast(?MODULE, {check_alert, CurrentCount, MaxConnections});
        false ->
            ok
    end.

%% @private Handle alert checking in gen_server
-spec handle_alert(non_neg_integer(), pos_integer(), #state{}) -> #state{}.
handle_alert(CurrentCount, MaxConnections, State) ->
    TimeNow = erlang:system_time(millisecond),

    case State#state.last_alert of
        undefined ->
            %% First alert
            emit_alert(CurrentCount, MaxConnections),
            State#state{last_alert = TimeNow};
        LastAlert when TimeNow - LastAlert > ?ALERT_COOLDOWN ->
            %% Cooldown expired, can alert again
            emit_alert(CurrentCount, MaxConnections),
            State#state{last_alert = TimeNow};
        _ ->
            %% Cooldown active, skip alert
            State
    end.

%% @private Emit alert log
-spec emit_alert(non_neg_integer(), pos_integer()) -> ok.
emit_alert(CurrentCount, MaxConnections) ->
    UsagePercent = CurrentCount / MaxConnections * 100,
    logger:warning("Connection limit alert: ~p/~p connections (~.1f% capacity)",
                   [CurrentCount, MaxConnections, UsagePercent]),
    ok.
