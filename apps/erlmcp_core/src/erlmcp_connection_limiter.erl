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
-export([
    start_link/0,
    stop/0,
    accept_connection/1,
    release_connection/1,
    get_connection_count/0,
    get_connection_count/1,
    set_limit/1,
    get_limit/0,
    get_stats/0,
    is_limit_enabled/0
]).

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
-type stats() :: #{
    current_connections => non_neg_integer(),
    max_connections => non_neg_integer(),
    alert_threshold => float(),
    last_alert => integer() | undefined
}.

%% Server state
-record(state, {
    max_connections :: pos_integer(),
    alert_threshold :: float(),
    last_alert :: integer() | undefined
}).

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

%% @doc Check if connection should be accepted
%% Returns 'accept' or '{error, too_many_connections}'
-spec accept_connection(server_id()) -> limit_result().
accept_connection(ServerId) ->
    case is_limit_enabled() of
        false ->
            accept;
        true ->
            MaxConnections = get_limit(),
            CurrentCount = get_connection_count(),

            case CurrentCount < MaxConnections of
                true ->
                    %% Increment counter
                    increment_counter(),
                    increment_server_counter(ServerId),

                    %% Check for alert threshold
                    check_alert_threshold(CurrentCount, MaxConnections),

                    accept;
                false ->
                    logger:warning("Connection limit exceeded: ~p/~p for server ~p",
                                 [CurrentCount, MaxConnections, ServerId]),
                    {error, too_many_connections}
            end
    end.

%% @doc Release a connection slot
-spec release_connection(server_id()) -> ok.
release_connection(ServerId) ->
    case is_limit_enabled() of
        false ->
            ok;
        true ->
            decrement_counter(),
            decrement_server_counter(ServerId),
            ok
    end.

%% @doc Get current global connection count
-spec get_connection_count() -> non_neg_integer().
get_connection_count() ->
    case is_limit_enabled() of
        false ->
            0;
        true ->
            gen_server:call(?MODULE, get_connection_count)
    end.

%% @doc Get connection count for a specific server
-spec get_connection_count(server_id()) -> non_neg_integer().
get_connection_count(ServerId) ->
    case is_limit_enabled() of
        false ->
            0;
        true ->
            try
                Key = {c, l, {erlmcp_server_connections, ServerId}},
                case gproc:lookup_value(Key) of
                    Value when is_integer(Value) ->
                        Value;
                    _ ->
                        0
                end
            catch
                error:badarg ->
                    0
            end
    end.

%% @doc Set the maximum connection limit
-spec set_limit(pos_integer()) -> ok.
set_limit(Limit) when is_integer(Limit), Limit > 0 ->
    gen_server:call(?MODULE, {set_limit, Limit}).

%% @doc Get the current maximum connection limit
-spec get_limit() -> pos_integer().
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
    case application:get_env(erlmcp, connection_limiting) of
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
    %% Load configuration
    MaxConnections = load_config(max_connections, ?DEFAULT_MAX_CONNECTIONS),
    AlertThreshold = load_config(alert_threshold, ?DEFAULT_ALERT_THRESHOLD),

    %% Initialize gproc counter
    try
        gproc:reg(?GPROC_KEY, 0),
        logger:info("Connection limiter started: max=~p, alert_threshold=~p",
                   [MaxConnections, AlertThreshold])
    catch
        error:badarg ->
            %% Already registered
            ok
    end,

    {ok, #state{
        max_connections = MaxConnections,
        alert_threshold = AlertThreshold,
        last_alert = undefined
    }}.

handle_call(get_connection_count, _From, State) ->
    Count = get_counter_value(),
    {reply, Count, State};

handle_call({set_limit, Limit}, _From, State) when is_integer(Limit), Limit > 0 ->
    logger:info("Connection limit updated: ~p -> ~p",
               [State#state.max_connections, Limit]),
    {reply, ok, State#state{max_connections = Limit}};

handle_call(get_limit, _From, State) ->
    {reply, State#state.max_connections, State};

handle_call(get_stats, _From, State) ->
    Count = get_counter_value(),
    Stats = #{
        current_connections => Count,
        max_connections => State#state.max_connections,
        alert_threshold => State#state.alert_threshold,
        last_alert => State#state.last_alert
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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
    case application:get_env(erlmcp, connection_limiting) of
        {ok, Config} when is_map(Config) ->
            maps:get(Key, Config, Default);
        {ok, Config} when is_list(Config) ->
            proplists:get_value(Key, Config, Default);
        _ ->
            Default
    end.

%% @private Get current counter value
-spec get_counter_value() -> non_neg_integer().
get_counter_value() ->
    try
        case gproc:lookup_value(?GPROC_KEY) of
            Value when is_integer(Value) ->
                Value;
            _ ->
                0
        end
    catch
        error:badarg ->
            0
    end.

%% @private Increment global counter
-spec increment_counter() -> ok.
increment_counter() ->
    try
        gproc:update_counter(?GPROC_KEY, 1)
    catch
        error:badarg ->
            %% Counter doesn't exist, create it
            gproc:reg(?GPROC_KEY, 1)
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

%% @private Increment server-specific counter
-spec increment_server_counter(server_id()) -> ok.
increment_server_counter(ServerId) ->
    Key = {c, l, {erlmcp_server_connections, ServerId}},
    try
        gproc:update_counter(Key, 1)
    catch
        error:badarg ->
            %% Counter doesn't exist, create it
            gproc:reg(Key, 1)
    end,
    ok.

%% @private Decrement server-specific counter
-spec decrement_server_counter(server_id()) -> ok.
decrement_server_counter(ServerId) ->
    try
        Key = {c, l, {erlmcp_server_connections, ServerId}},
        gproc:update_counter(Key, -1)
    catch
        error:badarg ->
            ok
    end,
    ok.

%% @private Check if alert threshold is exceeded
-spec check_alert_threshold(non_neg_integer(), pos_integer()) -> ok.
check_alert_threshold(CurrentCount, MaxConnections) ->
    Threshold = case application:get_env(erlmcp, connection_limiting) of
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
        LastAlert when (TimeNow - LastAlert) > ?ALERT_COOLDOWN ->
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
    UsagePercent = (CurrentCount / MaxConnections) * 100,
    logger:warning("Connection limit alert: ~p/~p connections (~.1f% capacity)",
                 [CurrentCount, MaxConnections, UsagePercent]),
    ok.
