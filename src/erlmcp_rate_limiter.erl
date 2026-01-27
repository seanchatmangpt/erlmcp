%%%-------------------------------------------------------------------
%% @doc Rate Limiting and DoS Protection Module
%%
%% This module implements comprehensive rate limiting and DoS protection
%% for the erlmcp system. It provides:
%%
%% - Per-client rate limiting (messages/second)
%% - Per-client connection rate limiting
%% - Global rate limiting (requests/second)
%% - Tool execution rate limiting
%% - Resource subscription rate limiting
%% - Token bucket algorithm for standard rate limiting
%% - Sliding window for global limits
%% - Graceful degradation under load
%%
%% Configuration via sys.config:
%% ```erlang
%% {erlmcp, [
%%     {rate_limiting, #{
%%         % Per-client message rate limit (messages/second)
%%         max_messages_per_sec => 100,
%%         % Per-client connection rate limit (connections/second)
%%         max_connections_per_sec => 10,
%%         % Global message rate limit (messages/second)
%%         global_max_messages_per_sec => 10000,
%%         % Tool execution rate limit per client (calls/second)
%%         max_tool_calls_per_sec => 50,
%%         % Resource subscription rate limit per client
%%         max_subscriptions_per_sec => 20,
%%         % Token bucket refill interval (milliseconds)
%%         bucket_refill_interval_ms => 100,
%%         % DDoS protection: block after this many violations per minute
%%         ddos_violation_threshold => 100,
%%         % DDoS protection: duration to block IP (milliseconds)
%%         ddos_block_duration_ms => 300000,  % 5 minutes
%%         % Enable rate limiting (true|false)
%%         enabled => true
%%     }}
%% ]}
%% ```
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limiter).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    check_message_rate/2,
    check_connection_rate/2,
    check_tool_call_rate/2,
    check_subscription_rate/2,
    check_global_rate/1,
    get_client_info/1,
    reset_client/1,
    get_stats/0,
    is_rate_limited/1,
    is_ddos_attack/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal exports for testing
-export([
    create_token_bucket/1,
    refill_bucket/2,
    consume_token/2,
    bucket_tokens/1
]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type client_id() :: term().
-type rate_limit_result() :: {ok, non_neg_integer()} |
                              {error, rate_limited, pos_integer()}.
-type global_rate_result() :: {ok, non_neg_integer()} |
                               {error, global_rate_limited, pos_integer()}.

%% Token bucket state: {tokens, last_refill_time_ms}
-type token_bucket() :: {float(), integer()}.

%% Client tracking state: #{
%%     message_bucket => token_bucket(),
%%     connection_bucket => token_bucket(),
%%     tool_call_bucket => token_bucket(),
%%     subscription_bucket => token_bucket(),
%%     violations => non_neg_integer(),
%%     violation_window_start => integer(),
%%     blocked_until => integer() | undefined
%% }
-type client_state() :: #{
    message_bucket => token_bucket(),
    connection_bucket => token_bucket(),
    tool_call_bucket => token_bucket(),
    subscription_bucket => token_bucket(),
    violations => non_neg_integer(),
    violation_window_start => integer(),
    blocked_until => integer() | undefined
}.

%% Server state
-record(state, {
    config :: #{atom() => any()},
    clients :: ets:table(),         % ETS table for per-client buckets
    global_bucket :: token_bucket(),
    violations :: ets:table(),      % ETS table for violation tracking
    last_cleanup :: integer()       % Last cleanup timestamp
}).

-define(ETS_CLIENTS, rate_limit_clients).
-define(ETS_VIOLATIONS, rate_limit_violations).
-define(CLEANUP_INTERVAL, 60000).  % Cleanup every 60 seconds
-define(VIOLATION_WINDOW, 60000).  % 1 minute violation window

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Check if client message rate limit is exceeded
%% @param ClientId - Unique identifier for the client (IP, session ID, etc.)
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_message_rate(client_id(), integer()) -> rate_limit_result().
check_message_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false -> {ok, 999};
        true -> gen_server:call(?MODULE, {check_message_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if client connection rate limit is exceeded
%% @param ClientId - Unique identifier for the client
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_connection_rate(client_id(), integer()) -> rate_limit_result().
check_connection_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false -> {ok, 999};
        true -> gen_server:call(?MODULE, {check_connection_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if client tool call rate limit is exceeded
%% @param ClientId - Unique identifier for the client
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_tool_call_rate(client_id(), integer()) -> rate_limit_result().
check_tool_call_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false -> {ok, 999};
        true -> gen_server:call(?MODULE, {check_tool_call_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if client subscription rate limit is exceeded
%% @param ClientId - Unique identifier for the client
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_subscription_rate(client_id(), integer()) -> rate_limit_result().
check_subscription_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false -> {ok, 999};
        true -> gen_server:call(?MODULE, {check_subscription_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if global rate limit is exceeded
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, RequestsRemaining} or {error, global_rate_limited, RetryAfterMs}
-spec check_global_rate(integer()) -> global_rate_result().
check_global_rate(TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false -> {ok, 999};
        true -> gen_server:call(?MODULE, {check_global_rate, TimeNowMs})
    end.

%% @doc Get current state for a client (for monitoring)
%% @param ClientId - Unique identifier for the client
%% @returns {ok, ClientState} or {error, not_found}
-spec get_client_info(client_id()) -> {ok, client_state()} | {error, not_found}.
get_client_info(ClientId) ->
    gen_server:call(?MODULE, {get_client_info, ClientId}).

%% @doc Reset rate limit state for a client
%% @param ClientId - Unique identifier for the client
%% @returns ok
-spec reset_client(client_id()) -> ok.
reset_client(ClientId) ->
    gen_server:call(?MODULE, {reset_client, ClientId}).

%% @doc Get statistics (for monitoring and debugging)
%% @returns #{clients => Count, violations => Count, blocked_clients => []}
-spec get_stats() -> #{atom() => any()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Check if a client is rate limited (DDoS protection)
%% @param ClientId - Unique identifier for the client
%% @returns true | false
-spec is_rate_limited(client_id()) -> boolean().
is_rate_limited(ClientId) ->
    gen_server:call(?MODULE, {is_rate_limited, ClientId}).

%% @doc Check if a DDoS attack is detected for client
%% @param ClientId - Unique identifier for the client
%% @returns true | false
-spec is_ddos_attack(client_id()) -> boolean().
is_ddos_attack(ClientId) ->
    gen_server:call(?MODULE, {is_ddos_attack, ClientId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    % Load configuration
    Config = load_config(),

    % Create ETS tables for client tracking and violation tracking
    Clients = ets:new(?ETS_CLIENTS, [set, public, {read_concurrency, true}]),
    Violations = ets:new(?ETS_VIOLATIONS, [set, public, {read_concurrency, true}]),

    % Initialize global token bucket
    GlobalCapacity = maps:get(global_max_messages_per_sec, Config, 10000),
    GlobalBucket = create_token_bucket(GlobalCapacity),

    % Schedule cleanup timer
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),

    logger:info("Rate limiter started with config: ~p", [Config]),

    State = #state{
        config = Config,
        clients = Clients,
        violations = Violations,
        global_bucket = GlobalBucket,
        last_cleanup = erlang:system_time(millisecond)
    },
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call({check_message_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_messages_per_sec, State#state.config, 100),
    Result = check_rate(ClientId, message_bucket, MaxRate, TimeNowMs, State),
    {reply, Result, State};

handle_call({check_connection_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_connections_per_sec, State#state.config, 10),
    Result = check_rate(ClientId, connection_bucket, MaxRate, TimeNowMs, State),
    {reply, Result, State};

handle_call({check_tool_call_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_tool_calls_per_sec, State#state.config, 50),
    Result = check_rate(ClientId, tool_call_bucket, MaxRate, TimeNowMs, State),
    {reply, Result, State};

handle_call({check_subscription_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_subscriptions_per_sec, State#state.config, 20),
    Result = check_rate(ClientId, subscription_bucket, MaxRate, TimeNowMs, State),
    {reply, Result, State};

handle_call({check_global_rate, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(global_max_messages_per_sec, State#state.config, 10000),
    {Result, NewBucket} = check_global_rate_internal(State#state.global_bucket, MaxRate, TimeNowMs),
    {reply, Result, State#state{global_bucket = NewBucket}};

handle_call({get_client_info, ClientId}, _From, State) ->
    case ets:lookup(State#state.clients, ClientId) of
        [{_, ClientState}] ->
            {reply, {ok, ClientState}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({reset_client, ClientId}, _From, State) ->
    ets:delete(State#state.clients, ClientId),
    ets:delete(State#state.violations, ClientId),
    {reply, ok, State};

handle_call(get_stats, _From, State) ->
    ClientCount = ets:info(State#state.clients, size),
    ViolationCount = ets:info(State#state.violations, size),
    TimeNowMs = erlang:system_time(millisecond),
    BlockedClients = ets:foldl(fun
        ({CId, ClientState}, Acc) ->
            case ClientState of
                #{blocked_until := Until} when Until > TimeNowMs ->
                    [CId | Acc];
                _ ->
                    Acc
            end
    end, [], State#state.clients),

    Stats = #{
        clients => ClientCount,
        violations => ViolationCount,
        blocked_clients => BlockedClients,
        config => State#state.config
    },
    {reply, Stats, State};

handle_call({is_rate_limited, ClientId}, _From, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    case ets:lookup(State#state.clients, ClientId) of
        [{_, ClientState}] ->
            case ClientState of
                #{blocked_until := Until} when Until > TimeNowMs ->
                    {reply, true, State};
                _ ->
                    {reply, false, State}
            end;
        [] ->
            {reply, false, State}
    end;

handle_call({is_ddos_attack, ClientId}, _From, State) ->
    case ets:lookup(State#state.violations, ClientId) of
        [{_, Count}] ->
            Threshold = maps:get(ddos_violation_threshold, State#state.config, 100),
            {reply, Count >= Threshold, State};
        [] ->
            {reply, false, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup, State) ->
    % Cleanup expired client entries and blocked status
    TimeNowMs = erlang:system_time(millisecond),

    % Remove entries for clients whose block duration has expired
    ets:foldl(fun({ClientId, ClientState}, _) ->
        case ClientState of
            #{blocked_until := Until} when Until < TimeNowMs ->
                ets:delete(State#state.clients, ClientId);
            _ ->
                ok
        end
    end, ok, State#state.clients),

    % Clean up violation counts older than block duration
    ViolationWindow = maps:get(ddos_block_duration_ms, State#state.config, 300000),
    ets:foldl(fun({ClientId, _}, _) ->
        case ets:lookup(State#state.violations, ClientId) of
            [{_, {_Count, Timestamp}}] when (TimeNowMs - Timestamp) > ViolationWindow ->
                ets:delete(State#state.violations, ClientId);
            _ ->
                ok
        end
    end, ok, State#state.violations),

    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {noreply, State#state{last_cleanup = TimeNowMs}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.clients),
    ets:delete(State#state.violations),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Load configuration from sys.config
-spec load_config() -> #{atom() => any()}.
load_config() ->
    DefaultConfig = #{
        max_messages_per_sec => 100,
        max_connections_per_sec => 10,
        global_max_messages_per_sec => 10000,
        max_tool_calls_per_sec => 50,
        max_subscriptions_per_sec => 20,
        bucket_refill_interval_ms => 100,
        ddos_violation_threshold => 100,
        ddos_block_duration_ms => 300000,
        enabled => true
    },

    case application:get_env(erlmcp, rate_limiting) of
        {ok, Config} when is_map(Config) ->
            maps:merge(DefaultConfig, Config);
        {ok, Config} when is_list(Config) ->
            maps:merge(DefaultConfig, maps:from_list(Config));
        _ ->
            DefaultConfig
    end.

%% @private Check if rate limiting is enabled
-spec is_rate_limiting_enabled() -> boolean().
is_rate_limiting_enabled() ->
    case application:get_env(erlmcp, rate_limiting) of
        {ok, Config} ->
            Enabled = case Config of
                M when is_map(M) -> maps:get(enabled, M, true);
                L when is_list(L) -> proplists:get_value(enabled, L, true);
                _ -> true
            end,
            Enabled;
        _ ->
            true
    end.

%% @private Check rate limit for a specific bucket type
-spec check_rate(client_id(), atom(), float(), integer(), #state{}) ->
    rate_limit_result().
check_rate(ClientId, BucketType, MaxRate, TimeNowMs, State) ->
    % Check if client is DDoS-blocked
    ClientStateResult = ets:lookup(State#state.clients, ClientId),
    case ClientStateResult of
        [{_, ClientState}] ->
            % Check if blocked
            case ClientState of
                #{blocked_until := Until} when is_integer(Until), Until > TimeNowMs ->
                    % Client is blocked due to DDoS protection
                    RetryAfterMs = Until - TimeNowMs,
                    logger:warning("Client ~p is rate limited until ~p (DoS protection)",
                                 [ClientId, Until]),
                    {error, rate_limited, RetryAfterMs};
                _ ->
                    % Not blocked, check rate limit
                    check_rate_internal(ClientId, BucketType, MaxRate, TimeNowMs, ClientState, State)
            end;
        [] ->
            % New client, create state
            NewClientState = create_client_state(),
            check_rate_internal(ClientId, BucketType, MaxRate, TimeNowMs, NewClientState, State)
    end.

%% @private Internal rate check after DDoS blocking check
-spec check_rate_internal(client_id(), atom(), float(), integer(), client_state(), #state{}) ->
    rate_limit_result().
check_rate_internal(ClientId, BucketType, MaxRate, _TimeNowMs, ClientState, State) ->
    % Get the appropriate bucket
    Bucket = maps:get(BucketType, ClientState, create_token_bucket(MaxRate)),
    RefillIntervalMs = maps:get(bucket_refill_interval_ms, State#state.config, 100),

    % Try to consume a token
    case consume_token(Bucket, MaxRate) of
        {ok, NewBucket, TokensRemaining} ->
            % Update client state
            NewClientState = ClientState#{BucketType => NewBucket},
            ets:insert(State#state.clients, {ClientId, NewClientState}),
            logger:debug("Client ~p passed rate limit check (~p tokens remaining)",
                       [ClientId, TokensRemaining]),
            {ok, TokensRemaining};
        {error, exceeded} ->
            % Rate limit exceeded - increment violation counter
            increment_violations(ClientId, State),

            % Calculate retry-after time
            RefillTime = RefillIntervalMs,
            logger:warning("Client ~p exceeded rate limit (max ~p/sec)",
                         [ClientId, MaxRate]),
            {error, rate_limited, RefillTime}
    end.

%% @private Check global rate limit
-spec check_global_rate_internal(token_bucket(), float() | integer(), integer()) ->
    {global_rate_result(), token_bucket()}.
check_global_rate_internal(Bucket, MaxRate, _TimeNowMs) ->
    case consume_token(Bucket, MaxRate) of
        {ok, NewBucket, RequestsRemaining} ->
            logger:debug("Global rate limit check passed (~p requests remaining)",
                       [RequestsRemaining]),
            {{ok, RequestsRemaining}, NewBucket};
        {error, exceeded} ->
            logger:warning("Global rate limit exceeded (max ~p/sec)", [MaxRate]),
            {{error, global_rate_limited, 100}, Bucket}
    end.

%% @private Increment violation counter for DDoS detection
-spec increment_violations(client_id(), #state{}) -> ok.
increment_violations(ClientId, State) ->
    TimeNowMs = erlang:system_time(millisecond),

    case ets:lookup(State#state.violations, ClientId) of
        [{_, {Count, Timestamp}}] ->
            % Check if violation window has expired
            ViolationWindow = ?VIOLATION_WINDOW,
            if
                (TimeNowMs - Timestamp) > ViolationWindow ->
                    % Window expired, reset counter
                    ets:insert(State#state.violations, {ClientId, {1, TimeNowMs}});
                true ->
                    % Increment counter
                    NewCount = Count + 1,
                    ets:insert(State#state.violations, {ClientId, {NewCount, Timestamp}}),

                    % Check if DDoS threshold exceeded
                    Threshold = maps:get(ddos_violation_threshold, State#state.config, 100),
                    if
                        NewCount >= Threshold ->
                            % Block client
                            BlockDurationMs = maps:get(ddos_block_duration_ms, State#state.config, 300000),
                            BlockUntil = TimeNowMs + BlockDurationMs,
                            case ets:lookup(State#state.clients, ClientId) of
                                [{_, ClientState}] ->
                                    ets:insert(State#state.clients, {ClientId, ClientState#{blocked_until => BlockUntil}});
                                [] ->
                                    NewClientState = create_client_state(),
                                    ets:insert(State#state.clients, {ClientId, NewClientState#{blocked_until => BlockUntil}})
                            end,
                            logger:error("Client ~p blocked due to DDoS attack (violations: ~p)",
                                       [ClientId, NewCount]);
                        true ->
                            ok
                    end
            end;
        [] ->
            ets:insert(State#state.violations, {ClientId, {1, TimeNowMs}})
    end,
    ok.

%% @private Create initial client state
-spec create_client_state() -> client_state().
create_client_state() ->
    #{
        message_bucket => create_token_bucket(100),
        connection_bucket => create_token_bucket(10),
        tool_call_bucket => create_token_bucket(50),
        subscription_bucket => create_token_bucket(20),
        violations => 0,
        violation_window_start => erlang:system_time(millisecond),
        blocked_until => undefined
    }.

%% @doc Create a token bucket with specified capacity
%% @param Capacity - Maximum tokens in bucket (tokens per second)
%% @returns Token bucket state {Tokens, LastRefillMs}
-spec create_token_bucket(float() | integer()) -> token_bucket().
create_token_bucket(Capacity) when is_number(Capacity) ->
    TimeNowMs = erlang:system_time(millisecond),
    {float(Capacity), TimeNowMs}.

%% @doc Refill bucket based on elapsed time
%% @param Bucket - Current bucket state
%% @param Capacity - Capacity (tokens per second)
%% @returns Updated bucket state
-spec refill_bucket(token_bucket(), float() | integer()) -> token_bucket().
refill_bucket({Tokens, LastRefillMs}, Capacity) ->
    TimeNowMs = erlang:system_time(millisecond),
    ElapsedMs = TimeNowMs - LastRefillMs,

    % Add tokens based on elapsed time
    % Capacity is tokens per second, so we calculate: Capacity * (ElapsedMs / 1000)
    TokensToAdd = (Capacity * ElapsedMs) / 1000.0,
    NewTokens = min(Tokens + TokensToAdd, float(Capacity)),

    {NewTokens, TimeNowMs}.

%% @doc Consume one token from bucket
%% @param Bucket - Current bucket state
%% @param Capacity - Capacity (tokens per second)
%% @returns {ok, NewBucket, TokensRemaining} | {error, exceeded}
-spec consume_token(token_bucket(), float() | integer()) ->
    {ok, token_bucket(), non_neg_integer()} | {error, exceeded}.
consume_token(Bucket, Capacity) ->
    RefilledBucket = refill_bucket(Bucket, Capacity),
    {Tokens, _LastRefillMs} = RefilledBucket,

    if
        Tokens >= 1.0 ->
            NewTokens = Tokens - 1.0,
            {ok, {NewTokens, erlang:system_time(millisecond)}, round(NewTokens)};
        true ->
            {error, exceeded}
    end.

%% @doc Get current tokens in bucket (for testing)
%% @param Bucket - Token bucket state
%% @returns Number of tokens
-spec bucket_tokens(token_bucket()) -> float().
bucket_tokens({Tokens, _LastRefillMs}) ->
    Tokens.

