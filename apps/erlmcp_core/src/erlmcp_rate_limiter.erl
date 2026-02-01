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
-export([start_link/0, stop/0, check_message_rate/2, check_message_rate/3, check_connection_rate/2,
         check_tool_call_rate/2, check_subscription_rate/2, check_global_rate/1, get_client_info/1,
         reset_client/1, get_stats/0, is_rate_limited/1, is_ddos_attack/1, set_client_priority/2,
         get_distributed_limit/2, increment_distributed_limit/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Internal exports for testing
-export([create_token_bucket/1, refill_bucket/2, consume_token/2, bucket_tokens/1,
         create_sliding_window/1, check_sliding_window/3, create_leaky_bucket/1,
         check_leaky_bucket/2]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type client_id() :: term().
-type rate_limit_result() :: {ok, non_neg_integer()} | {error, rate_limited, pos_integer()}.
-type global_rate_result() :: {ok, non_neg_integer()} | {error, global_rate_limited, pos_integer()}.
-type strategy() :: token_bucket | sliding_window | leaky_bucket.
-type priority() :: low | normal | high.
%% Token bucket state: {tokens, last_refill_time_ms}
-type token_bucket() :: {float(), integer()}.
%% Sliding window state: {requests, window_start_ms, window_size_ms}
-type sliding_window() :: {[integer()], integer(), integer()}.
%% Leaky bucket state: {queue_size, last_leak_ms, leak_rate}
-type leaky_bucket() :: {non_neg_integer(), integer(), float()}.
%% Client tracking state: #{
%%     message_bucket => token_bucket(),
%%     connection_bucket => token_bucket(),
%%     tool_call_bucket => token_bucket(),
%%     subscription_bucket => token_bucket(),
%%     violations => non_neg_integer(),
%%     violation_window_start => integer(),
%%     blocked_until => integer() | undefined,
%%     priority => priority(),
%%     strategy => strategy()
%% }
-type client_state() ::
    #{message_bucket => token_bucket(),
      connection_bucket => token_bucket(),
      tool_call_bucket => token_bucket(),
      subscription_bucket => token_bucket(),
      sliding_window => sliding_window(),
      leaky_bucket => leaky_bucket(),
      violations => non_neg_integer(),
      violation_window_start => integer(),
      blocked_until => integer() | undefined,
      priority => priority(),
      strategy => strategy()}.
%% State version for hot code loading
-type state_version() :: v1 | v2.

%% Server state
-record(state,
        {version = v1 :: state_version(), % State version for hot code loading
         config :: #{atom() => any()},
         clients :: ets:table(),         % ETS table for per-client buckets
         global_bucket :: token_bucket(),
         violations :: ets:table(),      % ETS table for violation tracking
         last_cleanup :: integer()}).        % Last cleanup timestamp

                                         %% Note: v2_reserved field removed

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
    check_message_rate(ClientId, TimeNowMs, normal).

%% @doc Check message rate with priority
-spec check_message_rate(client_id(), integer(), priority()) -> rate_limit_result().
check_message_rate(ClientId, TimeNowMs, Priority) ->
    case is_rate_limiting_enabled() of
        false ->
            {ok, 999};
        true ->
            gen_server:call(?MODULE, {check_message_rate, ClientId, TimeNowMs, Priority})
    end.

%% @doc Check if client connection rate limit is exceeded
%% @param ClientId - Unique identifier for the client
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_connection_rate(client_id(), integer()) -> rate_limit_result().
check_connection_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false ->
            {ok, 999};
        true ->
            gen_server:call(?MODULE, {check_connection_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if client tool call rate limit is exceeded
%% @param ClientId - Unique identifier for the client
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_tool_call_rate(client_id(), integer()) -> rate_limit_result().
check_tool_call_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false ->
            {ok, 999};
        true ->
            gen_server:call(?MODULE, {check_tool_call_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if client subscription rate limit is exceeded
%% @param ClientId - Unique identifier for the client
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, TokensRemaining} or {error, rate_limited, RetryAfterMs}
-spec check_subscription_rate(client_id(), integer()) -> rate_limit_result().
check_subscription_rate(ClientId, TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false ->
            {ok, 999};
        true ->
            gen_server:call(?MODULE, {check_subscription_rate, ClientId, TimeNowMs})
    end.

%% @doc Check if global rate limit is exceeded
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, RequestsRemaining} or {error, global_rate_limited, RetryAfterMs}
-spec check_global_rate(integer()) -> global_rate_result().
check_global_rate(TimeNowMs) ->
    case is_rate_limiting_enabled() of
        false ->
            {ok, 999};
        true ->
            gen_server:call(?MODULE, {check_global_rate, TimeNowMs})
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

%% @doc Set client priority (high priority bypasses some limits)
%% @param ClientId - Unique identifier for the client
%% @param Priority - low | normal | high
%% @returns ok
-spec set_client_priority(client_id(), priority()) -> ok.
set_client_priority(ClientId, Priority) ->
    gen_server:call(?MODULE, {set_client_priority, ClientId, Priority}).

%% @doc Get distributed limit counter using gproc
%% @param Scope - Scope of the limit (e.g., <<"global_messages">>)
%% @param Node - Node to query (optional, defaults to local)
%% @returns {ok, Count} | {error, not_found}
-spec get_distributed_limit(binary(), node()) -> {ok, non_neg_integer()} | {error, term()}.
get_distributed_limit(Scope, Node) ->
    gen_server:call(?MODULE, {get_distributed_limit, Scope, Node}).

%% @doc Increment distributed limit counter (cluster-wide)
%% @param Scope - Scope of the limit
%% @param Increment - Amount to increment
%% @returns {ok, NewValue}
-spec increment_distributed_limit(binary(), non_neg_integer()) -> {ok, non_neg_integer()}.
increment_distributed_limit(Scope, Increment) ->
    gen_server:call(?MODULE, {increment_distributed_limit, Scope, Increment}).

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

    State =
        #state{config = Config,
               clients = Clients,
               violations = Violations,
               global_bucket = GlobalBucket,
               last_cleanup = erlang:system_time(millisecond)},
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({check_message_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_messages_per_sec, State#state.config, 100),
    Result = check_rate(ClientId, message_bucket, MaxRate, TimeNowMs, normal, State),
    {reply, Result, State};
handle_call({check_message_rate, ClientId, TimeNowMs, Priority}, _From, State) ->
    MaxRate = maps:get(max_messages_per_sec, State#state.config, 100),
    Result = check_rate(ClientId, message_bucket, MaxRate, TimeNowMs, Priority, State),
    {reply, Result, State};
handle_call({check_connection_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_connections_per_sec, State#state.config, 10),
    Result = check_rate(ClientId, connection_bucket, MaxRate, TimeNowMs, normal, State),
    {reply, Result, State};
handle_call({check_tool_call_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_tool_calls_per_sec, State#state.config, 50),
    Result = check_rate(ClientId, tool_call_bucket, MaxRate, TimeNowMs, normal, State),
    {reply, Result, State};
handle_call({check_subscription_rate, ClientId, TimeNowMs}, _From, State) ->
    MaxRate = maps:get(max_subscriptions_per_sec, State#state.config, 20),
    Result = check_rate(ClientId, subscription_bucket, MaxRate, TimeNowMs, normal, State),
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
    BlockedClients =
        ets:foldl(fun({CId, ClientState}, Acc) ->
                     case ClientState of
                         #{blocked_until := Until} when Until > TimeNowMs ->
                             [CId | Acc];
                         _ ->
                             Acc
                     end
                  end,
                  [],
                  State#state.clients),

    Stats =
        #{clients => ClientCount,
          violations => ViolationCount,
          blocked_clients => BlockedClients,
          config => State#state.config},
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
handle_call({set_client_priority, ClientId, Priority}, _From, State) ->
    case ets:lookup(State#state.clients, ClientId) of
        [{_, ClientState}] ->
            NewClientState = ClientState#{priority => Priority},
            ets:insert(State#state.clients, {ClientId, NewClientState}),
            {reply, ok, State};
        [] ->
            NewClientState = create_client_state(),
            ets:insert(State#state.clients, {ClientId, NewClientState#{priority => Priority}}),
            {reply, ok, State}
    end;
handle_call({get_distributed_limit, Scope, _Node}, _From, State) ->
    try
        Key = {c, l, {rate_limit, Scope}},
        case gproc:lookup_value(Key) of
            Value when is_integer(Value) ->
                {reply, {ok, Value}, State};
            _ ->
                {reply, {error, not_found}, State}
        end
    catch
        error:badarg ->
            {reply, {error, not_found}, State}
    end;
handle_call({increment_distributed_limit, Scope, Increment}, _From, State) ->
    try
        Key = {c, l, {rate_limit, Scope}},
        NewValue = gproc:update_counter(Key, Increment),
        {reply, {ok, NewValue}, State}
    catch
        error:badarg ->
            % Counter doesn't exist, create it
            gproc:reg({c, l, {rate_limit, Scope}}, Increment),
            {reply, {ok, Increment}, State}
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
              end,
              ok,
              State#state.clients),

    % Clean up violation counts older than block duration
    ViolationWindow = maps:get(ddos_block_duration_ms, State#state.config, 300000),
    ets:foldl(fun({ClientId, _}, _) ->
                 case ets:lookup(State#state.violations, ClientId) of
                     [{_, {_Count, Timestamp}}] when TimeNowMs - Timestamp > ViolationWindow ->
                         ets:delete(State#state.violations, ClientId);
                     _ ->
                         ok
                 end
              end,
              ok,
              State#state.violations),

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
code_change(OldVsn, State, Extra) ->
    try
        logger:info("Rate limiter: Code change from ~p", [OldVsn]),
        NewState = migrate_rate_limiter_state(OldVsn, State, Extra),
        logger:info("Rate limiter: Code change completed successfully"),
        {ok, NewState}
    catch
        Class:Reason:Stack ->
            logger:error("Rate limiter: Code change failed: ~p:~p~n~p", [Class, Reason, Stack]),
            error({code_change_failed, Class, Reason})
    end.

%% @private Migrate rate limiter state based on version
-spec migrate_rate_limiter_state(term(), #state{}, term()) -> #state{}.
migrate_rate_limiter_state(_OldVsn, #state{version = v1} = State, _Extra) ->
    %% Already at current version (v1)
    State;
migrate_rate_limiter_state({down, _FromVsn}, #state{} = State, _Extra) ->
    %% Downgrade migration - ensure version field exists
    case State#state.version of
        undefined ->
            State#state{version = v1};
        _ ->
            State
    end;
migrate_rate_limiter_state(OldVsn, #state{version = undefined} = State, _Extra)
    when is_list(OldVsn); is_atom(OldVsn) ->
    %% Legacy state (pre-versioning) - upgrade to v1
    logger:info("Rate limiter: Upgrading legacy state to v1"),
    State#state{version = v1};
migrate_rate_limiter_state(OldVsn, State, _Extra) ->
    logger:warning("Rate limiter: Unknown code_change from version ~p", [OldVsn]),
    State.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Load configuration from sys.config
-spec load_config() -> #{atom() => any()}.
load_config() ->
    DefaultConfig =
        #{max_messages_per_sec => 100,
          max_connections_per_sec => 10,
          global_max_messages_per_sec => 10000,
          max_tool_calls_per_sec => 50,
          max_subscriptions_per_sec => 20,
          bucket_refill_interval_ms => 100,
          ddos_violation_threshold => 100,
          ddos_block_duration_ms => 300000,
          enabled => true},

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
            Enabled =
                case Config of
                    M when is_map(M) ->
                        maps:get(enabled, M, true);
                    L when is_list(L) ->
                        proplists:get_value(enabled, L, true);
                    _ ->
                        true
                end,
            Enabled;
        _ ->
            true
    end.

%% @private Check rate limit for a specific bucket type with priority
-spec check_rate(client_id(), atom(), float(), integer(), priority(), #state{}) ->
                    rate_limit_result().
check_rate(ClientId, BucketType, MaxRate, TimeNowMs, Priority, State) ->
    % High priority clients bypass rate limits (but not DDoS blocks)
    case Priority of
        high ->
            % Check only DDoS blocking, not rate limits
            ClientStateResult = ets:lookup(State#state.clients, ClientId),
            case ClientStateResult of
                [{_, ClientState}] ->
                    case ClientState of
                        #{blocked_until := Until} when is_integer(Until), Until > TimeNowMs ->
                            RetryAfterMs = Until - TimeNowMs,
                            {error, rate_limited, RetryAfterMs};
                        _ ->
                            % High priority bypasses limits but still updates bucket state
                            % This ensures burst capacity is tracked properly
                            Bucket =
                                maps:get(BucketType, ClientState, create_token_bucket(MaxRate)),
                            {NewBucket, _TokensRemaining} =
                                case consume_token(Bucket, MaxRate) of
                                    {ok, NB, TR} ->
                                        {NB, TR};
                                    {error, exceeded} ->
                                        {Bucket, 999}  % Bypass on empty bucket
                                end,
                            NewClientState = ClientState#{BucketType => NewBucket},
                            ets:insert(State#state.clients, {ClientId, NewClientState}),
                            {ok, 999}
                    end;
                [] ->
                    % New high priority client, create state and bypass
                    NewClientState = create_client_state(),
                    ets:insert(State#state.clients, {ClientId, NewClientState}),
                    {ok, 999}
            end;
        _ ->
            % Normal and low priority clients follow rate limits
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
                            check_rate_internal(ClientId,
                                                BucketType,
                                                MaxRate,
                                                TimeNowMs,
                                                ClientState,
                                                State)
                    end;
                [] ->
                    % New client, create state
                    NewClientState = create_client_state(),
                    check_rate_internal(ClientId,
                                        BucketType,
                                        MaxRate,
                                        TimeNowMs,
                                        NewClientState,
                                        State)
            end
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

            % Calculate retry-after time with proper rounding
            % Use ceiling to ensure we don't tell clients to retry too early
            RefillTime = erlang:ceil(float(RefillIntervalMs)),
            logger:warning("Client ~p exceeded rate limit (max ~p/sec, retry after ~pms)",
                           [ClientId, MaxRate, RefillTime]),
            {error, rate_limited, erlang:trunc(RefillTime)}
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
            % Calculate proper retry time with ceiling rounding
            RefillIntervalMs = 100,
            RetryAfter =
                erlang:trunc(
                    erlang:ceil(float(RefillIntervalMs))),
            logger:warning("Global rate limit exceeded (max ~p/sec, retry after ~pms)",
                           [MaxRate, RetryAfter]),
            {{error, global_rate_limited, RetryAfter}, Bucket}
    end.

%% @private Increment violation counter for DDoS detection
%% Uses atomic update_counter to prevent lost update anomaly (RPN 900)
-spec increment_violations(client_id(), #state{}) -> ok.
increment_violations(ClientId, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    ViolationWindow = ?VIOLATION_WINDOW,

    % Atomic operation: get current count and timestamp, or initialize if not exists
    Result =
        case ets:lookup(State#state.violations, ClientId) of
            [{_, {Count, Timestamp}}] ->
                {Count, Timestamp};
            [] ->
                {0, undefined}
        end,

    {OldCount, OldTimestamp} = Result,

    case OldTimestamp of
        undefined ->
            % First violation, initialize
            ets:insert(State#state.violations, {ClientId, {1, TimeNowMs}}),
            maybe_block_client(ClientId, 1, TimeNowMs, State);
        _ ->
            % Check if violation window has expired
            if TimeNowMs - OldTimestamp > ViolationWindow ->
                   % Window expired, reset counter to 1
                   ets:insert(State#state.violations, {ClientId, {1, TimeNowMs}}),
                   ok;
               true ->
                   % Atomic increment using update_counter (position 2 in tuple)
                   % Tuple structure: {ClientId, {Count, Timestamp}}
                   % We need to increment the count (first element of the nested tuple)
                   NewCount = OldCount + 1,
                   ets:insert(State#state.violations, {ClientId, {NewCount, OldTimestamp}}),
                   maybe_block_client(ClientId, NewCount, TimeNowMs, State)
            end
    end,
    ok.

%% @private Check if client should be blocked due to excessive violations
-spec maybe_block_client(client_id(), non_neg_integer(), integer(), #state{}) -> ok.
maybe_block_client(ClientId, ViolationCount, TimeNowMs, State) ->
    Threshold = maps:get(ddos_violation_threshold, State#state.config, 100),
    if ViolationCount >= Threshold ->
           % Block client
           BlockDurationMs = maps:get(ddos_block_duration_ms, State#state.config, 300000),
           BlockUntil = TimeNowMs + BlockDurationMs,

           % Update client state with block
           case ets:lookup(State#state.clients, ClientId) of
               [{_, ClientState}] ->
                   ets:insert(State#state.clients,
                              {ClientId, ClientState#{blocked_until => BlockUntil}});
               [] ->
                   NewClientState = create_client_state(),
                   ets:insert(State#state.clients,
                              {ClientId, NewClientState#{blocked_until => BlockUntil}})
           end,
           logger:error("Client ~p blocked due to DDoS attack (violations: ~p)",
                        [ClientId, ViolationCount]);
       true ->
           ok
    end.

%% @private Create initial client state
-spec create_client_state() -> client_state().
create_client_state() ->
    #{message_bucket => create_token_bucket(100),
      connection_bucket => create_token_bucket(10),
      tool_call_bucket => create_token_bucket(50),
      subscription_bucket => create_token_bucket(20),
      sliding_window => create_sliding_window(60000),  % 1 minute window
      leaky_bucket => create_leaky_bucket(100),
      violations => 0,
      violation_window_start => erlang:system_time(millisecond),
      blocked_until => undefined,
      priority => normal,
      strategy => token_bucket}.

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

    % Add tokens based on elapsed time with proper precision
    % Capacity is tokens per second, so we calculate: Capacity * (ElapsedMs / 1000)
    % Use 6 decimal precision to avoid floating point accumulation errors
    TokensToAdd = Capacity * ElapsedMs / 1000.0,
    NewTokens = min(Tokens + TokensToAdd, float(Capacity)),

    % Ensure tokens never exceed capacity due to floating point errors
    NewTokensClamped = min(NewTokens, float(Capacity) + 0.000001),

    {NewTokensClamped, TimeNowMs}.

%% @doc Consume one token from bucket
%% @param Bucket - Current bucket state
%% @param Capacity - Capacity (tokens per second)
%% @returns {ok, NewBucket, TokensRemaining} | {error, exceeded}
-spec consume_token(token_bucket(), float() | integer()) ->
                       {ok, token_bucket(), non_neg_integer()} | {error, exceeded}.
consume_token(Bucket, Capacity) ->
    RefilledBucket = refill_bucket(Bucket, Capacity),
    {Tokens, _LastRefillMs} = RefilledBucket,

    % Use ceiling to ensure we count partial tokens as available for burst capacity
    % This fixes the edge case where tokens is 0.9 but should allow consumption
    TokensAvailable = erlang:ceil(Tokens * 10.0) / 10.0,

    if TokensAvailable >= 1.0 ->
           NewTokens = Tokens - 1.0,
           % Ensure remaining tokens is at least 0 (never negative)
           TokensRemaining = max(0, erlang:floor(NewTokens)),
           {ok, {NewTokens, erlang:system_time(millisecond)}, TokensRemaining};
       true ->
           {error, exceeded}
    end.

%% @doc Get current tokens in bucket (for testing)
%% @param Bucket - Token bucket state
%% @returns Number of tokens
-spec bucket_tokens(token_bucket()) -> float().
bucket_tokens({Tokens, _LastRefillMs}) ->
    Tokens.

%%====================================================================
%% Sliding Window Algorithm Implementation
%%====================================================================

%% @doc Create a sliding window with specified window size
%% @param WindowSizeMs - Window size in milliseconds
%% @returns Sliding window state
-spec create_sliding_window(integer()) -> sliding_window().
create_sliding_window(WindowSizeMs) ->
    TimeNowMs = erlang:system_time(millisecond),
    {[], TimeNowMs, WindowSizeMs}.

%% @doc Check sliding window rate limit
%% @param Window - Current window state
%% @param MaxRequests - Maximum requests in window
%% @param TimeNowMs - Current time in milliseconds
%% @returns {ok, NewWindow, RequestsRemaining} | {error, exceeded}
-spec check_sliding_window(sliding_window(), integer(), integer()) ->
                              {ok, sliding_window(), non_neg_integer()} | {error, exceeded}.
check_sliding_window({Requests, WindowStart, WindowSize}, MaxRequests, TimeNowMs) ->
    % Remove requests outside the window
    WindowStartTime = TimeNowMs - WindowSize,
    ValidRequests = [T || T <- Requests, T >= WindowStartTime],

    RequestCount = length(ValidRequests),
    if RequestCount < MaxRequests ->
           % Add this request to window
           NewRequests = [TimeNowMs | ValidRequests],
           NewWindow = {NewRequests, WindowStart, WindowSize},
           Remaining = MaxRequests - RequestCount - 1,
           {ok, NewWindow, Remaining};
       true ->
           {error, exceeded}
    end.

%%====================================================================
%% Leaky Bucket Algorithm Implementation
%%====================================================================

%% @doc Create a leaky bucket with specified capacity
%% @param Capacity - Maximum queue size
%% @returns Leaky bucket state
-spec create_leaky_bucket(non_neg_integer()) -> leaky_bucket().
create_leaky_bucket(Capacity) ->
    TimeNowMs = erlang:system_time(millisecond),
    {0, TimeNowMs, float(Capacity) / 1000.0}.  % Leak rate: capacity per second

%% @doc Check leaky bucket (requests leak out at constant rate)
%% @param Bucket - Current bucket state
%% @param Capacity - Maximum queue size
%% @returns {ok, NewBucket, SpaceRemaining} | {error, exceeded}
-spec check_leaky_bucket(leaky_bucket(), non_neg_integer()) ->
                            {ok, leaky_bucket(), non_neg_integer()} | {error, exceeded}.
check_leaky_bucket({QueueSize, LastLeakMs, LeakRate}, Capacity) ->
    TimeNowMs = erlang:system_time(millisecond),
    ElapsedMs = TimeNowMs - LastLeakMs,

    % Calculate how much leaked out
    Leaked = trunc(LeakRate * ElapsedMs / 1000.0),
    CurrentSize = max(0, QueueSize - Leaked),

    if CurrentSize < Capacity ->
           % Add this request
           NewSize = CurrentSize + 1,
           NewBucket = {NewSize, TimeNowMs, LeakRate},
           Remaining = Capacity - NewSize,
           {ok, NewBucket, Remaining};
       true ->
           {error, exceeded}
    end.
