%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_rate_limiter - Authentication Rate Limiting
%%%
%%% Protects against brute force and dictionary attacks by enforcing:
%%% - Per-client rate limits (10 attempts/second)
%%% - IP-based blocking for repeated failures
%%% - Exponential backoff (1s, 2s, 4s, 8s, 16s)
%%% - Authentication failure logging for detection
%%%
%%% Design:
%%% - gen_server for state management
%%% - ETS for fast lookups (client_id -> {count, window_start})
%%% - Sliding window algorithm for rate limiting
%%% - Automatic cleanup of expired entries
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_rate_limiter).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    check_rate_limit/1,
    check_rate_limit/2,
    record_failure/1,
    record_failure/2,
    record_success/1,
    record_success/2,
    is_blocked/1,
    get_client_stats/1,
    reset_client/1,
    clear_all_blocks/0,
    get_blocked_clients/0,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type client_id() :: binary().
-type ip_address() :: inet:ip_address().
-type attempt_count() :: non_neg_integer().
-type window_start() :: integer(). %% milliseconds
-type backoff_level() :: 0..5. %% 0=none, 1=1s, 2=2s, 3=4s, 4=8s, 5=16s

-record(rate_limit_state, {
    count :: attempt_count(),
    window_start :: window_start()
}).

-type rate_limit_state() :: #rate_limit_state{}.

-record(block_state, {
    reason :: term(),
    blocked_until :: integer(), %% milliseconds
    failure_count :: non_neg_integer(),
    backoff_level :: backoff_level()
}).

-type block_state() :: #block_state{}.

-record(client_stats, {
    client_id :: client_id() | undefined,
    ip_address :: ip_address() | undefined,
    total_attempts :: non_neg_integer(),
    successful_auths :: non_neg_integer(),
    failed_auths :: non_neg_integer(),
    rate_limited_count :: non_neg_integer(),
    blocked_count :: non_neg_integer(),
    current_backoff_level :: backoff_level(),
    last_attempt_at :: integer() | undefined
}).

-type client_stats() :: #client_stats{}.

-record(state, {
    rate_limits :: ets:tid(),           % client_id -> rate_limit_state
    blocks :: ets:tid(),                % client_id -> block_state
    client_stats :: ets:tid(),          % client_id -> client_stats
    ip_blocks :: ets:tid(),             % ip_address -> block_state
    config :: map()
}).

-type state() :: #state{}.

-define(DEFAULT_MAX_ATTEMPTS_PER_SECOND, 10).
-define(DEFAULT_WINDOW_MS, 1000).
-define(DEFAULT_MAX_FAILURES, 5).
-define(DEFAULT_BLOCK_DURATION_MS, 300000). %% 5 minutes
-define(DEFAULT_BACKOFF_LEVELS, [0, 1000, 2000, 4000, 8000, 16000]).
-define(DEFAULT_CLEANUP_INTERVAL_MS, 60000). %% 1 minute

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start rate limiter with default config
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start rate limiter with custom config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Check rate limit for client (by client_id only)
-spec check_rate_limit(client_id()) -> ok | {error, rate_limited}.
check_rate_limit(ClientId) ->
    check_rate_limit(ClientId, undefined).

%% @doc Check rate limit for client (with IP address)
-spec check_rate_limit(client_id(), ip_address() | undefined) ->
    ok | {error, rate_limited} | {error, blocked, term()}.
check_rate_limit(ClientId, IpAddress) ->
    gen_server:call(?MODULE, {check_rate_limit, ClientId, IpAddress}).

%% @doc Record authentication failure (by client_id only)
-spec record_failure(client_id()) -> ok.
record_failure(ClientId) ->
    record_failure(ClientId, undefined).

%% @doc Record authentication failure (with IP address)
-spec record_failure(client_id(), ip_address() | undefined) -> ok.
record_failure(ClientId, IpAddress) ->
    gen_server:call(?MODULE, {record_failure, ClientId, IpAddress}).

%% @doc Record authentication success (by client_id only)
-spec record_success(client_id()) -> ok.
record_success(ClientId) ->
    record_success(ClientId, undefined).

%% @doc Record authentication success (with IP address)
-spec record_success(client_id(), ip_address() | undefined) -> ok.
record_success(ClientId, IpAddress) ->
    gen_server:call(?MODULE, {record_success, ClientId, IpAddress}).

%% @doc Check if client is currently blocked
-spec is_blocked(client_id()) -> boolean().
is_blocked(ClientId) ->
    gen_server:call(?MODULE, {is_blocked, ClientId}).

%% @doc Get statistics for a client
-spec get_client_stats(client_id()) -> {ok, client_stats()} | {error, not_found}.
get_client_stats(ClientId) ->
    gen_server:call(?MODULE, {get_client_stats, ClientId}).

%% @doc Reset rate limiting for a specific client (admin use)
-spec reset_client(client_id()) -> ok.
reset_client(ClientId) ->
    gen_server:call(?MODULE, {reset_client, ClientId}).

%% @doc Clear all blocks (admin use - emergency only)
-spec clear_all_blocks() -> ok.
clear_all_blocks() ->
    gen_server:call(?MODULE, clear_all_blocks).

%% @doc Get list of currently blocked clients
-spec get_blocked_clients() -> {ok, [client_id()]}.
get_blocked_clients() ->
    gen_server:call(?MODULE, get_blocked_clients).

%% @doc Stop rate limiter
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    State = #state{
        rate_limits = ets:new(auth_rate_limits, [set, protected]),
        blocks = ets:new(auth_blocks, [set, protected]),
        client_stats = ets:new(auth_client_stats, [set, protected]),
        ip_blocks = ets:new(auth_ip_blocks, [set, protected]),
        config = Config
    },

    % Start cleanup timer
    erlang:send_after(maps_get(cleanup_interval_ms, Config, ?DEFAULT_CLEANUP_INTERVAL_MS),
                      self(), cleanup_expired),

    logger:info("Auth rate limiter started: max ~p attempts/sec",
                [maps_get(max_attempts_per_second, Config, ?DEFAULT_MAX_ATTEMPTS_PER_SECOND)]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({check_rate_limit, ClientId, IpAddress}, _From, State) ->
    Result = do_check_rate_limit(ClientId, IpAddress, State),
    {reply, Result, State};

handle_call({record_failure, ClientId, IpAddress}, _From, State) ->
    ok = do_record_failure(ClientId, IpAddress, State),
    {reply, ok, State};

handle_call({record_success, ClientId, IpAddress}, _From, State) ->
    ok = do_record_success(ClientId, IpAddress, State),
    {reply, ok, State};

handle_call({is_blocked, ClientId}, _From, State) ->
    Blocked = do_is_blocked(ClientId, State),
    {reply, Blocked, State};

handle_call({get_client_stats, ClientId}, _From, State) ->
    Result = case ets:lookup(State#state.client_stats, ClientId) of
        [{_, Stats}] -> {ok, Stats};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({reset_client, ClientId}, _From, State) ->
    ets:delete(State#state.rate_limits, ClientId),
    ets:delete(State#state.blocks, ClientId),
    ets:delete(State#state.client_stats, ClientId),
    logger:warning("Rate limiter reset for client: ~p", [ClientId]),
    {reply, ok, State};

handle_call(clear_all_blocks, _From, State) ->
    ets:delete_all_objects(State#state.blocks),
    ets:delete_all_objects(State#state.ip_blocks),
    logger:warning("All rate limit blocks cleared (emergency action)"),
    {reply, ok, State};

handle_call(get_blocked_clients, _From, State) ->
    Blocked = [ClientId || {ClientId, _} <- ets:tab2list(State#state.blocks)],
    {reply, {ok, Blocked}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired, State) ->
    cleanup_expired_entries(State),
    CleanupInterval = maps_get(cleanup_interval_ms, State#state.config,
                                ?DEFAULT_CLEANUP_INTERVAL_MS),
    erlang:send_after(CleanupInterval, self(), cleanup_expired),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.rate_limits),
    ets:delete(State#state.blocks),
    ets:delete(State#state.client_stats),
    ets:delete(State#state.ip_blocks),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Check if client is rate limited
do_check_rate_limit(ClientId, IpAddress, State) ->
    Now = erlang:system_time(millisecond),

    % First check if blocked
    case do_is_blocked(ClientId, State) of
        true ->
            {error, blocked, client_id_blocked};
        false ->
            case IpAddress of
                undefined ->
                    check_rate_limit_window(ClientId, Now, State);
                Ip ->
                    case ets:lookup(State#state.ip_blocks, Ip) of
                        [{_, #block_state{blocked_until = BlockedUntil}}] when Now < BlockedUntil ->
                            RemainingTime = BlockedUntil - Now,
                            {error, blocked, {ip_blocked, RemainingTime}};
                        _ ->
                            check_rate_limit_window(ClientId, Now, State)
                    end
            end
    end.

%% @private Check rate limit window
check_rate_limit_window(ClientId, Now, State) ->
    MaxAttempts = maps_get(max_attempts_per_second, State#state.config,
                            ?DEFAULT_MAX_ATTEMPTS_PER_SECOND),
    WindowMs = maps_get(window_ms, State#state.config, ?DEFAULT_WINDOW_MS),

    case ets:lookup(State#state.rate_limits, ClientId) of
        [{_, #rate_limit_state{count = Count, window_start = Start}}] ->
            TimeSinceStart = Now - Start,
            if
                TimeSinceStart >= WindowMs ->
                    % Window expired, reset
                    ets:insert(State#state.rate_limits,
                              {ClientId, #rate_limit_state{count = 1, window_start = Now}}),
                    ok;
                Count >= MaxAttempts ->
                    % Rate limit exceeded
                    {error, rate_limited};
                true ->
                    % Within limit, increment count
                    NewCount = Count + 1,
                    ets:insert(State#state.rate_limits,
                              {ClientId, #rate_limit_state{count = NewCount, window_start = Start}}),
                    ok
            end;
        [] ->
            % First attempt in window
            ets:insert(State#state.rate_limits,
                      {ClientId, #rate_limit_state{count = 1, window_start = Now}}),
            ok
    end.

%% @private Check if client is blocked
do_is_blocked(ClientId, State) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(State#state.blocks, ClientId) of
        [{_, #block_state{blocked_until = BlockedUntil}}] when Now < BlockedUntil ->
            true;
        _ ->
            false
    end.

%% @private Record authentication failure
do_record_failure(ClientId, IpAddress, State) ->
    Now = erlang:system_time(millisecond),

    % Update or create client stats
    Stats = case ets:lookup(State#state.client_stats, ClientId) of
        [{_, ExistingStats}] ->
            ExistingStats#client_stats{
                failed_auths = ExistingStats#client_stats.failed_auths + 1,
                total_attempts = ExistingStats#client_stats.total_attempts + 1,
                last_attempt_at = Now
            };
        [] ->
            #client_stats{
                client_id = ClientId,
                ip_address = IpAddress,
                total_attempts = 1,
                successful_auths = 0,
                failed_auths = 1,
                rate_limited_count = 0,
                blocked_count = 0,
                current_backoff_level = 0,
                last_attempt_at = Now
            }
    end,
    ets:insert(State#state.client_stats, {ClientId, Stats}),

    % Check if we need to block the client
    MaxFailures = maps_get(max_failures, State#state.config, ?DEFAULT_MAX_FAILURES),
    BlockDuration = maps_get(block_duration_ms, State#state.config, ?DEFAULT_BLOCK_DURATION_MS),
    BackoffLevels = maps_get(backoff_levels, State#state.config, ?DEFAULT_BACKOFF_LEVELS),

    Failures = Stats#client_stats.failed_auths,
    CurrentBackoff = Stats#client_stats.current_backoff_level,

    if
        Failures >= MaxFailures andalso CurrentBackoff < length(BackoffLevels) - 1 ->
            % Increment backoff level
            NewBackoffLevel = CurrentBackoff + 1,
            BackoffDelay = lists:nth(NewBackoffLevel + 1, BackoffLevels),
            BlockedUntil = Now + BackoffDelay,

            ets:insert(State#state.blocks, {ClientId, #block_state{
                reason = too_many_failures,
                blocked_until = BlockedUntil,
                failure_count = Failures,
                backoff_level = NewBackoffLevel
            }}),

            % Update stats
            NewStats = Stats#client_stats{
                blocked_count = Stats#client_stats.blocked_count + 1,
                current_backoff_level = NewBackoffLevel
            },
            ets:insert(State#state.client_stats, {ClientId, NewStats}),

            % Log the block
            logger:warning("Client ~p blocked after ~p failures (backoff level ~p, ~pms)",
                          [ClientId, Failures, NewBackoffLevel, BackoffDelay]),

            % Also block IP if provided
            case IpAddress of
                undefined -> ok;
                Ip ->
                    ets:insert(State#state.ip_blocks, {Ip, #block_state{
                        reason = too_many_failures,
                        blocked_until = BlockedUntil,
                        failure_count = Failures,
                        backoff_level = NewBackoffLevel
                    }})
            end;

        Failures >= MaxFailures * 2 ->
            % Permanent block (5 minutes)
            BlockedUntil = Now + BlockDuration,

            ets:insert(State#state.blocks, {ClientId, #block_state{
                reason = excessive_failures,
                blocked_until = BlockedUntil,
                failure_count = Failures,
                backoff_level = 5
            }}),

            logger:error("Client ~p permanently blocked (~pms) after ~p failures",
                        [ClientId, BlockDuration, Failures]),

            % Also block IP if provided
            case IpAddress of
                undefined -> ok;
                Ip ->
                    ets:insert(State#state.ip_blocks, {Ip, #block_state{
                        reason = excessive_failures,
                        blocked_until = BlockedUntil,
                        failure_count = Failures,
                        backoff_level = 5
                    }})
            end;

        true ->
            ok
    end,

    ok.

%% @private Record authentication success
do_record_success(ClientId, IpAddress, State) ->
    Now = erlang:system_time(millisecond),

    % Update or create client stats
    Stats = case ets:lookup(State#state.client_stats, ClientId) of
        [{_, ExistingStats}] ->
            ExistingStats#client_stats{
                successful_auths = ExistingStats#client_stats.successful_auths + 1,
                total_attempts = ExistingStats#client_stats.total_attempts + 1,
                current_backoff_level = 0,  % Reset backoff on success
                last_attempt_at = Now
            };
        [] ->
            #client_stats{
                client_id = ClientId,
                ip_address = IpAddress,
                total_attempts = 1,
                successful_auths = 1,
                failed_auths = 0,
                rate_limited_count = 0,
                blocked_count = 0,
                current_backoff_level = 0,
                last_attempt_at = Now
            }
    end,
    ets:insert(State#state.client_stats, {ClientId, Stats}),

    % Clear any blocks on successful auth
    ets:delete(State#state.blocks, ClientId),
    case IpAddress of
        undefined -> ok;
        Ip -> ets:delete(State#state.ip_blocks, Ip)
    end,

    ok.

%% @private Cleanup expired entries
cleanup_expired_entries(State) ->
    Now = erlang:system_time(millisecond),

    % Cleanup expired blocks
    ets:foldl(fun({ClientId, #block_state{blocked_until = BlockedUntil}}, Acc) ->
        case BlockedUntil < Now of
            true ->
                ets:delete(State#state.blocks, ClientId),
                logger:info("Block expired for client: ~p", [ClientId]);
            false ->
                ok
        end,
        Acc
    end, ok, State#state.blocks),

    % Cleanup expired IP blocks
    ets:foldl(fun({IpAddress, #block_state{blocked_until = BlockedUntil}}, Acc) ->
        case BlockedUntil < Now of
            true ->
                ets:delete(State#state.ip_blocks, IpAddress),
                logger:info("IP block expired: ~p", [IpAddress]);
            false ->
                ok
        end,
        Acc
    end, ok, State#state.ip_blocks),

    % Cleanup old rate limit states (older than 1 hour)
    HourAgo = Now - (3600 * 1000),
    ets:foldl(fun({ClientId, #rate_limit_state{window_start = WindowStart}}, Acc) ->
        case WindowStart < HourAgo of
            true ->
                ets:delete(State#state.rate_limits, ClientId);
            false ->
                ok
        end,
        Acc
    end, ok, State#state.rate_limits),

    ok.

%% @private Safe maps:get with default
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
