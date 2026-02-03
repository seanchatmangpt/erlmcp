%%%-------------------------------------------------------------------
%%% @doc A2A Push Notification Module
%%%
%%% This module handles A2A push notifications for task updates.
%%% It provides HTTP webhook delivery with retry logic, authentication
%%% header injection, token verification, and rate limiting.
%%%
%%% Features:
%%% - Push configuration CRUD operations
%%% - HTTP webhook delivery with exponential backoff retry
%%% - Bearer and Basic authentication schemes
%%% - Token-based notification verification
%%% - Per-URL rate limiting
%%% - Delivery status tracking
%%% - Timeout handling
%%% - Failure notification callbacks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_push).

-behaviour(gen_server).

-include("erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Configuration CRUD
    create_config/3,
    get_config/2,
    list_configs/1,
    delete_config/2,

    %% Notification operations
    notify/2,
    verify_token/2,

    %% Status and metrics
    get_delivery_status/2,
    get_rate_limit_status/1,
    get_stats/0,

    %% Admin operations
    clear_rate_limits/0,
    reset_delivery_stats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Types
%%====================================================================

-type task_id() :: binary().
-type config_id() :: binary().
-type url() :: binary().
-type token() :: binary().

-type delivery_result() :: ok | {error, term()}.
-type delivery_status() :: pending | in_progress | delivered | failed | rate_limited.

-record(rate_limit_state, {
    count :: non_neg_integer(),
    window_start :: integer(),  % milliseconds
    blocked_until :: integer() | undefined
}).

-record(delivery_record, {
    task_id :: task_id(),
    config_id :: config_id(),
    status :: delivery_status(),
    attempts :: non_neg_integer(),
    last_attempt :: integer() | undefined,
    last_error :: term() | undefined,
    delivered_at :: integer() | undefined
}).

-record(state, {
    %% Push configs: task_id => #{config_id => #a2a_task_push_notification_config{}}
    configs :: ets:tid(),

    %% Rate limits per URL: url => #rate_limit_state{}
    rate_limits :: ets:tid(),

    %% Delivery status: {task_id, config_id} => #delivery_record{}
    delivery_status :: ets:tid(),

    %% Pending retries: reference() => retry_info
    pending_retries :: ets:tid(),

    %% Verification tokens: token => {task_id, config_id, expiry}
    tokens :: ets:tid(),

    %% Configuration
    config :: map(),

    %% Failure callback (optional)
    failure_callback :: fun((task_id(), config_id(), term()) -> ok) | undefined
}).

%%====================================================================
%% Configuration Defaults
%%====================================================================

-define(DEFAULT_MAX_RETRIES, 5).
-define(DEFAULT_INITIAL_BACKOFF_MS, 1000).
-define(DEFAULT_MAX_BACKOFF_MS, 60000).
-define(DEFAULT_TIMEOUT_MS, 30000).
-define(DEFAULT_RATE_LIMIT_PER_MINUTE, 60).
-define(DEFAULT_RATE_LIMIT_WINDOW_MS, 60000).
-define(DEFAULT_TOKEN_EXPIRY_MS, 3600000). % 1 hour
-define(DEFAULT_CLEANUP_INTERVAL_MS, 300000). % 5 minutes

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the push notification server with default config
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the push notification server with custom config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Stop the push notification server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Create a push notification config for a task
-spec create_config(task_id(), config_id(), #a2a_push_notification_config{}) ->
    {ok, #a2a_task_push_notification_config{}} | {error, term()}.
create_config(TaskId, ConfigId, #a2a_push_notification_config{} = PushConfig) ->
    gen_server:call(?MODULE, {create_config, TaskId, ConfigId, PushConfig}).

%% @doc Get a push notification config by task_id and config_id
-spec get_config(task_id(), config_id()) ->
    {ok, #a2a_task_push_notification_config{}} | {error, not_found}.
get_config(TaskId, ConfigId) ->
    gen_server:call(?MODULE, {get_config, TaskId, ConfigId}).

%% @doc List all push notification configs for a task
-spec list_configs(task_id()) -> {ok, [#a2a_task_push_notification_config{}]}.
list_configs(TaskId) ->
    gen_server:call(?MODULE, {list_configs, TaskId}).

%% @doc Delete a push notification config
-spec delete_config(task_id(), config_id()) -> ok | {error, not_found}.
delete_config(TaskId, ConfigId) ->
    gen_server:call(?MODULE, {delete_config, TaskId, ConfigId}).

%% @doc Send a push notification for a task update
-spec notify(task_id(), map()) -> {ok, [delivery_result()]} | {error, term()}.
notify(TaskId, Payload) ->
    gen_server:call(?MODULE, {notify, TaskId, Payload}).

%% @doc Verify a notification token
-spec verify_token(task_id(), token()) -> {ok, config_id()} | {error, term()}.
verify_token(TaskId, Token) ->
    gen_server:call(?MODULE, {verify_token, TaskId, Token}).

%% @doc Get delivery status for a specific config
-spec get_delivery_status(task_id(), config_id()) ->
    {ok, #delivery_record{}} | {error, not_found}.
get_delivery_status(TaskId, ConfigId) ->
    gen_server:call(?MODULE, {get_delivery_status, TaskId, ConfigId}).

%% @doc Get rate limit status for a URL
-spec get_rate_limit_status(url()) -> {ok, map()} | {error, not_found}.
get_rate_limit_status(Url) ->
    gen_server:call(?MODULE, {get_rate_limit_status, Url}).

%% @doc Get overall stats
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Clear all rate limits (admin operation)
-spec clear_rate_limits() -> ok.
clear_rate_limits() ->
    gen_server:call(?MODULE, clear_rate_limits).

%% @doc Reset delivery statistics (admin operation)
-spec reset_delivery_stats() -> ok.
reset_delivery_stats() ->
    gen_server:call(?MODULE, reset_delivery_stats).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([map()]) -> {ok, #state{}}.
init([Config]) ->
    process_flag(trap_exit, true),

    State = #state{
        configs = ets:new(a2a_push_configs, [set, protected]),
        rate_limits = ets:new(a2a_push_rate_limits, [set, protected]),
        delivery_status = ets:new(a2a_push_delivery_status, [set, protected]),
        pending_retries = ets:new(a2a_push_pending_retries, [set, protected]),
        tokens = ets:new(a2a_push_tokens, [set, protected]),
        config = Config,
        failure_callback = maps:get(failure_callback, Config, undefined)
    },

    %% Start cleanup timer
    CleanupInterval = maps_get(cleanup_interval_ms, Config, ?DEFAULT_CLEANUP_INTERVAL_MS),
    erlang:send_after(CleanupInterval, self(), cleanup),

    logger:info("A2A push notification server started"),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({create_config, TaskId, ConfigId, PushConfig}, _From, State) ->
    Result = do_create_config(TaskId, ConfigId, PushConfig, State),
    {reply, Result, State};

handle_call({get_config, TaskId, ConfigId}, _From, State) ->
    Result = do_get_config(TaskId, ConfigId, State),
    {reply, Result, State};

handle_call({list_configs, TaskId}, _From, State) ->
    Result = do_list_configs(TaskId, State),
    {reply, Result, State};

handle_call({delete_config, TaskId, ConfigId}, _From, State) ->
    Result = do_delete_config(TaskId, ConfigId, State),
    {reply, Result, State};

handle_call({notify, TaskId, Payload}, _From, State) ->
    Result = do_notify(TaskId, Payload, State),
    {reply, Result, State};

handle_call({verify_token, TaskId, Token}, _From, State) ->
    Result = do_verify_token(TaskId, Token, State),
    {reply, Result, State};

handle_call({get_delivery_status, TaskId, ConfigId}, _From, State) ->
    Result = case ets:lookup(State#state.delivery_status, {TaskId, ConfigId}) of
        [{_, Record}] -> {ok, Record};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({get_rate_limit_status, Url}, _From, State) ->
    Result = case ets:lookup(State#state.rate_limits, Url) of
        [{_, #rate_limit_state{count = Count, window_start = Start, blocked_until = Blocked}}] ->
            Now = erlang:system_time(millisecond),
            WindowMs = maps_get(rate_limit_window_ms, State#state.config, ?DEFAULT_RATE_LIMIT_WINDOW_MS),
            Limit = maps_get(rate_limit_per_minute, State#state.config, ?DEFAULT_RATE_LIMIT_PER_MINUTE),
            {ok, #{
                count => Count,
                limit => Limit,
                window_remaining_ms => max(0, WindowMs - (Now - Start)),
                is_blocked => Blocked =/= undefined andalso Now < Blocked
            }};
        [] ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        total_configs => ets:info(State#state.configs, size),
        active_rate_limits => ets:info(State#state.rate_limits, size),
        delivery_records => ets:info(State#state.delivery_status, size),
        pending_retries => ets:info(State#state.pending_retries, size),
        active_tokens => ets:info(State#state.tokens, size)
    },
    {reply, {ok, Stats}, State};

handle_call(clear_rate_limits, _From, State) ->
    ets:delete_all_objects(State#state.rate_limits),
    logger:warning("All rate limits cleared (admin action)"),
    {reply, ok, State};

handle_call(reset_delivery_stats, _From, State) ->
    ets:delete_all_objects(State#state.delivery_status),
    logger:warning("Delivery statistics reset (admin action)"),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({retry_delivery, TaskId, ConfigId, Payload, Attempt}, State) ->
    do_retry_delivery(TaskId, ConfigId, Payload, Attempt, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({retry, Ref, TaskId, ConfigId, Payload, Attempt}, State) ->
    ets:delete(State#state.pending_retries, Ref),
    do_retry_delivery(TaskId, ConfigId, Payload, Attempt, State),
    {noreply, State};

handle_info(cleanup, State) ->
    cleanup_expired_entries(State),
    CleanupInterval = maps_get(cleanup_interval_ms, State#state.config, ?DEFAULT_CLEANUP_INTERVAL_MS),
    erlang:send_after(CleanupInterval, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.configs),
    ets:delete(State#state.rate_limits),
    ets:delete(State#state.delivery_status),
    ets:delete(State#state.pending_retries),
    ets:delete(State#state.tokens),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Configuration CRUD
%%====================================================================

%% @private Create a push notification config
do_create_config(TaskId, ConfigId, PushConfig, State) ->
    %% Validate URL
    case validate_url(PushConfig#a2a_push_notification_config.url) of
        ok ->
            %% Generate token if not provided
            Token = case PushConfig#a2a_push_notification_config.token of
                undefined -> generate_token();
                T -> T
            end,

            UpdatedPushConfig = PushConfig#a2a_push_notification_config{
                id = ConfigId,
                token = Token
            },

            TaskPushConfig = #a2a_task_push_notification_config{
                id = ConfigId,
                task_id = TaskId,
                push_notification_config = UpdatedPushConfig
            },

            %% Store config
            Key = {TaskId, ConfigId},
            ets:insert(State#state.configs, {Key, TaskPushConfig}),

            %% Store token mapping
            TokenExpiry = erlang:system_time(millisecond) +
                maps_get(token_expiry_ms, State#state.config, ?DEFAULT_TOKEN_EXPIRY_MS),
            ets:insert(State#state.tokens, {Token, {TaskId, ConfigId, TokenExpiry}}),

            logger:info("Push config created: task_id=~s, config_id=~s", [TaskId, ConfigId]),
            {ok, TaskPushConfig};
        {error, Reason} ->
            {error, {invalid_url, Reason}}
    end.

%% @private Get a push notification config
do_get_config(TaskId, ConfigId, State) ->
    case ets:lookup(State#state.configs, {TaskId, ConfigId}) of
        [{_, Config}] -> {ok, Config};
        [] -> {error, not_found}
    end.

%% @private List all configs for a task
do_list_configs(TaskId, State) ->
    Configs = ets:foldl(
        fun({{T, _}, Config}, Acc) when T =:= TaskId ->
            [Config | Acc];
           (_, Acc) ->
            Acc
        end,
        [],
        State#state.configs
    ),
    {ok, Configs}.

%% @private Delete a push notification config
do_delete_config(TaskId, ConfigId, State) ->
    Key = {TaskId, ConfigId},
    case ets:lookup(State#state.configs, Key) of
        [{_, #a2a_task_push_notification_config{
            push_notification_config = #a2a_push_notification_config{token = Token}
        }}] ->
            ets:delete(State#state.configs, Key),
            ets:delete(State#state.tokens, Token),
            ets:delete(State#state.delivery_status, Key),
            logger:info("Push config deleted: task_id=~s, config_id=~s", [TaskId, ConfigId]),
            ok;
        [] ->
            {error, not_found}
    end.

%%====================================================================
%% Internal Functions - Notification Delivery
%%====================================================================

%% @private Send notifications to all configs for a task
do_notify(TaskId, Payload, State) ->
    case do_list_configs(TaskId, State) of
        {ok, []} ->
            {ok, []};
        {ok, Configs} ->
            Results = lists:map(
                fun(#a2a_task_push_notification_config{
                    id = ConfigId,
                    push_notification_config = PushConfig
                }) ->
                    deliver_notification(TaskId, ConfigId, PushConfig, Payload, State)
                end,
                Configs
            ),
            {ok, Results}
    end.

%% @private Deliver a single notification
deliver_notification(TaskId, ConfigId, PushConfig, Payload, State) ->
    Url = PushConfig#a2a_push_notification_config.url,

    %% Check rate limit
    case check_rate_limit(Url, State) of
        ok ->
            %% Update delivery status
            update_delivery_status(TaskId, ConfigId, in_progress, State),

            %% Build request
            Headers = build_headers(PushConfig),
            Body = encode_payload(TaskId, PushConfig, Payload),
            Timeout = maps_get(timeout_ms, State#state.config, ?DEFAULT_TIMEOUT_MS),

            %% Send HTTP request
            case send_http_request(Url, Headers, Body, Timeout) of
                {ok, _Response} ->
                    update_delivery_status(TaskId, ConfigId, delivered, State),
                    record_rate_limit(Url, State),
                    ok;
                {error, Reason} ->
                    handle_delivery_failure(TaskId, ConfigId, PushConfig, Payload, Reason, 1, State),
                    {error, Reason}
            end;
        {error, rate_limited} ->
            update_delivery_status(TaskId, ConfigId, rate_limited, State),
            {error, rate_limited}
    end.

%% @private Handle delivery failure with retry logic
handle_delivery_failure(TaskId, ConfigId, PushConfig, Payload, Reason, Attempt, State) ->
    MaxRetries = maps_get(max_retries, State#state.config, ?DEFAULT_MAX_RETRIES),

    if
        Attempt < MaxRetries ->
            %% Schedule retry with exponential backoff
            InitialBackoff = maps_get(initial_backoff_ms, State#state.config, ?DEFAULT_INITIAL_BACKOFF_MS),
            MaxBackoff = maps_get(max_backoff_ms, State#state.config, ?DEFAULT_MAX_BACKOFF_MS),
            BackoffMs = min(InitialBackoff * trunc(math:pow(2, Attempt - 1)), MaxBackoff),

            %% Add jitter (10%)
            Jitter = rand:uniform(BackoffMs div 10),
            DelayMs = BackoffMs + Jitter,

            Ref = make_ref(),
            ets:insert(State#state.pending_retries, {Ref, {TaskId, ConfigId, Attempt}}),
            erlang:send_after(DelayMs, self(), {retry, Ref, TaskId, ConfigId, Payload, Attempt + 1}),

            update_delivery_status(TaskId, ConfigId, pending, Reason, State),
            logger:warning("Push notification retry scheduled: task_id=~s, config_id=~s, "
                          "attempt=~p, delay_ms=~p",
                          [TaskId, ConfigId, Attempt, DelayMs]);
        true ->
            %% Max retries exceeded
            update_delivery_status(TaskId, ConfigId, failed, Reason, State),
            notify_failure(TaskId, ConfigId, Reason, State),
            logger:error("Push notification failed permanently: task_id=~s, config_id=~s, "
                        "attempts=~p, reason=~p",
                        [TaskId, ConfigId, Attempt, Reason])
    end.

%% @private Retry delivery
do_retry_delivery(TaskId, ConfigId, Payload, Attempt, State) ->
    case do_get_config(TaskId, ConfigId, State) of
        {ok, #a2a_task_push_notification_config{push_notification_config = PushConfig}} ->
            Url = PushConfig#a2a_push_notification_config.url,

            case check_rate_limit(Url, State) of
                ok ->
                    update_delivery_status(TaskId, ConfigId, in_progress, State),

                    Headers = build_headers(PushConfig),
                    Body = encode_payload(TaskId, PushConfig, Payload),
                    Timeout = maps_get(timeout_ms, State#state.config, ?DEFAULT_TIMEOUT_MS),

                    case send_http_request(Url, Headers, Body, Timeout) of
                        {ok, _Response} ->
                            update_delivery_status(TaskId, ConfigId, delivered, State),
                            record_rate_limit(Url, State),
                            logger:info("Push notification delivered on retry: task_id=~s, "
                                       "config_id=~s, attempt=~p",
                                       [TaskId, ConfigId, Attempt]);
                        {error, Reason} ->
                            handle_delivery_failure(TaskId, ConfigId, PushConfig, Payload,
                                                   Reason, Attempt, State)
                    end;
                {error, rate_limited} ->
                    %% Reschedule with rate limit backoff
                    DelayMs = 5000,
                    Ref = make_ref(),
                    ets:insert(State#state.pending_retries, {Ref, {TaskId, ConfigId, Attempt}}),
                    erlang:send_after(DelayMs, self(), {retry, Ref, TaskId, ConfigId, Payload, Attempt})
            end;
        {error, not_found} ->
            logger:warning("Config not found for retry: task_id=~s, config_id=~s",
                          [TaskId, ConfigId])
    end.

%%====================================================================
%% Internal Functions - HTTP Request
%%====================================================================

%% @private Send HTTP POST request
send_http_request(Url, Headers, Body, Timeout) ->
    %% Use httpc for HTTP requests (part of inets application)
    Request = {binary_to_list(Url), Headers, "application/json", Body},
    HttpOptions = [{timeout, Timeout}, {connect_timeout, Timeout div 2}],
    Options = [{body_format, binary}],

    case httpc:request(post, Request, HttpOptions, Options) of
        {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} when StatusCode >= 200, StatusCode < 300 ->
            {ok, #{status => StatusCode, body => RespBody}};
        {ok, {{_, StatusCode, Reason}, _RespHeaders, RespBody}} ->
            {error, {http_error, StatusCode, Reason, RespBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% @private Build HTTP headers with authentication
build_headers(#a2a_push_notification_config{authentication = undefined, token = Token}) ->
    BaseHeaders = [{"Content-Type", "application/json"}],
    case Token of
        undefined -> BaseHeaders;
        T -> [{"X-A2A-Token", binary_to_list(T)} | BaseHeaders]
    end;
build_headers(#a2a_push_notification_config{
    authentication = #a2a_authentication_info{scheme = Scheme, credentials = Creds},
    token = Token
}) ->
    BaseHeaders = [{"Content-Type", "application/json"}],

    WithToken = case Token of
        undefined -> BaseHeaders;
        T -> [{"X-A2A-Token", binary_to_list(T)} | BaseHeaders]
    end,

    %% Add Authorization header based on scheme
    case Scheme of
        <<"Bearer">> when Creds =/= undefined ->
            AuthHeader = {"Authorization", "Bearer " ++ binary_to_list(Creds)},
            [AuthHeader | WithToken];
        <<"Basic">> when Creds =/= undefined ->
            %% Credentials should be base64 encoded username:password
            AuthHeader = {"Authorization", "Basic " ++ binary_to_list(Creds)},
            [AuthHeader | WithToken];
        _ ->
            WithToken
    end.

%% @private Encode notification payload
encode_payload(TaskId, #a2a_push_notification_config{token = Token}, Payload) ->
    FullPayload = #{
        <<"taskId">> => TaskId,
        <<"token">> => Token,
        <<"timestamp">> => iso8601_timestamp(),
        <<"data">> => Payload
    },
    erlmcp_json:encode(FullPayload).

%%====================================================================
%% Internal Functions - Rate Limiting
%%====================================================================

%% @private Check if URL is rate limited
check_rate_limit(Url, State) ->
    Now = erlang:system_time(millisecond),
    WindowMs = maps_get(rate_limit_window_ms, State#state.config, ?DEFAULT_RATE_LIMIT_WINDOW_MS),
    Limit = maps_get(rate_limit_per_minute, State#state.config, ?DEFAULT_RATE_LIMIT_PER_MINUTE),

    case ets:lookup(State#state.rate_limits, Url) of
        [{_, #rate_limit_state{blocked_until = BlockedUntil}}] when BlockedUntil =/= undefined,
                                                                    Now < BlockedUntil ->
            {error, rate_limited};
        [{_, #rate_limit_state{count = Count, window_start = Start} = RateState}] ->
            if
                Now - Start >= WindowMs ->
                    %% Window expired, reset
                    ets:insert(State#state.rate_limits, {Url, #rate_limit_state{
                        count = 0,
                        window_start = Now,
                        blocked_until = undefined
                    }}),
                    ok;
                Count >= Limit ->
                    %% Rate limit exceeded, block for remainder of window
                    BlockedUntil = Start + WindowMs,
                    ets:insert(State#state.rate_limits, {Url, RateState#rate_limit_state{
                        blocked_until = BlockedUntil
                    }}),
                    logger:warning("Rate limit exceeded for URL: ~s", [Url]),
                    {error, rate_limited};
                true ->
                    ok
            end;
        [] ->
            %% First request to this URL
            ets:insert(State#state.rate_limits, {Url, #rate_limit_state{
                count = 0,
                window_start = Now,
                blocked_until = undefined
            }}),
            ok
    end.

%% @private Record a request for rate limiting
record_rate_limit(Url, State) ->
    case ets:lookup(State#state.rate_limits, Url) of
        [{_, #rate_limit_state{count = Count} = RateState}] ->
            ets:insert(State#state.rate_limits, {Url, RateState#rate_limit_state{
                count = Count + 1
            }});
        [] ->
            Now = erlang:system_time(millisecond),
            ets:insert(State#state.rate_limits, {Url, #rate_limit_state{
                count = 1,
                window_start = Now,
                blocked_until = undefined
            }})
    end.

%%====================================================================
%% Internal Functions - Token Verification
%%====================================================================

%% @private Verify a notification token
do_verify_token(TaskId, Token, State) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(State#state.tokens, Token) of
        [{_, {StoredTaskId, ConfigId, Expiry}}] when StoredTaskId =:= TaskId, Now < Expiry ->
            {ok, ConfigId};
        [{_, {StoredTaskId, _ConfigId, _Expiry}}] when StoredTaskId =/= TaskId ->
            {error, task_id_mismatch};
        [{_, {_StoredTaskId, _ConfigId, Expiry}}] when Now >= Expiry ->
            {error, token_expired};
        [] ->
            {error, invalid_token}
    end.

%% @private Generate a secure token
generate_token() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64:encode(Bytes).

%%====================================================================
%% Internal Functions - Delivery Status
%%====================================================================

%% @private Update delivery status
update_delivery_status(TaskId, ConfigId, Status, State) ->
    update_delivery_status(TaskId, ConfigId, Status, undefined, State).

update_delivery_status(TaskId, ConfigId, Status, Error, State) ->
    Now = erlang:system_time(millisecond),
    Key = {TaskId, ConfigId},

    Record = case ets:lookup(State#state.delivery_status, Key) of
        [{_, ExistingRecord}] ->
            ExistingRecord#delivery_record{
                status = Status,
                attempts = ExistingRecord#delivery_record.attempts + 1,
                last_attempt = Now,
                last_error = Error,
                delivered_at = case Status of
                    delivered -> Now;
                    _ -> ExistingRecord#delivery_record.delivered_at
                end
            };
        [] ->
            #delivery_record{
                task_id = TaskId,
                config_id = ConfigId,
                status = Status,
                attempts = 1,
                last_attempt = Now,
                last_error = Error,
                delivered_at = case Status of
                    delivered -> Now;
                    _ -> undefined
                end
            }
    end,

    ets:insert(State#state.delivery_status, {Key, Record}).

%%====================================================================
%% Internal Functions - Failure Notification
%%====================================================================

%% @private Notify about permanent delivery failure
notify_failure(TaskId, ConfigId, Reason, State) ->
    case State#state.failure_callback of
        undefined ->
            ok;
        Callback when is_function(Callback, 3) ->
            try
                Callback(TaskId, ConfigId, Reason)
            catch
                _:CallbackError ->
                    logger:error("Failure callback error: ~p", [CallbackError])
            end
    end.

%%====================================================================
%% Internal Functions - Cleanup
%%====================================================================

%% @private Cleanup expired entries
cleanup_expired_entries(State) ->
    Now = erlang:system_time(millisecond),

    %% Cleanup expired tokens
    ets:foldl(
        fun({Token, {_TaskId, _ConfigId, Expiry}}, Acc) when Now >= Expiry ->
            ets:delete(State#state.tokens, Token),
            Acc + 1;
           (_, Acc) ->
            Acc
        end,
        0,
        State#state.tokens
    ),

    %% Cleanup old rate limit entries (older than 1 hour)
    HourAgo = Now - 3600000,
    ets:foldl(
        fun({Url, #rate_limit_state{window_start = Start}}, Acc) when Start < HourAgo ->
            ets:delete(State#state.rate_limits, Url),
            Acc + 1;
           (_, Acc) ->
            Acc
        end,
        0,
        State#state.rate_limits
    ),

    ok.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Validate URL format
validate_url(Url) when is_binary(Url) ->
    UrlStr = binary_to_list(Url),
    case uri_string:parse(UrlStr) of
        #{scheme := Scheme} when Scheme =:= "https"; Scheme =:= "http" ->
            ok;
        #{scheme := _Scheme} ->
            {error, unsupported_scheme};
        _ ->
            {error, invalid_format}
    end;
validate_url(_) ->
    {error, not_binary}.

%% @private Generate ISO8601 timestamp
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).

%% @private Safe maps:get with default
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
