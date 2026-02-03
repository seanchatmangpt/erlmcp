%%%-------------------------------------------------------------------
%%% @doc A2A (Agent-to-Agent) Protocol Client
%%%
%%% This module implements the client side of the A2A protocol for
%%% connecting to remote A2A agents. It supports:
%%% - Agent discovery via agent card URL
%%% - Connection to agent interfaces (JSON-RPC, gRPC, HTTP+JSON)
%%% - Request/response handling with message correlation
%%% - Streaming message support via SSE or WebSocket
%%% - Task subscription and push notification configuration
%%% - Authentication (OAuth2, API key, HTTP auth, mTLS)
%%% - Retry logic with exponential backoff
%%%
%%% Uses gen_statem for connection state management:
%%% - disconnected -> connecting -> connected -> authenticated
%%% - Handles reconnection on failures
%%%
%%% Protocol Specification: https://github.com/google/a2a-spec
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_client).

-behaviour(gen_statem).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/1,
    start_link/2,
    stop/1,
    %% Connection management
    connect/1,
    connect/2,
    disconnect/1,
    get_status/1,
    %% Agent card operations
    get_agent_card/1,
    get_extended_agent_card/1,
    %% Message operations
    send_message/2,
    send_message/3,
    send_streaming_message/3,
    %% Task operations
    get_task/2,
    get_task/3,
    list_tasks/2,
    list_tasks/3,
    cancel_task/2,
    cancel_task/3,
    %% Subscription operations
    subscribe/2,
    subscribe/3,
    unsubscribe/2,
    %% Push notification configuration
    create_push_config/3,
    list_push_configs/2]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4,
    %% State functions
    disconnected/3,
    connecting/3,
    connected/3,
    authenticated/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records and Types
%%====================================================================

-record(state, {
    %% Connection configuration
    agent_url :: binary(),
    agent_card :: #a2a_agent_card{} | undefined,
    active_interface :: #a2a_agent_interface{} | undefined,

    %% HTTP client (Gun)
    gun_pid :: pid() | undefined,
    gun_monitor :: reference() | undefined,
    host :: inet:hostname() | undefined,
    port :: inet:port_number() | undefined,
    transport :: tcp | ssl,

    %% Authentication
    auth_config :: auth_config() | undefined,
    auth_token :: binary() | undefined,
    auth_expires :: integer() | undefined,

    %% Request tracking
    request_id = 1 :: pos_integer(),
    pending_requests = #{} :: #{integer() => pending_request()},

    %% Streaming and subscriptions
    stream_handlers = #{} :: #{binary() => stream_handler()},
    active_subscriptions = #{} :: #{binary() => subscription_info()},

    %% Retry configuration
    retry_config :: retry_config(),
    retry_count = 0 :: non_neg_integer(),
    retry_timer :: reference() | undefined,

    %% Options
    timeout :: timeout(),
    tenant :: binary() | undefined,

    %% Metrics
    metrics :: client_metrics()
}).

-record(pending_request, {
    id :: integer(),
    method :: binary(),
    from :: gen_statem:from(),
    start_time :: integer(),
    stream_ref :: gun:stream_ref() | undefined,
    timeout_ref :: reference() | undefined
}).

-record(stream_handler, {
    task_id :: binary(),
    callback :: fun((term()) -> ok),
    buffer = <<>> :: binary(),
    stream_ref :: gun:stream_ref() | undefined
}).

-record(subscription_info, {
    task_id :: binary(),
    callback :: fun((term()) -> ok),
    stream_ref :: gun:stream_ref() | undefined
}).

-record(retry_config, {
    max_retries = 5 :: non_neg_integer(),
    base_delay_ms = 1000 :: pos_integer(),
    max_delay_ms = 30000 :: pos_integer(),
    jitter = true :: boolean()
}).

-record(client_metrics, {
    requests_sent = 0 :: non_neg_integer(),
    responses_received = 0 :: non_neg_integer(),
    errors = 0 :: non_neg_integer(),
    reconnections = 0 :: non_neg_integer(),
    avg_latency_us = 0.0 :: float()
}).

-type pending_request() :: #pending_request{}.
-type stream_handler() :: #stream_handler{}.
-type subscription_info() :: #subscription_info{}.
-type retry_config() :: #retry_config{}.
-type client_metrics() :: #client_metrics{}.

-type auth_config() :: #{
    type := api_key | http_auth | oauth2 | openid_connect | mtls,
    %% For API key
    api_key => binary(),
    api_key_name => binary(),
    api_key_location => header | query | cookie,
    %% For HTTP auth
    scheme => binary(),
    credentials => binary(),
    %% For OAuth2
    client_id => binary(),
    client_secret => binary(),
    token_url => binary(),
    scopes => [binary()],
    %% For mTLS
    cert_file => binary(),
    key_file => binary(),
    ca_file => binary()
}.

-type client_opts() :: #{
    agent_url := binary(),
    auth => auth_config(),
    tenant => binary(),
    timeout => timeout(),
    retry => retry_config() | map(),
    ssl_options => [ssl:tls_client_option()]
}.

-type stream_callback() :: fun((
    {status_update, #a2a_task_status_update_event{}} |
    {artifact_update, #a2a_task_artifact_update_event{}} |
    {error, term()} |
    complete
) -> ok).

-export_type([client_opts/0, auth_config/0, stream_callback/0]).

%% Timeouts and defaults
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_CONNECT_TIMEOUT, 10000).
-define(GUN_CONNECT_TIMEOUT, 5000).
-define(AUTH_REFRESH_MARGIN_MS, 60000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the A2A client with options
-spec start_link(client_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_statem:start_link(?MODULE, Opts, []).

%% @doc Start a named A2A client
-spec start_link(atom(), client_opts()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) when is_atom(Name), is_map(Opts) ->
    gen_statem:start_link({local, Name}, ?MODULE, Opts, []).

%% @doc Stop the A2A client
-spec stop(pid() | atom()) -> ok.
stop(Client) ->
    gen_statem:stop(Client).

%% @doc Connect to agent by URL (fetches agent card and establishes connection)
-spec connect(pid() | atom()) -> ok | {error, term()}.
connect(Client) ->
    connect(Client, ?DEFAULT_CONNECT_TIMEOUT).

-spec connect(pid() | atom(), timeout()) -> ok | {error, term()}.
connect(Client, Timeout) ->
    gen_statem:call(Client, connect, Timeout).

%% @doc Disconnect from agent
-spec disconnect(pid() | atom()) -> ok.
disconnect(Client) ->
    gen_statem:call(Client, disconnect, 5000).

%% @doc Get current client status
-spec get_status(pid() | atom()) -> {ok, map()} | {error, term()}.
get_status(Client) ->
    gen_statem:call(Client, get_status, 5000).

%% @doc Fetch remote agent card from the agent URL
-spec get_agent_card(pid() | atom()) -> {ok, #a2a_agent_card{}} | {error, term()}.
get_agent_card(Client) ->
    gen_statem:call(Client, get_agent_card, ?DEFAULT_TIMEOUT).

%% @doc Fetch extended agent card (requires authentication)
-spec get_extended_agent_card(pid() | atom()) ->
    {ok, #a2a_agent_card{}} | {error, term()}.
get_extended_agent_card(Client) ->
    gen_statem:call(Client, get_extended_agent_card, ?DEFAULT_TIMEOUT).

%% @doc Send a message to the agent
-spec send_message(pid() | atom(), #a2a_message{}) ->
    {ok, #a2a_send_message_response{}} | {error, term()}.
send_message(Client, Message) ->
    send_message(Client, Message, #{}).

-spec send_message(pid() | atom(), #a2a_message{}, map()) ->
    {ok, #a2a_send_message_response{}} | {error, term()}.
send_message(Client, #a2a_message{} = Message, Opts) ->
    gen_statem:call(Client, {send_message, Message, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Send message with streaming response
-spec send_streaming_message(pid() | atom(), #a2a_message{}, stream_callback()) ->
    {ok, binary()} | {error, term()}.
send_streaming_message(Client, Message, Callback) ->
    send_streaming_message(Client, Message, Callback, #{}).

-spec send_streaming_message(pid() | atom(), #a2a_message{}, stream_callback(), map()) ->
    {ok, binary()} | {error, term()}.
send_streaming_message(Client, #a2a_message{} = Message, Callback, Opts)
  when is_function(Callback, 1) ->
    gen_statem:call(Client, {send_streaming_message, Message, Callback, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Get task status
-spec get_task(pid() | atom(), binary()) -> {ok, #a2a_task{}} | {error, term()}.
get_task(Client, TaskId) ->
    get_task(Client, TaskId, #{}).

-spec get_task(pid() | atom(), binary(), map()) -> {ok, #a2a_task{}} | {error, term()}.
get_task(Client, TaskId, Opts) when is_binary(TaskId) ->
    gen_statem:call(Client, {get_task, TaskId, Opts}, ?DEFAULT_TIMEOUT).

%% @doc List tasks with filters
-spec list_tasks(pid() | atom(), map()) ->
    {ok, #a2a_list_tasks_response{}} | {error, term()}.
list_tasks(Client, Filters) ->
    list_tasks(Client, Filters, #{}).

-spec list_tasks(pid() | atom(), map(), map()) ->
    {ok, #a2a_list_tasks_response{}} | {error, term()}.
list_tasks(Client, Filters, Opts) when is_map(Filters) ->
    gen_statem:call(Client, {list_tasks, Filters, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Cancel a task
-spec cancel_task(pid() | atom(), binary()) -> ok | {error, term()}.
cancel_task(Client, TaskId) ->
    cancel_task(Client, TaskId, #{}).

-spec cancel_task(pid() | atom(), binary(), map()) -> ok | {error, term()}.
cancel_task(Client, TaskId, Opts) when is_binary(TaskId) ->
    gen_statem:call(Client, {cancel_task, TaskId, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Subscribe to task updates (streaming)
-spec subscribe(pid() | atom(), binary()) -> {ok, reference()} | {error, term()}.
subscribe(Client, TaskId) ->
    subscribe(Client, TaskId, undefined).

-spec subscribe(pid() | atom(), binary(), stream_callback() | undefined) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, TaskId, Callback) when is_binary(TaskId) ->
    gen_statem:call(Client, {subscribe, TaskId, Callback}, ?DEFAULT_TIMEOUT).

%% @doc Unsubscribe from task updates
-spec unsubscribe(pid() | atom(), binary()) -> ok | {error, term()}.
unsubscribe(Client, TaskId) when is_binary(TaskId) ->
    gen_statem:call(Client, {unsubscribe, TaskId}, ?DEFAULT_TIMEOUT).

%% @doc Create push notification configuration
-spec create_push_config(pid() | atom(), binary(), #a2a_push_notification_config{}) ->
    {ok, #a2a_task_push_notification_config{}} | {error, term()}.
create_push_config(Client, TaskId, #a2a_push_notification_config{} = Config)
  when is_binary(TaskId) ->
    gen_statem:call(Client, {create_push_config, TaskId, Config}, ?DEFAULT_TIMEOUT).

%% @doc Get push notification configuration
-spec get_push_config(pid() | atom(), binary(), binary()) ->
    {ok, #a2a_task_push_notification_config{}} | {error, term()}.
get_push_config(Client, TaskId, ConfigId)
  when is_binary(TaskId), is_binary(ConfigId) ->
    gen_statem:call(Client, {get_push_config, TaskId, ConfigId}, ?DEFAULT_TIMEOUT).

%% @doc List push notification configurations
-spec list_push_configs(pid() | atom(), binary()) ->
    {ok, #a2a_list_push_configs_response{}} | {error, term()}.
list_push_configs(Client, TaskId) when is_binary(TaskId) ->
    gen_statem:call(Client, {list_push_configs, TaskId}, ?DEFAULT_TIMEOUT).

%% @doc Delete push notification configuration
-spec delete_push_config(pid() | atom(), binary(), binary()) -> ok | {error, term()}.
delete_push_config(Client, TaskId, ConfigId)
  when is_binary(TaskId), is_binary(ConfigId) ->
    gen_statem:call(Client, {delete_push_config, TaskId, ConfigId}, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_statem Callbacks
%%====================================================================

callback_mode() ->
    [state_functions, state_enter].

init(Opts) when is_map(Opts) ->
    process_flag(trap_exit, true),

    AgentUrl = maps:get(agent_url, Opts),
    AuthConfig = maps:get(auth, Opts, undefined),
    Tenant = maps:get(tenant, Opts, undefined),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    RetryOpts = maps:get(retry, Opts, #{}),

    RetryConfig = build_retry_config(RetryOpts),

    State = #state{
        agent_url = AgentUrl,
        auth_config = AuthConfig,
        tenant = Tenant,
        timeout = Timeout,
        transport = ssl,
        retry_config = RetryConfig,
        metrics = #client_metrics{}
    },

    logger:info("[A2A Client] Initialized for agent: ~s", [AgentUrl]),

    {ok, disconnected, State}.

terminate(Reason, _StateName, #state{gun_pid = GunPid} = _State) ->
    logger:info("[A2A Client] Terminating: ~p", [Reason]),
    case GunPid of
        undefined -> ok;
        Pid -> catch gun:close(Pid)
    end,
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% State: disconnected
%%====================================================================

disconnected(enter, _OldState, State) ->
    %% Clean up any leftover state
    NewState = State#state{
        gun_pid = undefined,
        gun_monitor = undefined,
        agent_card = undefined,
        active_interface = undefined,
        auth_token = undefined,
        pending_requests = #{}
    },
    {keep_state, NewState};

disconnected({call, From}, connect, State) ->
    {next_state, connecting, State, [{reply, From, ok}]};

disconnected({call, From}, get_status, State) ->
    Status = build_status(disconnected, State),
    {keep_state_and_data, [{reply, From, {ok, Status}}]};

disconnected({call, From}, disconnect, _State) ->
    {keep_state_and_data, [{reply, From, ok}]};

disconnected({call, From}, get_agent_card, #state{agent_url = Url} = State) ->
    %% Allow fetching agent card without full connection
    case fetch_agent_card(Url) of
        {ok, Card} ->
            {keep_state, State#state{agent_card = Card},
             [{reply, From, {ok, Card}}]};
        {error, _} = Error ->
            {keep_state_and_data, [{reply, From, Error}]}
    end;

disconnected({call, From}, _Request, _State) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

disconnected(info, {retry_connect}, State) ->
    {next_state, connecting, State};

disconnected(EventType, Event, State) ->
    handle_common_event(EventType, Event, disconnected, State).

%%====================================================================
%% State: connecting
%%====================================================================

connecting(enter, _OldState, #state{agent_url = Url} = State) ->
    %% Start connection process
    case do_connect(Url, State) of
        {ok, NewState} ->
            {next_state, connected, NewState};
        {error, Reason} ->
            logger:warning("[A2A Client] Connection failed: ~p", [Reason]),
            handle_connection_failure(Reason, State)
    end;

connecting({call, From}, get_status, State) ->
    Status = build_status(connecting, State),
    {keep_state_and_data, [{reply, From, {ok, Status}}]};

connecting({call, From}, disconnect, State) ->
    {next_state, disconnected, State, [{reply, From, ok}]};

connecting({call, From}, _Request, _State) ->
    {keep_state_and_data, [{reply, From, {error, connecting}}]};

connecting(EventType, Event, State) ->
    handle_common_event(EventType, Event, connecting, State).

%%====================================================================
%% State: connected
%%====================================================================

connected(enter, _OldState, #state{auth_config = AuthConfig} = State) ->
    case AuthConfig of
        undefined ->
            %% No authentication needed, stay in connected state
            {keep_state, State};
        _ ->
            %% Need to authenticate
            case do_authenticate(State) of
                {ok, NewState} ->
                    {next_state, authenticated, NewState};
                {error, Reason} ->
                    logger:error("[A2A Client] Authentication failed: ~p", [Reason]),
                    {next_state, disconnected, State}
            end
    end;

connected({call, From}, get_status, State) ->
    Status = build_status(connected, State),
    {keep_state_and_data, [{reply, From, {ok, Status}}]};

connected({call, From}, disconnect, State) ->
    {next_state, disconnected, State, [{reply, From, ok}]};

connected({call, From}, get_agent_card, #state{agent_card = Card} = _State) ->
    {keep_state_and_data, [{reply, From, {ok, Card}}]};

%% Forward requests to authenticated state handlers for non-auth endpoints
connected({call, From}, Request, State) ->
    handle_authenticated_request(Request, From, State);

connected(info, {gun_up, GunPid, http2}, #state{gun_pid = GunPid} = _State) ->
    logger:debug("[A2A Client] HTTP/2 connection established"),
    keep_state_and_data;

connected(info, {gun_down, GunPid, _, Reason, _}, #state{gun_pid = GunPid} = State) ->
    logger:warning("[A2A Client] Connection down: ~p", [Reason]),
    handle_connection_failure(Reason, State);

connected(EventType, Event, State) ->
    handle_common_event(EventType, Event, connected, State).

%%====================================================================
%% State: authenticated
%%====================================================================

authenticated(enter, _OldState, State) ->
    logger:info("[A2A Client] Authenticated and ready"),
    {keep_state, State};

authenticated({call, From}, get_status, State) ->
    Status = build_status(authenticated, State),
    {keep_state_and_data, [{reply, From, {ok, Status}}]};

authenticated({call, From}, disconnect, State) ->
    {next_state, disconnected, State, [{reply, From, ok}]};

authenticated({call, From}, Request, State) ->
    handle_authenticated_request(Request, From, State);

authenticated(info, {gun_response, GunPid, StreamRef, fin, Status, Headers},
              #state{gun_pid = GunPid} = State) ->
    handle_gun_response(StreamRef, Status, Headers, <<>>, State);

authenticated(info, {gun_response, GunPid, StreamRef, nofin, Status, Headers},
              #state{gun_pid = GunPid, pending_requests = Pending} = State) ->
    %% Store headers for when we get the body
    case maps:get(StreamRef, Pending, undefined) of
        undefined ->
            keep_state_and_data;
        Request ->
            UpdatedRequest = Request#pending_request{
                stream_ref = {status_headers, Status, Headers}
            },
            NewPending = maps:put(StreamRef, UpdatedRequest, Pending),
            {keep_state, State#state{pending_requests = NewPending}}
    end;

authenticated(info, {gun_data, GunPid, StreamRef, fin, Data},
              #state{gun_pid = GunPid, pending_requests = Pending} = State) ->
    case maps:get(StreamRef, Pending, undefined) of
        undefined ->
            keep_state_and_data;
        #pending_request{stream_ref = {status_headers, Status, Headers}} = _Request ->
            handle_gun_response(StreamRef, Status, Headers, Data, State);
        _ ->
            keep_state_and_data
    end;

authenticated(info, {gun_data, GunPid, StreamRef, nofin, Data},
              #state{gun_pid = GunPid, stream_handlers = Handlers} = State) ->
    %% Handle streaming data
    case maps:get(StreamRef, Handlers, undefined) of
        undefined ->
            keep_state_and_data;
        #stream_handler{callback = Callback, buffer = Buffer} = Handler ->
            %% Parse SSE events from the buffer
            NewBuffer = <<Buffer/binary, Data/binary>>,
            {Events, Remaining} = parse_sse_events(NewBuffer),
            lists:foreach(fun(Event) ->
                handle_stream_event(Event, Callback)
            end, Events),
            NewHandler = Handler#stream_handler{buffer = Remaining},
            NewHandlers = maps:put(StreamRef, NewHandler, Handlers),
            {keep_state, State#state{stream_handlers = NewHandlers}}
    end;

authenticated(info, {gun_down, GunPid, _, Reason, _}, #state{gun_pid = GunPid} = State) ->
    logger:warning("[A2A Client] Connection down: ~p", [Reason]),
    handle_connection_failure(Reason, State);

authenticated(info, {gun_error, GunPid, StreamRef, Reason},
              #state{gun_pid = GunPid, pending_requests = Pending} = State) ->
    case maps:get(StreamRef, Pending, undefined) of
        undefined ->
            keep_state_and_data;
        #pending_request{from = From, timeout_ref = TimerRef} = _Request ->
            cancel_timer(TimerRef),
            gen_statem:reply(From, {error, {gun_error, Reason}}),
            NewPending = maps:remove(StreamRef, Pending),
            {keep_state, State#state{pending_requests = NewPending}}
    end;

authenticated(info, {request_timeout, StreamRef},
              #state{pending_requests = Pending, gun_pid = GunPid} = State) ->
    case maps:get(StreamRef, Pending, undefined) of
        undefined ->
            keep_state_and_data;
        #pending_request{from = From} = _Request ->
            %% Cancel the stream
            catch gun:cancel(GunPid, StreamRef),
            gen_statem:reply(From, {error, timeout}),
            NewPending = maps:remove(StreamRef, Pending),
            {keep_state, State#state{pending_requests = NewPending}}
    end;

authenticated(EventType, Event, State) ->
    handle_common_event(EventType, Event, authenticated, State).

%%====================================================================
%% Request Handlers (used in connected/authenticated states)
%%====================================================================

handle_authenticated_request(get_agent_card, From, #state{agent_card = Card} = _State) ->
    {keep_state_and_data, [{reply, From, {ok, Card}}]};

handle_authenticated_request(get_extended_agent_card, From, State) ->
    Request = #a2a_get_extended_agent_card_request{
        tenant = State#state.tenant
    },
    do_rpc_request(?A2A_METHOD_GET_EXTENDED_AGENT_CARD, Request, From, State);

handle_authenticated_request({send_message, Message, Opts}, From, State) ->
    Configuration = build_send_message_config(Opts),
    Request = #a2a_send_message_request{
        tenant = State#state.tenant,
        message = Message,
        configuration = Configuration,
        metadata = maps:get(metadata, Opts, undefined)
    },
    do_rpc_request(?A2A_METHOD_SEND_MESSAGE, Request, From, State);

handle_authenticated_request({send_streaming_message, Message, Callback, Opts}, From, State) ->
    Configuration = build_send_message_config(Opts),
    Request = #a2a_send_message_request{
        tenant = State#state.tenant,
        message = Message,
        configuration = Configuration,
        metadata = maps:get(metadata, Opts, undefined)
    },
    do_streaming_request(?A2A_METHOD_SEND_STREAMING_MESSAGE, Request, Callback, From, State);

handle_authenticated_request({get_task, TaskId, Opts}, From, State) ->
    Request = #a2a_get_task_request{
        tenant = State#state.tenant,
        id = TaskId,
        history_length = maps:get(history_length, Opts, undefined)
    },
    do_rpc_request(?A2A_METHOD_GET_TASK, Request, From, State);

handle_authenticated_request({list_tasks, Filters, _Opts}, From, State) ->
    Request = #a2a_list_tasks_request{
        tenant = State#state.tenant,
        context_id = maps:get(context_id, Filters, undefined),
        status = maps:get(status, Filters, undefined),
        page_size = maps:get(page_size, Filters, undefined),
        page_token = maps:get(page_token, Filters, undefined),
        history_length = maps:get(history_length, Filters, undefined),
        status_timestamp_after = maps:get(status_timestamp_after, Filters, undefined),
        include_artifacts = maps:get(include_artifacts, Filters, undefined)
    },
    do_rpc_request(?A2A_METHOD_LIST_TASKS, Request, From, State);

handle_authenticated_request({cancel_task, TaskId, _Opts}, From, State) ->
    Request = #a2a_cancel_task_request{
        tenant = State#state.tenant,
        id = TaskId
    },
    do_rpc_request(?A2A_METHOD_CANCEL_TASK, Request, From, State);

handle_authenticated_request({subscribe, TaskId, Callback}, From, State) ->
    Request = #a2a_subscribe_to_task_request{
        tenant = State#state.tenant,
        id = TaskId
    },
    do_subscription_request(?A2A_METHOD_SUBSCRIBE_TO_TASK, Request, TaskId, Callback, From, State);

handle_authenticated_request({unsubscribe, TaskId}, From,
                              #state{active_subscriptions = Subs, gun_pid = GunPid} = State) ->
    case maps:get(TaskId, Subs, undefined) of
        undefined ->
            {keep_state_and_data, [{reply, From, {error, not_subscribed}}]};
        #subscription_info{stream_ref = StreamRef} ->
            catch gun:cancel(GunPid, StreamRef),
            NewSubs = maps:remove(TaskId, Subs),
            {keep_state, State#state{active_subscriptions = NewSubs},
             [{reply, From, ok}]}
    end;

handle_authenticated_request({create_push_config, TaskId, Config}, From, State) ->
    ConfigId = generate_uuid(),
    Request = #a2a_create_push_config_request{
        tenant = State#state.tenant,
        task_id = TaskId,
        config_id = ConfigId,
        config = Config
    },
    do_rpc_request(?A2A_METHOD_CREATE_PUSH_CONFIG, Request, From, State);

handle_authenticated_request({get_push_config, TaskId, ConfigId}, From, State) ->
    Request = #a2a_get_push_config_request{
        tenant = State#state.tenant,
        task_id = TaskId,
        id = ConfigId
    },
    do_rpc_request(?A2A_METHOD_GET_PUSH_CONFIG, Request, From, State);

handle_authenticated_request({list_push_configs, TaskId}, From, State) ->
    Request = #a2a_list_push_configs_request{
        tenant = State#state.tenant,
        task_id = TaskId
    },
    do_rpc_request(?A2A_METHOD_LIST_PUSH_CONFIGS, Request, From, State);

handle_authenticated_request({delete_push_config, TaskId, ConfigId}, From, State) ->
    Request = #a2a_delete_push_config_request{
        tenant = State#state.tenant,
        task_id = TaskId,
        id = ConfigId
    },
    do_rpc_request(?A2A_METHOD_DELETE_PUSH_CONFIG, Request, From, State);

handle_authenticated_request(_Unknown, From, _State) ->
    {keep_state_and_data, [{reply, From, {error, unknown_request}}]}.

%%====================================================================
%% Common Event Handler
%%====================================================================

handle_common_event(info, {'DOWN', MonRef, process, GunPid, Reason},
                    _StateName, #state{gun_monitor = MonRef, gun_pid = GunPid} = State) ->
    logger:error("[A2A Client] Gun process died: ~p", [Reason]),
    handle_connection_failure(Reason, State);

handle_common_event(info, {'EXIT', _Pid, normal}, _StateName, _State) ->
    keep_state_and_data;

handle_common_event(info, {'EXIT', _Pid, Reason}, _StateName, _State) ->
    logger:warning("[A2A Client] Linked process died: ~p", [Reason]),
    keep_state_and_data;

handle_common_event(_EventType, _Event, _StateName, _State) ->
    keep_state_and_data.

%%====================================================================
%% Internal Functions - Connection
%%====================================================================

do_connect(AgentUrl, State) ->
    %% Fetch agent card first
    case fetch_agent_card(AgentUrl) of
        {ok, AgentCard} ->
            %% Select best interface
            case select_interface(AgentCard) of
                {ok, Interface} ->
                    %% Parse interface URL and connect
                    case parse_url(Interface#a2a_agent_interface.url) of
                        {ok, Host, Port, _Path, Transport} ->
                            connect_to_host(Host, Port, Transport,
                                           AgentCard, Interface, State);
                        {error, Reason} ->
                            {error, {invalid_interface_url, Reason}}
                    end;
                {error, Reason} ->
                    {error, {no_suitable_interface, Reason}}
            end;
        {error, Reason} ->
            {error, {agent_card_fetch_failed, Reason}}
    end.

connect_to_host(Host, Port, Transport, AgentCard, Interface, State) ->
    GunOpts = #{
        transport => Transport,
        protocols => [http2, http],
        retry => 0,
        trace => false,
        http2_opts => #{
            max_concurrent_streams => 100
        }
    },

    GunOpts1 = case Transport of
        ssl ->
            GunOpts#{transport_opts => build_ssl_opts(State)};
        tcp ->
            GunOpts
    end,

    case gun:open(Host, Port, GunOpts1) of
        {ok, GunPid} ->
            MonRef = monitor(process, GunPid),
            case gun:await_up(GunPid, ?GUN_CONNECT_TIMEOUT) of
                {ok, _Protocol} ->
                    logger:info("[A2A Client] Connected to ~s:~p", [Host, Port]),
                    {ok, State#state{
                        gun_pid = GunPid,
                        gun_monitor = MonRef,
                        host = Host,
                        port = Port,
                        transport = Transport,
                        agent_card = AgentCard,
                        active_interface = Interface,
                        retry_count = 0
                    }};
                {error, Reason} ->
                    gun:close(GunPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {gun_open_failed, Reason}}
    end.

fetch_agent_card(AgentUrl) ->
    %% The agent card is typically at /.well-known/agent.json
    CardUrl = build_agent_card_url(AgentUrl),
    case httpc:request(get, {binary_to_list(CardUrl), []},
                       [{timeout, 10000}, {connect_timeout, 5000}], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            case json:decode(list_to_binary(Body)) of
                Map when is_map(Map) ->
                    erlmcp_a2a_protocol:decode_agent_card(Map);
                _ ->
                    {error, invalid_agent_card_format}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

build_agent_card_url(AgentUrl) ->
    %% Append /.well-known/agent.json if not already present
    case binary:match(AgentUrl, <<".well-known/agent.json">>) of
        nomatch ->
            Url1 = case binary:last(AgentUrl) of
                $/ -> AgentUrl;
                _ -> <<AgentUrl/binary, "/">>
            end,
            <<Url1/binary, ".well-known/agent.json">>;
        _ ->
            AgentUrl
    end.

select_interface(#a2a_agent_card{supported_interfaces = Interfaces}) ->
    %% Prefer JSON-RPC over other bindings
    Preference = [?A2A_PROTOCOL_BINDING_JSONRPC,
                  ?A2A_PROTOCOL_BINDING_HTTP_JSON,
                  ?A2A_PROTOCOL_BINDING_GRPC],

    case find_preferred_interface(Preference, Interfaces) of
        undefined ->
            case Interfaces of
                [First | _] -> {ok, First};
                [] -> {error, no_interfaces}
            end;
        Interface ->
            {ok, Interface}
    end.

find_preferred_interface([], _Interfaces) ->
    undefined;
find_preferred_interface([Binding | Rest], Interfaces) ->
    case lists:keyfind(Binding, #a2a_agent_interface.protocol_binding, Interfaces) of
        false -> find_preferred_interface(Rest, Interfaces);
        Interface -> Interface
    end.

%%====================================================================
%% Internal Functions - Authentication
%%====================================================================

do_authenticate(#state{auth_config = #{type := api_key} = Config} = State) ->
    %% API key authentication - just store the token
    ApiKey = maps:get(api_key, Config),
    {ok, State#state{auth_token = ApiKey}};

do_authenticate(#state{auth_config = #{type := http_auth} = Config} = State) ->
    %% HTTP auth - build the header value
    Scheme = maps:get(scheme, Config, <<"Bearer">>),
    Credentials = maps:get(credentials, Config),
    Token = <<Scheme/binary, " ", Credentials/binary>>,
    {ok, State#state{auth_token = Token}};

do_authenticate(#state{auth_config = #{type := oauth2} = Config} = State) ->
    %% OAuth2 - fetch access token
    TokenUrl = maps:get(token_url, Config),
    ClientId = maps:get(client_id, Config),
    ClientSecret = maps:get(client_secret, Config),
    Scopes = maps:get(scopes, Config, []),

    Body = uri_string:compose_query([
        {<<"grant_type">>, <<"client_credentials">>},
        {<<"client_id">>, ClientId},
        {<<"client_secret">>, ClientSecret},
        {<<"scope">>, iolist_to_binary(lists:join(<<" ">>, Scopes))}
    ]),

    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    case httpc:request(post, {binary_to_list(TokenUrl), Headers,
                              "application/x-www-form-urlencoded",
                              Body},
                       [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            case json:decode(list_to_binary(RespBody)) of
                #{<<"access_token">> := Token} = TokenResp ->
                    ExpiresIn = maps:get(<<"expires_in">>, TokenResp, 3600),
                    ExpiresAt = erlang:system_time(millisecond) +
                               (ExpiresIn * 1000) - ?AUTH_REFRESH_MARGIN_MS,
                    {ok, State#state{
                        auth_token = Token,
                        auth_expires = ExpiresAt
                    }};
                _ ->
                    {error, invalid_token_response}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, {oauth2_error, StatusCode}};
        {error, Reason} ->
            {error, {oauth2_request_failed, Reason}}
    end;

do_authenticate(#state{auth_config = #{type := _}} = State) ->
    %% Other auth types - assume no token needed or handled elsewhere
    {ok, State}.

%%====================================================================
%% Internal Functions - RPC Requests
%%====================================================================

do_rpc_request(Method, Request, From, State) ->
    #state{
        gun_pid = GunPid,
        active_interface = Interface,
        request_id = ReqId,
        pending_requests = Pending,
        timeout = Timeout,
        metrics = Metrics
    } = State,

    %% Build JSON-RPC request
    Params = encode_request(Method, Request),
    JsonRpcRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => ReqId,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Body = json:encode(JsonRpcRequest),

    %% Build path from interface
    Path = get_interface_path(Interface),
    Headers = build_request_headers(State),

    %% Send request
    StreamRef = gun:post(GunPid, Path, Headers, Body),

    %% Setup timeout
    TimerRef = erlang:send_after(Timeout, self(), {request_timeout, StreamRef}),

    %% Track pending request
    PendingReq = #pending_request{
        id = ReqId,
        method = Method,
        from = From,
        start_time = erlang:monotonic_time(microsecond),
        stream_ref = StreamRef,
        timeout_ref = TimerRef
    },
    NewPending = maps:put(StreamRef, PendingReq, Pending),

    NewMetrics = Metrics#client_metrics{
        requests_sent = Metrics#client_metrics.requests_sent + 1
    },

    {keep_state, State#state{
        request_id = ReqId + 1,
        pending_requests = NewPending,
        metrics = NewMetrics
    }}.

do_streaming_request(Method, Request, Callback, From, State) ->
    #state{
        gun_pid = GunPid,
        active_interface = Interface,
        request_id = ReqId,
        stream_handlers = Handlers
    } = State,

    %% Build JSON-RPC request
    Params = encode_request(Method, Request),
    JsonRpcRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => ReqId,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Body = json:encode(JsonRpcRequest),

    %% Build path and headers for SSE
    Path = get_interface_path(Interface),
    Headers = build_request_headers(State) ++
              [{<<"accept">>, <<"text/event-stream">>}],

    %% Send request
    StreamRef = gun:post(GunPid, Path, Headers, Body),

    %% Track stream handler
    TaskId = case Request of
        #a2a_send_message_request{message = #a2a_message{task_id = TId}} when TId =/= undefined ->
            TId;
        _ ->
            generate_uuid()
    end,

    Handler = #stream_handler{
        task_id = TaskId,
        callback = Callback,
        stream_ref = StreamRef
    },
    NewHandlers = maps:put(StreamRef, Handler, Handlers),

    %% Reply with task ID immediately
    gen_statem:reply(From, {ok, TaskId}),

    {keep_state, State#state{
        request_id = ReqId + 1,
        stream_handlers = NewHandlers
    }}.

do_subscription_request(Method, Request, TaskId, Callback, From, State) ->
    #state{
        gun_pid = GunPid,
        active_interface = Interface,
        request_id = ReqId,
        active_subscriptions = Subs
    } = State,

    %% Check if already subscribed
    case maps:is_key(TaskId, Subs) of
        true ->
            {keep_state_and_data, [{reply, From, {error, already_subscribed}}]};
        false ->
            %% Build JSON-RPC request
            Params = encode_request(Method, Request),
            JsonRpcRequest = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => ReqId,
                <<"method">> => Method,
                <<"params">> => Params
            },
            Body = json:encode(JsonRpcRequest),

            %% Build path and headers for SSE
            Path = get_interface_path(Interface),
            Headers = build_request_headers(State) ++
                      [{<<"accept">>, <<"text/event-stream">>}],

            %% Send request
            StreamRef = gun:post(GunPid, Path, Headers, Body),

            %% Track subscription
            SubRef = make_ref(),
            SubInfo = #subscription_info{
                task_id = TaskId,
                callback = Callback,
                stream_ref = StreamRef
            },
            NewSubs = maps:put(TaskId, SubInfo, Subs),

            {keep_state, State#state{
                request_id = ReqId + 1,
                active_subscriptions = NewSubs
            }, [{reply, From, {ok, SubRef}}]}
    end.

%%====================================================================
%% Internal Functions - Response Handling
%%====================================================================

handle_gun_response(StreamRef, Status, _Headers, Body,
                    #state{pending_requests = Pending, metrics = Metrics} = State) ->
    case maps:get(StreamRef, Pending, undefined) of
        undefined ->
            keep_state_and_data;
        #pending_request{method = Method, from = From, start_time = StartTime,
                         timeout_ref = TimerRef} = _Request ->
            cancel_timer(TimerRef),

            %% Calculate latency
            Latency = erlang:monotonic_time(microsecond) - StartTime,
            NewPending = maps:remove(StreamRef, Pending),

            %% Parse response
            Result = case Status of
                200 ->
                    parse_json_rpc_response(Body, Method);
                _ ->
                    {error, {http_error, Status, Body}}
            end,

            %% Update metrics
            NewMetrics = update_metrics(Metrics, Result, Latency),

            gen_statem:reply(From, Result),

            {keep_state, State#state{
                pending_requests = NewPending,
                metrics = NewMetrics
            }}
    end.

parse_json_rpc_response(Body, Method) ->
    try json:decode(Body) of
        #{<<"result">> := Result} ->
            decode_response(Method, Result);
        #{<<"error">> := Error} ->
            {error, {jsonrpc_error, Error}};
        _ ->
            {error, invalid_jsonrpc_response}
    catch
        _:_ ->
            {error, {json_parse_error, Body}}
    end.

decode_response(?A2A_METHOD_SEND_MESSAGE, Result) ->
    erlmcp_a2a_protocol:decode_send_message_response(Result);
decode_response(?A2A_METHOD_GET_TASK, Result) ->
    erlmcp_a2a_protocol:decode_task(Result);
decode_response(?A2A_METHOD_LIST_TASKS, Result) ->
    erlmcp_a2a_protocol:decode_list_tasks_response(Result);
decode_response(?A2A_METHOD_CANCEL_TASK, _Result) ->
    ok;
decode_response(?A2A_METHOD_GET_EXTENDED_AGENT_CARD, Result) ->
    erlmcp_a2a_protocol:decode_agent_card(Result);
decode_response(?A2A_METHOD_CREATE_PUSH_CONFIG, Result) ->
    decode_push_config_result(Result);
decode_response(?A2A_METHOD_GET_PUSH_CONFIG, Result) ->
    decode_push_config_result(Result);
decode_response(?A2A_METHOD_LIST_PUSH_CONFIGS, Result) ->
    decode_list_push_configs_result(Result);
decode_response(?A2A_METHOD_DELETE_PUSH_CONFIG, _Result) ->
    ok;
decode_response(_Method, Result) ->
    {ok, Result}.

decode_push_config_result(Result) ->
    {ok, Config} = erlmcp_a2a_protocol:decode_push_notification_config(
        maps:get(<<"pushNotificationConfig">>, Result, Result)
    ),
    TaskPushConfig = #a2a_task_push_notification_config{
        tenant = maps:get(<<"tenant">>, Result, undefined),
        id = maps:get(<<"id">>, Result),
        task_id = maps:get(<<"taskId">>, Result),
        push_notification_config = Config
    },
    {ok, TaskPushConfig}.

decode_list_push_configs_result(Result) ->
    ConfigMaps = maps:get(<<"configs">>, Result, []),
    Configs = [begin
        {ok, C} = erlmcp_a2a_protocol:decode_push_notification_config(
            maps:get(<<"pushNotificationConfig">>, M, M)
        ),
        #a2a_task_push_notification_config{
            tenant = maps:get(<<"tenant">>, M, undefined),
            id = maps:get(<<"id">>, M),
            task_id = maps:get(<<"taskId">>, M),
            push_notification_config = C
        }
    end || M <- ConfigMaps],
    {ok, #a2a_list_push_configs_response{
        configs = Configs,
        next_page_token = maps:get(<<"nextPageToken">>, Result, undefined)
    }}.

%%====================================================================
%% Internal Functions - Streaming
%%====================================================================

parse_sse_events(Buffer) ->
    parse_sse_events(Buffer, []).

parse_sse_events(Buffer, Acc) ->
    case binary:split(Buffer, <<"\n\n">>) of
        [Event, Rest] ->
            ParsedEvent = parse_sse_event(Event),
            parse_sse_events(Rest, [ParsedEvent | Acc]);
        [_Incomplete] ->
            {lists:reverse(Acc), Buffer}
    end.

parse_sse_event(EventData) ->
    Lines = binary:split(EventData, <<"\n">>, [global]),
    parse_sse_lines(Lines, #{}).

parse_sse_lines([], Acc) -> Acc;
parse_sse_lines([<<>> | Rest], Acc) ->
    parse_sse_lines(Rest, Acc);
parse_sse_lines([Line | Rest], Acc) ->
    case binary:split(Line, <<": ">>) of
        [<<"data">>, Value] ->
            Data = maps:get(data, Acc, <<>>),
            parse_sse_lines(Rest, Acc#{data => <<Data/binary, Value/binary>>});
        [<<"event">>, Value] ->
            parse_sse_lines(Rest, Acc#{event => Value});
        [<<"id">>, Value] ->
            parse_sse_lines(Rest, Acc#{id => Value});
        _ ->
            parse_sse_lines(Rest, Acc)
    end.

handle_stream_event(#{data := Data} = Event, Callback) ->
    EventType = maps:get(event, Event, <<"message">>),
    try json:decode(Data) of
        JsonData ->
            ParsedEvent = case EventType of
                <<"taskStatusUpdate">> ->
                    {ok, Update} = erlmcp_a2a_protocol:decode_task_status_update_event(JsonData),
                    {status_update, Update};
                <<"taskArtifactUpdate">> ->
                    {ok, Update} = erlmcp_a2a_protocol:decode_task_artifact_update_event(JsonData),
                    {artifact_update, Update};
                <<"done">> ->
                    complete;
                _ ->
                    {message, JsonData}
            end,
            catch Callback(ParsedEvent)
    catch
        _:_ ->
            catch Callback({error, {invalid_event_data, Data}})
    end;
handle_stream_event(_Event, _Callback) ->
    ok.

%%====================================================================
%% Internal Functions - Retry and Failure Handling
%%====================================================================

handle_connection_failure(Reason, #state{retry_config = RetryConfig,
                                          retry_count = Count} = State) ->
    #retry_config{max_retries = MaxRetries} = RetryConfig,
    if
        Count < MaxRetries ->
            Delay = calculate_backoff(Count, RetryConfig),
            logger:info("[A2A Client] Scheduling retry ~p/~p in ~pms",
                       [Count + 1, MaxRetries, Delay]),
            TimerRef = erlang:send_after(Delay, self(), {retry_connect}),
            NewState = State#state{
                retry_count = Count + 1,
                retry_timer = TimerRef,
                metrics = (State#state.metrics)#client_metrics{
                    reconnections = (State#state.metrics)#client_metrics.reconnections + 1
                }
            },
            {next_state, disconnected, NewState};
        true ->
            logger:error("[A2A Client] Max retries exceeded, giving up"),
            %% Fail all pending requests
            fail_pending_requests(State, {error, {connection_failed, Reason}}),
            {next_state, disconnected, State#state{retry_count = 0}}
    end.

calculate_backoff(RetryCount, #retry_config{base_delay_ms = BaseDelay,
                                             max_delay_ms = MaxDelay,
                                             jitter = UseJitter}) ->
    %% Exponential backoff: base * 2^retry
    Delay = min(BaseDelay * (1 bsl RetryCount), MaxDelay),
    case UseJitter of
        true ->
            %% Add up to 25% jitter
            Jitter = rand:uniform(Delay div 4),
            Delay + Jitter;
        false ->
            Delay
    end.

fail_pending_requests(#state{pending_requests = Pending}, Error) ->
    maps:foreach(fun(_StreamRef, #pending_request{from = From, timeout_ref = TimerRef}) ->
        cancel_timer(TimerRef),
        gen_statem:reply(From, Error)
    end, Pending).

%%====================================================================
%% Internal Functions - Helpers
%%====================================================================

build_retry_config(Opts) when is_map(Opts) ->
    #retry_config{
        max_retries = maps:get(max_retries, Opts, 5),
        base_delay_ms = maps:get(base_delay_ms, Opts, 1000),
        max_delay_ms = maps:get(max_delay_ms, Opts, 30000),
        jitter = maps:get(jitter, Opts, true)
    };
build_retry_config(#retry_config{} = Config) ->
    Config.

build_send_message_config(Opts) ->
    PushConfig = case maps:get(push_notification_config, Opts, undefined) of
        undefined -> undefined;
        Config -> Config
    end,
    #a2a_send_message_configuration{
        accepted_output_modes = maps:get(accepted_output_modes, Opts, undefined),
        push_notification_config = PushConfig,
        history_length = maps:get(history_length, Opts, undefined),
        blocking = maps:get(blocking, Opts, false)
    }.

build_request_headers(#state{auth_config = AuthConfig, auth_token = Token} = _State) ->
    BaseHeaders = [
        {<<"content-type">>, <<"application/json">>},
        {<<"accept">>, <<"application/json">>}
    ],
    case {AuthConfig, Token} of
        {undefined, _} ->
            BaseHeaders;
        {#{type := api_key, api_key_location := header, api_key_name := Name}, ApiKey} ->
            [{Name, ApiKey} | BaseHeaders];
        {#{type := http_auth}, AuthHeader} ->
            [{<<"authorization">>, AuthHeader} | BaseHeaders];
        {#{type := oauth2}, AccessToken} ->
            [{<<"authorization">>, <<"Bearer ", AccessToken/binary>>} | BaseHeaders];
        _ ->
            BaseHeaders
    end.

get_interface_path(#a2a_agent_interface{url = Url}) ->
    case parse_url(Url) of
        {ok, _Host, _Port, Path, _Transport} -> Path;
        _ -> <<"/">>
    end.

parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url(Url) when is_list(Url) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parsed ->
            Port = maps:get(port, Parsed, default_port(Scheme)),
            Path = maps:get(path, Parsed, "/"),
            PathBin = iolist_to_binary(Path),
            Transport = case Scheme of
                "https" -> ssl;
                "http" -> tcp;
                _ -> ssl
            end,
            {ok, Host, Port, PathBin, Transport};
        _ ->
            {error, invalid_url}
    end.

default_port("https") -> 443;
default_port("http") -> 80;
default_port(_) -> 443.

build_ssl_opts(#state{auth_config = #{type := mtls} = Config}) ->
    [
        {certfile, binary_to_list(maps:get(cert_file, Config))},
        {keyfile, binary_to_list(maps:get(key_file, Config))},
        {cacertfile, binary_to_list(maps:get(ca_file, Config))},
        {verify, verify_peer},
        {versions, ['tlsv1.3', 'tlsv1.2']}
    ];
build_ssl_opts(_State) ->
    [
        {verify, verify_peer},
        {versions, ['tlsv1.3', 'tlsv1.2']}
    ].

build_status(StateName, #state{agent_url = Url, agent_card = Card,
                                 active_interface = Interface,
                                 retry_count = RetryCount,
                                 metrics = Metrics} = _State) ->
    #{
        state => StateName,
        agent_url => Url,
        agent_name => case Card of
            #a2a_agent_card{name = Name} -> Name;
            undefined -> undefined
        end,
        interface => case Interface of
            #a2a_agent_interface{url = IUrl, protocol_binding = Binding} ->
                #{url => IUrl, binding => Binding};
            undefined -> undefined
        end,
        retry_count => RetryCount,
        metrics => #{
            requests_sent => Metrics#client_metrics.requests_sent,
            responses_received => Metrics#client_metrics.responses_received,
            errors => Metrics#client_metrics.errors,
            reconnections => Metrics#client_metrics.reconnections,
            avg_latency_us => Metrics#client_metrics.avg_latency_us
        }
    }.

encode_request(?A2A_METHOD_SEND_MESSAGE, Request) ->
    erlmcp_a2a_protocol:encode_send_message_request(Request);
encode_request(?A2A_METHOD_SEND_STREAMING_MESSAGE, Request) ->
    erlmcp_a2a_protocol:encode_send_message_request(Request);
encode_request(?A2A_METHOD_GET_TASK, Request) ->
    erlmcp_a2a_protocol:encode_get_task_request(Request);
encode_request(?A2A_METHOD_LIST_TASKS, Request) ->
    erlmcp_a2a_protocol:encode_list_tasks_request(Request);
encode_request(?A2A_METHOD_CANCEL_TASK, Request) ->
    erlmcp_a2a_protocol:encode_cancel_task_request(Request);
encode_request(?A2A_METHOD_SUBSCRIBE_TO_TASK, #a2a_subscribe_to_task_request{} = Req) ->
    #{<<"id">> => Req#a2a_subscribe_to_task_request.id,
      <<"tenant">> => Req#a2a_subscribe_to_task_request.tenant};
encode_request(?A2A_METHOD_GET_EXTENDED_AGENT_CARD, #a2a_get_extended_agent_card_request{} = Req) ->
    #{<<"tenant">> => Req#a2a_get_extended_agent_card_request.tenant};
encode_request(?A2A_METHOD_CREATE_PUSH_CONFIG, #a2a_create_push_config_request{} = Req) ->
    #{<<"tenant">> => Req#a2a_create_push_config_request.tenant,
      <<"taskId">> => Req#a2a_create_push_config_request.task_id,
      <<"configId">> => Req#a2a_create_push_config_request.config_id,
      <<"config">> => erlmcp_a2a_protocol:encode_push_notification_config(
          Req#a2a_create_push_config_request.config)};
encode_request(?A2A_METHOD_GET_PUSH_CONFIG, #a2a_get_push_config_request{} = Req) ->
    #{<<"tenant">> => Req#a2a_get_push_config_request.tenant,
      <<"taskId">> => Req#a2a_get_push_config_request.task_id,
      <<"id">> => Req#a2a_get_push_config_request.id};
encode_request(?A2A_METHOD_LIST_PUSH_CONFIGS, #a2a_list_push_configs_request{} = Req) ->
    #{<<"tenant">> => Req#a2a_list_push_configs_request.tenant,
      <<"taskId">> => Req#a2a_list_push_configs_request.task_id,
      <<"pageSize">> => Req#a2a_list_push_configs_request.page_size,
      <<"pageToken">> => Req#a2a_list_push_configs_request.page_token};
encode_request(?A2A_METHOD_DELETE_PUSH_CONFIG, #a2a_delete_push_config_request{} = Req) ->
    #{<<"tenant">> => Req#a2a_delete_push_config_request.tenant,
      <<"taskId">> => Req#a2a_delete_push_config_request.task_id,
      <<"id">> => Req#a2a_delete_push_config_request.id};
encode_request(_Method, Request) when is_map(Request) ->
    Request.

update_metrics(Metrics, Result, Latency) ->
    #client_metrics{
        responses_received = Responses,
        errors = Errors,
        avg_latency_us = AvgLatency
    } = Metrics,

    NewResponses = Responses + 1,
    NewAvgLatency = (AvgLatency * Responses + Latency) / NewResponses,

    case Result of
        {ok, _} ->
            Metrics#client_metrics{
                responses_received = NewResponses,
                avg_latency_us = NewAvgLatency
            };
        ok ->
            Metrics#client_metrics{
                responses_received = NewResponses,
                avg_latency_us = NewAvgLatency
            };
        {error, _} ->
            Metrics#client_metrics{
                responses_received = NewResponses,
                errors = Errors + 1,
                avg_latency_us = NewAvgLatency
            }
    end.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).

generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, (C band 16#0fff) bor 16#4000,
                                    (D band 16#3fff) bor 16#8000, E])).
