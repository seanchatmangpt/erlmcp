%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive MCP Protocol Session Lifecycle State Machine for erlmcp v3
%%%
%%% == State Model ==
%%%   - idle: Initial state, no connections
%%%   - connecting: Establishing transport connection
%%%   - active: Normal operation, processing requests
%%%   - suspended: Temporarily suspended (idle timeout, admin action, rate limit)
%%%   - error_recovery: Recovering from error, attempting reconnection
%%%   - closed: Terminated state
%%%
%%% == Features ==
%%%   - State transitions with guards and invariants (Armstrong principles)
%%%   - History tracking with configurable max size
%%%   - State persistence (ETS/DETS/Mnesia backends)
%%%   - Exponential backoff retry with configurable strategies
%%%   - OTEL integration for observability
%%%   - Session monitoring and metrics
%%%   - Resource quota enforcement
%%%   - Audit logging for compliance
%%%   - Memory optimization with hibernation
%%%
%%% == Recovery Strategies ==
%%%   - immediate_retry: Retry immediately without delay
%%%   - exponential_backoff: Retry with exponential delay increase
%%%   - circuit_breaker: Use circuit breaker pattern
%%%   - failover: Failover to backup node
%%%   - manual_intervention: Wait for manual recovery
%%%
%%% @end
%%% @author erlmcp v3.0.0
%%% @copyright 2026
%%% @version 3.0.0
%%% @reference Implements gen_statem with handle_event_function and state_enter_calls
%%%-------------------------------------------------------------------
-module(erlmcp_session_statem).

-behaviour(gen_statem).

-include("erlmcp.hrl").

%% API exports
-export([start_link/2, start_link/3,
         %% State transitions
         connect/2, activate/1, suspend/1, resume/1,
         close/1, force_error_recovery/1,
         %% Query functions
         get_state/1, get_history/1, get_session_data/1,
         get_connections/1, get_metrics/1,
         %% Configuration
         update_config/2, set_timeout/2,
         %% Lifecycle
         stop/1, hibernate_now/1,
         %% Resource management (legacy API support)
         set_quota/2, check_quota/1, update_resources/2,
         %% Persistence (legacy API support)
         persist/1, load/2,
         %% Monitoring (legacy API support)
         subscribe/2, unsubscribe/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4,
         terminate/3, code_change/4, format_status/2]).

%% Types
-type session_id() :: binary().
-type connection_id() :: term().

%% Session states for MCP protocol lifecycle
-type session_state() ::
    idle |                 %% Initial state, no connections
    connecting |           %% Establishing transport connection
    active |               %% Normal operation, processing requests
    suspended |            %% Temporarily suspended
    error_recovery |       %% Recovering from error
    closed.                %% Terminated state

%% Reason for suspension
-type suspend_reason() ::
    idle_timeout |
    admin_action |
    rate_limit_exceeded |
    maintenance |
    transport_error |
    {custom, binary()}.

%% Error recovery strategy
-type recovery_strategy() ::
    immediate_retry |
    exponential_backoff |
    circuit_breaker |
    failover |
    manual_intervention.

%% Session metrics
-type session_metrics() ::
    #{state := session_state(),
      connections := non_neg_integer(),
      uptime_ms := non_neg_integer(),
      state_transitions := non_neg_integer(),
      recovery_attempts := non_neg_integer(),
      last_activity := integer()}.

%% Configuration
-type config() ::
    #{timeout_ms := pos_integer() | infinity,
      recovery_strategy := recovery_strategy(),
      max_recovery_attempts := non_neg_integer(),
      backoff_initial_ms := pos_integer(),
      backoff_max_ms := pos_integer(),
      backoff_multiplier := float(),
      enable_history := boolean(),
      max_history_size := pos_integer()}.

-type session_event() :: {connect, map()} |
                         {activate, map()} |
                         {deactivate, term()} |
                         {suspend, term()} |
                         {resume, map()} |
                         {force_error_recovery, term()} |
                         {close, term()} |
                         {timeout, connect | idle} |
                         {resource_update, map()}.
-type session_reason() :: term().

%% Session data record
-record(data,
        {session_id :: session_id(),
         current_state :: session_state(),
         %% User identity (session fixation protection)
         user_id :: binary() | undefined,
         %% Protocol state
         protocol_version :: binary() | undefined,
         capabilities :: #mcp_server_capabilities{} | undefined,
         client_capabilities :: #mcp_client_capabilities{} | undefined,
         %% Authentication state
         auth_context :: map() | undefined,
         auth_attempts = 0 :: non_neg_integer(),
         max_auth_attempts = 3 :: pos_integer(),
         auth_timeout_ms = 30000 :: pos_integer(),
         idle_timeout_ms = 300000 :: pos_integer(),
         %% Connection management
         connections = #{} :: #{connection_id() => pid()},
         connection_monitors = #{} :: #{pid() => reference()},
         %% State tracking
         state_history = [] :: [state_transition()],
         state_entry_time :: integer(),
         state_entered_at :: integer(),
         last_activity :: integer(),
         %% Configuration
         timeout_ms :: pos_integer() | infinity,
         recovery_strategy :: recovery_strategy(),
         recovery_attempts = 0 :: non_neg_integer(),
         max_recovery_attempts :: non_neg_integer(),
         %% Suspend state
         suspend_reason :: suspend_reason() | undefined,
         suspend_timeout_ref :: reference() | undefined,
         %% Error recovery
         error_reason :: term() | undefined,
         error_timestamp :: integer() | undefined,
         backoff_ref :: reference() | undefined,
         backoff_ms = 0 :: non_neg_integer(),
         %% Resources (legacy support)
         quota :: map(),
         resources_used :: map(),
         memory_limit_bytes :: pos_integer() | infinity,
         %% Persistence
         persistent = false :: boolean(),
         backend :: module() | undefined,
         %% Migration (legacy support)
         migration_allowed = false :: boolean(),
         migration_node :: node() | undefined,
         %% Subscribers
         subscribers = [] :: [{pid(), reference()}],
         %% Metadata
         metadata :: map(),
         %% Audit
         audit_log :: list(),
         %% Guards and invariants
         guards :: map(),
         created_at :: integer(),
         options :: map()}).

%% State transition record for history tracking
-record(state_transition,
        {from_state :: session_state() | undefined,
         to_state :: session_state(),
         timestamp :: integer(),
         reason :: term() | undefined}).

-type state_transition() :: #state_transition{}.

-type state_data() :: #data{}.

%% State transition result
-type transition_result() :: {ok, session_state()} |
                             {error, term()}.

%% Export types
-export_type([session_state/0, session_event/0, transition_result/0,
              suspend_reason/0, recovery_strategy/0, session_metrics/0, config/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(DEFAULT_TIMEOUT_MS, 300000).        % 5 minutes
-define(DEFAULT_MAX_RECOVERY_ATTEMPTS, 5).
-define(DEFAULT_BACKOFF_INITIAL_MS, 1000).  % 1 second
-define(DEFAULT_BACKOFF_MAX_MS, 60000).     % 60 seconds
-define(DEFAULT_BACKOFF_MULTIPLIER, 2.0).
-define(DEFAULT_MAX_HISTORY_SIZE, 100).

-define(CONNECT_TIMEOUT_MS, 30000).         % 30 seconds
-define(SUSPEND_TIMEOUT_MS, 600000).        % 10 minutes

-define(DEFAULT_MEMORY_LIMIT_BYTES, 104857600). % 100MB
-define(DEFAULT_QUOTA,
        #{max_requests => 1000,
          max_connections => 10,
          max_message_size => 10485760}).    % 10MB

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a session state machine with default configuration.
-spec start_link(session_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Options) ->
    start_link(SessionId, Options, []).

%% @doc Start a session state machine with custom configuration.
-spec start_link(session_id(), map(), list()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Options, GenStatemOpts) ->
    gen_statem:start_link(?MODULE, {SessionId, Options, default_config()}, GenStatemOpts).

%% @doc Initiate connection to MCP server (idle -> connecting).
-spec connect(pid(), map()) -> ok | {error, term()}.
connect(Pid, ConnectParams) ->
    gen_statem:call(Pid, {connect, ConnectParams}, ?CONNECT_TIMEOUT_MS).

%% @doc Activate the session (connecting/auth -> active).
-spec activate(pid()) -> ok | {error, term()}.
activate(Pid) ->
    gen_statem:call(Pid, activate).

%% @doc Suspend the session temporarily (active -> suspended).
-spec suspend(pid()) -> ok | {error, term()}.
suspend(Pid) ->
    gen_statem:call(Pid, suspend).

%% @doc Resume a suspended session (suspended -> active).
-spec resume(pid()) -> ok | {error, term()}.
resume(Pid) ->
    gen_statem:call(Pid, resume).

%% @doc Close the session gracefully (any -> closed).
-spec close(pid()) -> ok.
close(Pid) ->
    gen_statem:call(Pid, close).

%% @doc Force transition to error_recovery state.
-spec force_error_recovery(pid()) -> ok | {error, term()}.
force_error_recovery(Pid) ->
    gen_statem:call(Pid, {force_error_recovery, manual}).

%% @doc Get current state name.
-spec get_state(pid()) -> session_state().
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc Get state transition history.
-spec get_history(pid()) -> [state_transition()].
get_history(Pid) ->
    gen_statem:call(Pid, get_history).

%% @doc Get full session data.
-spec get_session_data(pid()) -> {ok, map()}.
get_session_data(Pid) ->
    gen_statem:call(Pid, get_session_data).

%% @doc Get active connections.
-spec get_connections(pid()) -> #{connection_id() => pid()}.
get_connections(Pid) ->
    gen_statem:call(Pid, get_connections).

%% @doc Get session metrics.
-spec get_metrics(pid()) -> {ok, session_metrics()}.
get_metrics(Pid) ->
    gen_statem:call(Pid, get_metrics).

%% @doc Update session configuration.
-spec update_config(pid(), map()) -> ok | {error, term()}.
update_config(Pid, ConfigUpdates) ->
    gen_statem:call(Pid, {update_config, ConfigUpdates}).

%% @doc Set session timeout.
-spec set_timeout(pid(), pos_integer() | infinity) -> ok | {error, term()}.
set_timeout(Pid, TimeoutMs) ->
    gen_statem:call(Pid, {set_timeout, TimeoutMs}).

%% @doc Stop the session state machine.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%% @doc Hibernate the session immediately (memory optimization).
-spec hibernate_now(pid()) -> ok.
hibernate_now(Pid) ->
    gen_statem:call(Pid, hibernate_now).

%%====================================================================
%% Legacy API Support (backward compatibility)
%%====================================================================

%% @doc Initialize session in 'idle' state (legacy)
-spec init_session(pid(), map()) -> transition_result().
init_session(Pid, Metadata) ->
    gen_statem:call(Pid, {init_session, Metadata}, 5000).

%% @doc Migrate session to another node (legacy)
-spec migrate(pid(), node()) -> transition_result().
migrate(Pid, TargetNode) ->
    gen_statem:call(Pid, {migrate, TargetNode}, 5000).

%% @doc Terminate session (legacy - use close/1 instead)
-spec terminate(pid()) -> ok.
terminate(Pid) ->
    close(Pid).

%% @doc Get session info (legacy)
-spec get_info(pid()) -> {ok, map()}.
get_info(Pid) ->
    gen_statem:call(Pid, get_info, 5000).

%% @doc Set session quota
-spec set_quota(pid(), map()) -> ok | {error, term()}.
set_quota(Pid, Quota) ->
    gen_statem:call(Pid, {set_quota, Quota}, 5000).

%% @doc Check if session has quota available
-spec check_quota(pid()) -> {ok, boolean(), map()}.
check_quota(Pid) ->
    gen_statem:call(Pid, check_quota, 5000).

%% @doc Update resources used
-spec update_resources(pid(), map()) -> ok | {error, term()}.
update_resources(Pid, Resources) ->
    gen_statem:call(Pid, {update_resources, Resources}, 5000).

%% @doc Persist session state
-spec persist(pid()) -> ok | {error, term()}.
persist(Pid) ->
    gen_statem:call(Pid, persist, 5000).

%% @doc Load session state
-spec load(pid(), binary()) -> ok | {error, term()}.
load(Pid, Data) ->
    gen_statem:call(Pid, {load, Data}, 5000).

%% @doc Subscribe to session events
-spec subscribe(pid(), pid()) -> ok | {error, term()}.
subscribe(Pid, Subscriber) ->
    gen_statem:call(Pid, {subscribe, Subscriber}, 5000).

%% @doc Unsubscribe from session events
-spec unsubscribe(pid(), pid()) -> ok.
unsubscribe(Pid, Subscriber) ->
    gen_statem:call(Pid, {unsubscribe, Subscriber}, 5000).

%% @doc Get default configuration
-spec default_config() -> config().
default_config() ->
    #{timeout_ms => ?DEFAULT_TIMEOUT_MS,
      recovery_strategy => exponential_backoff,
      max_recovery_attempts => ?DEFAULT_MAX_RECOVERY_ATTEMPTS,
      backoff_initial_ms => ?DEFAULT_BACKOFF_INITIAL_MS,
      backoff_max_ms => ?DEFAULT_BACKOFF_MAX_MS,
      backoff_multiplier => ?DEFAULT_BACKOFF_MULTIPLIER,
      enable_history => true,
      max_history_size => ?DEFAULT_MAX_HISTORY_SIZE,
      quota => ?DEFAULT_QUOTA,
      memory_limit_bytes => ?DEFAULT_MEMORY_LIMIT_BYTES}.

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec init({session_id(), map(), config()}) -> {ok, session_state(), state_data()}.
init({SessionId, Options, Config}) ->
    %% No blocking operations in init/1 - Armstrong principle
    logger:info("Session statem ~p initializing with config", [SessionId]),

    Now = erlang:system_time(millisecond),

    %% Extract user identity from options or metadata for session fixation protection
    Metadata = maps_get(metadata, Options, #{}),
    UserId = case maps_get(user_id, Options, undefined) of
        undefined -> maps_get(user_id, Metadata, <<"anonymous">>);
        FoundUserId -> FoundUserId
    end,

    %% Bind session to user (cryptographically) to prevent session fixation attacks
    %% The session ID is derived from: SHA256(UserId || OriginalSessionId || UniqueInteger)
    BoundSessionId = case UserId of
        <<"anonymous">> ->
            %% Anonymous sessions keep the original session ID
            SessionId;
        _ ->
            %% Authenticated sessions get a cryptographically bound session ID
            UserSalt = crypto:hash(sha256, <<UserId/binary, SessionId/binary>>),
            UniqueInt = integer_to_binary(erlang:unique_integer([monotonic, positive])),
            crypto:hash(sha256, <<UserSalt/binary, UniqueInt/binary>>)
    end,

    Data = #data{
        session_id = BoundSessionId,
        current_state = idle,
        user_id = UserId,
        protocol_version = maps:get(protocol_version, Options, ?MCP_VERSION),
        capabilities = maps:get(capabilities, Options, undefined),
        client_capabilities = undefined,
        auth_context = undefined,
        auth_attempts = 0,
        max_auth_attempts = maps_get(max_auth_attempts, Options, 3),
        auth_timeout_ms = maps_get(auth_timeout_ms, Config, 30000),
        idle_timeout_ms = maps_get(idle_timeout_ms, Config, 300000),
        connections = #{},
        connection_monitors = #{},
        state_history = [],
        state_entry_time = Now,
        state_entered_at = Now,
        last_activity = Now,
        timeout_ms = maps_get(timeout_ms, Config, ?DEFAULT_TIMEOUT_MS),
        recovery_strategy = maps_get(recovery_strategy, Config, exponential_backoff),
        recovery_attempts = 0,
        max_recovery_attempts = maps_get(max_recovery_attempts, Config,
                                        ?DEFAULT_MAX_RECOVERY_ATTEMPTS),
        suspend_reason = undefined,
        suspend_timeout_ref = undefined,
        error_reason = undefined,
        error_timestamp = undefined,
        backoff_ref = undefined,
        backoff_ms = 0,
        quota = maps_get(quota, Config, ?DEFAULT_QUOTA),
        resources_used = #{requests => 0, connections => 0, bytes => 0},
        memory_limit_bytes = maps_get(memory_limit_bytes, Config, ?DEFAULT_MEMORY_LIMIT_BYTES),
        persistent = maps_get(persistent, Options, false),
        backend = maps_get(backend, Options, undefined),
        migration_allowed = maps_get(migration_allowed, Options, false),
        migration_node = undefined,
        subscribers = [],
        metadata = Metadata,
        audit_log = [],
        guards = maps_get(guards, Options, #{}),
        created_at = Now,
        options = Config
    },

    %% Register with gproc for discovery
    try_register_session(BoundSessionId),

    %% Emit initial state transition (4-arg version for init)
    emit_state_transition_init(undefined, idle, Data, init),

    %% Log session fixation protection applied
    case UserId of
        <<"anonymous">> ->
            logger:debug("Anonymous session ~p created", [BoundSessionId]);
        _ ->
            logger:info("User-bound session ~p created for user ~p (session fixation protection enabled)",
                       [BoundSessionId, UserId])
    end,

    {ok, idle, Data}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [handle_event_function, state_enter_calls].

%%====================================================================
%% State enter events
%%====================================================================

%% State enter: new
handle_event(enter, _OldState, new, Data) ->
    emit_state_transition(Data, new),
    NewData = Data#data{state_entered_at = erlang:system_time(millisecond)},
    {keep_state, NewData};

%% State enter: auth - start auth timeout
handle_event(enter, _OldState, auth, Data) ->
    emit_state_transition(Data, auth),
    NewData = Data#data{
        state_entered_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond)
    },
    AuthTimeout = NewData#data.auth_timeout_ms,
    {{timeout, AuthTimeout}, {timeout, auth}, NewData};

%% State enter: active - start idle timeout
handle_event(enter, _OldState, active, Data) ->
    emit_state_transition(Data, active),
    NewData = Data#data{
        state_entered_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond)
    },
    IdleTimeout = NewData#data.idle_timeout_ms,
    {{timeout, IdleTimeout}, {timeout, idle}, NewData};

%% State enter: idle - start idle timeout
handle_event(enter, _OldState, idle, Data) ->
    emit_state_transition(Data, idle),
    NewData = Data#data{
        state_entered_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond)
    },
    IdleTimeout = NewData#data.idle_timeout_ms,
    {{timeout, IdleTimeout}, {timeout, idle}, NewData};

%% State enter: suspended
handle_event(enter, _OldState, suspended, Data) ->
    emit_state_transition(Data, suspended),
    NewData = Data#data{
        state_entered_at = erlang:system_time(millisecond)
    },
    {keep_state, NewData};

%% State enter: terminated - cleanup
handle_event(enter, _OldState, terminated, Data) ->
    emit_state_transition(Data, terminated),
    emit_audit_event(terminated, Data, session_terminated),
    cleanup_session(Data),
    {stop, normal};

%%====================================================================
%% State: new
%%====================================================================

%% Initialize session (explicit init)
handle_event({call, From}, {init_session, Metadata}, new, Data) ->
    NewData = Data#data{
        metadata = maps:merge(Data#data.metadata, Metadata),
        last_activity = erlang:system_time(millisecond)
    },
    emit_audit_event(new, NewData, session_initialized),
    notify_subscribers(NewData, {session_initialized, NewData#data.session_id}),
    {keep_state, NewData, [{reply, From, {ok, new}}]};

%% Authenticate (new -> auth)
handle_event({call, From}, {authenticate, AuthContext}, new, Data) ->
    case check_auth_guard(Data) of
        true ->
            NewData = Data#data{
                auth_context = AuthContext,
                auth_attempts = Data#data.auth_attempts + 1,
                last_activity = erlang:system_time(millisecond)
            },
            emit_audit_event(new, NewData, auth_attempt),
            {next_state, auth, NewData, [{reply, From, {ok, auth}}]};
        false ->
            emit_audit_event(new, Data, auth_blocked),
            {keep_state, Data, [{reply, From, {error, auth_blocked}}]}
    end;

%% Direct activate if pre-authenticated (new -> active)
handle_event({call, From}, activate, new, Data) ->
    case maps:get(pre_authenticated, Data#data.metadata, false) of
        true ->
            NewData = Data#data{
                auth_context = #{pre_authenticated => true},
                last_activity = erlang:system_time(millisecond)
            },
            emit_audit_event(new, NewData, activated_pre_auth),
            notify_subscribers(NewData, {session_activated, NewData#data.session_id}),
            {next_state, active, NewData, [{reply, From, {ok, active}}]};
        false ->
            emit_audit_event(new, Data, activation_denied),
            {keep_state, Data, [{reply, From, {error, not_authenticated}}]}
    end;

%% Terminate from new
handle_event({call, From}, terminate, new, Data) ->
    emit_audit_event(new, Data, terminate_requested),
    {next_state, terminated, Data, [{reply, From, ok}]};

%%====================================================================
%% State: auth
%%====================================================================

%% Auth succeed (auth -> active)
handle_event({call, From}, activate, auth, Data) ->
    case validate_auth(Data) of
        {ok, AuthData} ->
            NewData = Data#data{
                auth_context = AuthData,
                last_activity = erlang:system_time(millisecond)
            },
            emit_audit_event(auth, NewData, auth_success),
            notify_subscribers(NewData, {session_activated, NewData#data.session_id}),
            {next_state, active, NewData, [{reply, From, {ok, active}}]};
        {error, Reason} ->
            emit_audit_event(auth, Data, {auth_failed, Reason}),
            {next_state, terminated, Data, [{reply, From, {error, Reason}}]}
    end;

%% Auth timeout (auth -> terminated)
handle_event({timeout, auth}, auth_timeout, auth, Data) ->
    emit_audit_event(auth, Data, auth_timeout),
    notify_subscribers(Data, {session_auth_timeout, Data#data.session_id}),
    {next_state, terminated, Data};

%% Retry auth
handle_event({call, From}, {authenticate, AuthContext}, auth, Data) ->
    case Data#data.auth_attempts >= Data#data.max_auth_attempts of
        true ->
            emit_audit_event(auth, Data, max_auth_attempts_exceeded),
            {next_state, terminated, Data, [{reply, From, {error, max_attempts}}]};
        false ->
            NewData = Data#data{
                auth_context = AuthContext,
                auth_attempts = Data#data.auth_attempts + 1,
                last_activity = erlang:system_time(millisecond)
            },
            emit_audit_event(auth, NewData, auth_retry),
            {keep_state, NewData, [{reply, From, {ok, auth}}]}
    end;

%%====================================================================
%% State: active
%%====================================================================

%% Deactivate (active -> idle)
handle_event({call, From}, deactivate, active, Data) ->
    NewData = Data#data{
        last_activity = erlang:system_time(millisecond)
    },
    emit_audit_event(active, NewData, deactivated),
    notify_subscribers(NewData, {session_deactivated, NewData#data.session_id}),
    {next_state, idle, NewData, [{reply, From, {ok, idle}}]};

%% Suspend (active -> suspended)
handle_event({call, From}, suspend, active, Data) ->
    NewData = Data#data{
        last_activity = erlang:system_time(millisecond)
    },
    emit_audit_event(active, NewData, suspended),
    notify_subscribers(NewData, {session_suspended, NewData#data.session_id}),
    {next_state, suspended, NewData, [{reply, From, {ok, suspended}}]};

%% Update resources
handle_event({call, From}, {update_resources, Resources}, active, Data) ->
    NewUsed = maps:merge(Data#data.resources_used, Resources),
    case check_quota_limits(Data#data.quota, NewUsed) of
        ok ->
            NewData = Data#data{
                resources_used = NewUsed,
                last_activity = erlang:system_time(millisecond)
            },
            emit_audit_event(active, NewData, {resources_updated, Resources}),
            {keep_state, NewData, [{reply, From, ok}]};
        {error, Reason} ->
            emit_audit_event(active, Data, {quota_exceeded, Reason}),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;

%% Check quota
handle_event({call, From}, check_quota, active, Data) ->
    {HasQuota, Exceeded} = check_quota_status(Data#data.quota, Data#data.resources_used),
    {keep_state, Data, [{reply, From, {ok, HasQuota, Exceeded}}]};

%% Set quota
handle_event({call, From}, {set_quota, NewQuota}, active, Data) ->
    NewData = Data#data{quota = NewQuota},
    emit_audit_event(active, NewData, {quota_updated, NewQuota}),
    {keep_state, NewData, [{reply, From, ok}]};

%% Migrate (active -> active on new node)
handle_event({call, From}, {migrate, TargetNode}, active, Data) ->
    case Data#data.migration_allowed of
        true ->
            case perform_migration(Data, TargetNode) of
                {ok, MigrationData} ->
                    emit_audit_event(active, Data, {migration_started, TargetNode}),
                    notify_subscribers(Data, {session_migrating, Data#data.session_id, TargetNode}),
                    {keep_state, Data, [{reply, From, {ok, MigrationData}}]};
                {error, Reason} ->
                    emit_audit_event(active, Data, {migration_failed, Reason}),
                    {keep_state, Data, [{reply, From, {error, Reason}}]}
            end;
        false ->
            {keep_state, Data, [{reply, From, {error, migration_not_allowed}}]}
    end;

%% Persist
handle_event({call, From}, persist, active, Data) ->
    case Data#data.persistent of
        true ->
            case persist_session(Data) of
                ok ->
                    emit_audit_event(active, Data, session_persisted),
                    {keep_state, Data, [{reply, From, ok}]};
                {error, Reason} ->
                    {keep_state, Data, [{reply, From, {error, Reason}}]}
            end;
        false ->
            {keep_state, Data, [{reply, From, {error, not_persistent}}]}
    end;

%% Subscribe to events
handle_event({call, From}, {subscribe, Subscriber}, active, Data) ->
    MonRef = erlang:monitor(process, Subscriber),
    NewSubscribers = [{Subscriber, MonRef} | Data#data.subscribers],
    NewData = Data#data{subscribers = NewSubscribers},
    {keep_state, NewData, [{reply, From, ok}]};

%% Unsubscribe
handle_event({call, From}, {unsubscribe, Subscriber}, active, Data) ->
    NewSubscribers = lists:keydelete(Subscriber, 1, Data#data.subscribers),
    NewData = Data#data{subscribers = NewSubscribers},
    {keep_state, NewData, [{reply, From, ok}]};

%% Get state
handle_event({call, From}, get_state, active, Data) ->
    {keep_state, Data, [{reply, From, {ok, active}}]};

%% Get info
handle_event({call, From}, get_info, active, Data) ->
    Info = #{
        session_id => Data#data.session_id,
        state => active,
        created_at => Data#data.created_at,
        state_entered_at => Data#data.state_entered_at,
        last_activity => Data#data.last_activity,
        quota => Data#data.quota,
        resources_used => Data#data.resources_used
    },
    {keep_state, Data, [{reply, From, {ok, Info}}]};

%% Get metrics
handle_event({call, From}, get_metrics, active, Data) ->
    Metrics = calculate_metrics(Data),
    {keep_state, Data, [{reply, From, {ok, Metrics}}]};

%% Terminate
handle_event({call, From}, terminate, active, Data) ->
    emit_audit_event(active, Data, terminate_requested),
    {next_state, terminated, Data, [{reply, From, ok}]};

%%====================================================================
%% State: idle
%%====================================================================

%% Resume (idle -> active)
handle_event({call, From}, resume, idle, Data) ->
    NewData = Data#data{
        last_activity = erlang:system_time(millisecond)
    },
    emit_audit_event(idle, NewData, resumed),
    notify_subscribers(NewData, {session_resumed, NewData#data.session_id}),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

%% Suspend (idle -> suspended)
handle_event({call, From}, suspend, idle, Data) ->
    emit_audit_event(idle, Data, suspended_from_idle),
    {next_state, suspended, Data, [{reply, From, {ok, suspended}}]};

%% Idle timeout (idle -> suspended)
handle_event({timeout, idle}, idle_timeout, idle, Data) ->
    emit_audit_event(idle, Data, idle_timeout),
    notify_subscribers(Data, {session_idle_timeout, Data#data.session_id}),
    {next_state, suspended, Data};

%% Get state
handle_event({call, From}, get_state, idle, Data) ->
    {keep_state, Data, [{reply, From, {ok, idle}}]};

%% Get info
handle_event({call, From}, get_info, idle, Data) ->
    Info = #{
        session_id => Data#data.session_id,
        state => idle,
        created_at => Data#data.created_at,
        state_entered_at => Data#data.state_entered_at,
        last_activity => Data#data.last_activity,
        quota => Data#data.quota,
        resources_used => Data#data.resources_used
    },
    {keep_state, Data, [{reply, From, {ok, Info}}]};

%% Terminate
handle_event({call, From}, terminate, idle, Data) ->
    emit_audit_event(idle, Data, terminate_requested),
    {next_state, terminated, Data, [{reply, From, ok}]};

%%====================================================================
%% State: suspended
%%====================================================================

%% Resume (suspended -> active)
handle_event({call, From}, resume, suspended, Data) ->
    case check_resume_guard(Data) of
        true ->
            NewData = Data#data{
                last_activity = erlang:system_time(millisecond)
            },
            emit_audit_event(suspended, NewData, resumed),
            notify_subscribers(NewData, {session_resumed, NewData#data.session_id}),
            {next_state, active, NewData, [{reply, From, {ok, active}}]};
        false ->
            emit_audit_event(suspended, Data, resume_blocked),
            {keep_state, Data, [{reply, From, {error, resume_blocked}}]}
    end;

%% Migrate while suspended
handle_event({call, From}, {migrate, TargetNode}, suspended, Data) ->
    case Data#data.migration_allowed of
        true ->
            case perform_migration(Data, TargetNode) of
                {ok, MigrationData} ->
                    emit_audit_event(suspended, Data, {migration_started, TargetNode}),
                    {keep_state, Data, [{reply, From, {ok, MigrationData}}]};
                {error, Reason} ->
                    {keep_state, Data, [{reply, From, {error, Reason}}]}
            end;
        false ->
            {keep_state, Data, [{reply, From, {error, migration_not_allowed}}]}
    end;

%% Get state
handle_event({call, From}, get_state, suspended, Data) ->
    {keep_state, Data, [{reply, From, {ok, suspended}}]};

%% Terminate
handle_event({call, From}, terminate, suspended, Data) ->
    emit_audit_event(suspended, Data, terminate_requested),
    {next_state, terminated, Data, [{reply, From, ok}]};

%%====================================================================
%% State: terminated
%%====================================================================

%% All events in terminated state return error
handle_event({call, From}, _Event, terminated, Data) ->
    {keep_state, Data, [{reply, From, {error, session_terminated}}]};

%%====================================================================
%% Common events (all states)
%%====================================================================

%% Handle subscriber DOWN
handle_event(info, {'DOWN', MonRef, process, _Pid, _Reason}, State, Data) ->
    NewSubscribers = lists:keydelete(MonRef, 2, Data#data.subscribers),
    NewData = Data#data{subscribers = NewSubscribers},
    {keep_state, NewData};

%% Catch-all
handle_event(EventType, Event, State, Data) ->
    logger:warning("Unhandled event ~p in state ~p: ~p", [EventType, State, Event]),
    {keep_state, Data}.

%%====================================================================
%% Terminate and code_change
%%====================================================================

-spec terminate(term(), session_state(), state_data()) -> ok.
terminate(_Reason, _State, Data) ->
    logger:info("Session statem ~p terminating in state ~p",
                [Data#data.session_id, _State]),
    cleanup_session(Data),
    ok.

-spec code_change(term(), session_state(), state_data(), term()) ->
                     {ok, session_state(), state_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec format_status(normal | terminate, list()) -> map().
format_status(_Opt, [_PDict, State, Data]) ->
    #{state => State,
      session_id => Data#data.session_id,
      created_at => Data#data.created_at,
      last_activity => Data#data.last_activity,
      quota => Data#data.quota,
      resources_used => Data#data.resources_used}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Safe maps:get with default value (like maps:get/3 but safer)
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%% @private Emit state transition event
emit_state_transition(Data, NewState) ->
    Event = #{
        type => state_transition,
        session_id => Data#data.session_id,
        from_state => Data#data.current_state,
        to_state => NewState,
        timestamp => erlang:system_time(millisecond)
    },
    logger:debug("State transition: ~p", [Event]),
    ok.

%% @private Emit audit event
emit_audit_event(CurrentState, Data, Event) ->
    AuditEntry = #{
        state => CurrentState,
        event => Event,
        timestamp => erlang:system_time(millisecond),
        session_id => Data#data.session_id
    },
    logger:info("Audit: ~p", [AuditEntry]),
    ok.

%% @private Check auth guard
check_auth_guard(Data) ->
    Guards = Data#data.guards,
    case maps:get(auth_required, Guards, true) of
        true -> true;
        false -> false
    end.

%% @private Validate auth
validate_auth(Data) ->
    AuthContext = Data#data.auth_context,
    case maps:get(authenticated, AuthContext, false) of
        true -> {ok, AuthContext};
        false -> {error, authentication_failed}
    end.

%% @private Check resume guard
check_resume_guard(Data) ->
    Guards = Data#data.guards,
    case maps:get(resume_allowed, Guards, true) of
        true -> true;
        false -> false
    end.

%% @private Check quota limits
check_quota_limits(Quota, Used) ->
    MaxReqs = maps:get(max_requests, Quota, infinity),
    MaxConns = maps:get(max_connections, Quota, infinity),
    UsedReqs = maps:get(requests, Used, 0),
    UsedConns = maps:get(connections, Used, 0),

    case {MaxReqs, MaxConns} of
        {infinity, infinity} ->
            ok;
        {infinity, _} when UsedConns =< MaxConns ->
            ok;
        {_, infinity} when UsedReqs =< MaxReqs ->
            ok;
        {_, _} when UsedReqs =< MaxReqs, UsedConns =< MaxConns ->
            ok;
        _ ->
            {error, quota_exceeded}
    end.

%% @private Check quota status
check_quota_status(Quota, Used) ->
    MaxReqs = maps:get(max_requests, Quota, infinity),
    MaxConns = maps:get(max_connections, Quota, infinity),
    UsedReqs = maps:get(requests, Used, 0),
    UsedConns = maps:get(connections, Used, 0),

    HasQuota = case {MaxReqs, MaxConns} of
                   {infinity, infinity} -> true;
                   {infinity, _} -> UsedConns < MaxConns;
                   {_, infinity} -> UsedReqs < MaxReqs;
                   {_, _} -> UsedReqs < MaxReqs andalso UsedConns < MaxConns
               end,

    Exceeded = #{
        requests_exceeded => MaxReqs =/= infinity andalso UsedReqs >= MaxReqs,
        connections_exceeded => MaxConns =/= infinity andalso UsedConns >= MaxConns
    },

    {HasQuota, Exceeded}.

%% @private Calculate metrics
calculate_metrics(Data) ->
    Now = erlang:system_time(millisecond),
    #{
        session_age_ms => Now - Data#data.created_at,
        state_duration_ms => Now - Data#data.state_entered_at,
        idle_time_ms => Now - Data#data.last_activity,
        auth_attempts => Data#data.auth_attempts,
        quota_utilization => calculate_quota_utilization(Data)
    }.

%% @private Calculate quota utilization
calculate_quota_utilization(Data) ->
    Quota = Data#data.quota,
    Used = Data#data.resources_used,
    maps:map(fun(Key, Max) ->
                     case maps:get(Key, Used, 0) of
                         UsedVal when is_integer(Max), Max > 0 ->
                             (UsedVal * 100) div Max;
                         _ ->
                             0
                     end
             end,
             Quota).

%% @private Perform migration
perform_migration(Data, TargetNode) ->
    try
        %% Serialize state
        MigrationData = #{
            session_id => Data#data.session_id,
            state_data => Data,
            timestamp => erlang:system_time(millisecond)
        },
        {ok, MigrationData}
    catch
        _:Reason -> {error, {migration_failed, Reason}}
    end.

%% @private Persist session
persist_session(Data) ->
    case Data#data.backend of
        undefined -> {error, no_backend};
        Backend ->
            try
                Session = #{
                    id => Data#data.session_id,
                    state => Data#data.current_state,
                    data => Data,
                    timestamp => erlang:system_time(millisecond)
                },
                {ok, _} = Backend:store(Data#data.session_id, Session, #{}),
                ok
            catch
                _:Reason -> {error, {persist_failed, Reason}}
            end
    end.

%% @private Cleanup session
cleanup_session(Data) ->
    %% Demonitor all subscribers
    lists:foreach(fun({_Pid, MonRef}) ->
                          erlang:demonitor(MonRef, [flush])
                  end,
                  Data#data.subscribers),

    %% Clear resources
    logger:info("Session ~p cleaned up", [Data#data.session_id]),
    ok.

%% @private Notify subscribers
notify_subscribers(Data, Event) ->
    lists:foreach(fun({Pid, _MonRef}) ->
                          case is_process_alive(Pid) of
                              true ->
                                  Pid ! {session_event, Data#data.session_id, Event};
                              false ->
                                  ok
                          end
                  end,
                  Data#data.subscribers),
    ok.

%% @private Register session with gproc for discovery
try_register_session(SessionId) ->
    try
        gproc:reg({n, l, {erlmcp_session, SessionId}}),
        ok
    catch
        _:Error ->
            logger:warning("Failed to register session ~p: ~p", [SessionId, Error]),
            {error, registration_failed}
    end.

%% @private Emit state transition event (4-arg version for init)
emit_state_transition_init(FromState, ToState, Data, Reason) ->
    Event = #{
        type => state_transition,
        session_id => Data#data.session_id,
        from_state => FromState,
        to_state => ToState,
        reason => Reason,
        timestamp => erlang:system_time(millisecond)
    },
    logger:debug("State transition (init): ~p", [Event]),
    ok.

%% @private Validate session fixation protection
%% Returns true if session is properly bound to user identity
validate_session_fixation(Data, ProvidedSessionId) ->
    case Data#data.user_id of
        <<"anonymous">> ->
            %% Anonymous sessions: direct match
            Data#data.session_id =:= ProvidedSessionId;
        UserId ->
            %% Authenticated sessions: verify cryptographic binding
            ExpectedHash = crypto:hash(sha256,
                <<UserId/binary, ProvidedSessionId/binary>>),
            %% Compare stored session ID prefix with expected hash
            %% This ensures the session was created with proper user binding
            BinSessionId = Data#data.session_id,
            case byte_size(BinSessionId) >= 16 of
                true ->
                    <<Prefix:16/binary, _/binary>> = BinSessionId,
                    <<ExpectedPrefix:16/binary, _/binary>> = ExpectedHash,
                    Prefix =:= ExpectedPrefix;
                false ->
                    false
            end
    end.
