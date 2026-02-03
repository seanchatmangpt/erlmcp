%%%-------------------------------------------------------------------
%%% @doc
%%% Connection State Machine Records and Type Definitions
%%%
%%% This header file defines the state records and types used by
%%% erlmcp_connection_fsm for managing transport connection lifecycles.
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_CONNECTION_STATE_HRL).

-define(ERLMCP_CONNECTION_STATE_HRL, true).

%% Connection states
-type conn_state() ::
    disconnected      |  %% Initial state, not connected
    connecting        |  %% Actively attempting to connect
    connected         |  %% Successfully connected
    ready             |  %% Connected and ready for data transfer
    reconnecting      |  %% Attempting to reconnect after failure
    disconnecting     |  %% Gracefully shutting down
    failed            |  %% Connection failed, will not retry
    closed.           %% Connection closed permanently.

%% Transport types
-type transport_type() ::
    stdio             |
    tcp               |
    http              |
    http2             |
    ws                |  %% WebSocket
    sse               |  %% Server-Sent Events
    tls.              %% TLS/TCP

%% Backoff strategy
-type backoff_strategy() ::
    exponential       |
    linear            |
    fixed             |
    jitter            |
    decorrelated_jitter.

%% Circuit breaker state
-type circuit_state() ::
    closed            |  %% Normal operation
    open              |  %% Failing, reject requests
    half_open.        %% Testing if recovered

%% Flow control state
-type flow_control_state() ::
    normal            |
    throttled         |
    backpressure      |
    paused.

%% Connection metrics
-record(conn_metrics,
        {connect_count = 0 :: non_neg_integer(),
         disconnect_count = 0 :: non_neg_integer(),
         bytes_sent = 0 :: non_neg_integer(),
         bytes_received = 0 :: non_neg_integer(),
         messages_sent = 0 :: non_neg_integer(),
         messages_received = 0 :: non_neg_integer(),
         connection_errors = 0 :: non_neg_integer(),
         last_connect_time :: integer() | undefined,  %% Unix timestamp
         last_disconnect_time :: integer() | undefined,
         uptime_ms = 0 :: non_neg_integer(),
         latency_ms = 0 :: non_neg_integer()}).

-type conn_metrics() :: #conn_metrics{}.

%% Circuit breaker data
-record(circuit_breaker,
        {state = closed :: circuit_state(),
         failure_count = 0 :: non_neg_integer(),
         success_count = 0 :: non_neg_integer(),
         threshold = 5 :: pos_integer(),      %% Failures before opening
         timeout = 60000 :: pos_integer(),    %% ms to stay open
         half_open_max_calls = 3 :: pos_integer(),
         last_failure_time :: integer() | undefined,
         last_state_change :: integer() | undefined}).

-type circuit_breaker() :: #circuit_breaker{}.

%% Backoff configuration
-record(backoff_config,
        {strategy = exponential :: backoff_strategy(),
         initial_delay = 1000 :: pos_integer(),    %% 1 second
         max_delay = 60000 :: pos_integer(),       %% 60 seconds
         multiplier = 2.0 :: float(),             %% For exponential
         jitter = 0.1 :: float(),                 %% 10% jitter
         max_attempts = infinity :: pos_integer() | infinity}).

-type backoff_config() :: #backoff_config{}.

%% Flow control configuration
-record(flow_control,
        {state = normal :: flow_control_state(),
         window_size = 65536 :: pos_integer(),     %% bytes
         current_window = 65536 :: pos_integer(),
         high_watermark = 0.8 :: float(),         %% 80%
         low_watermark = 0.5 :: float(),          %% 50%
         pause_threshold = 0.9 :: float(),        %% 90%
         resume_threshold = 0.4 :: float()}).     %% 40%

-type flow_control() :: #flow_control{}.

%% Keep-alive configuration
-record(keepalive_config,
        {enabled = true :: boolean(),
         interval = 30000 :: pos_integer(),        %% 30 seconds
         timeout = 10000 :: pos_integer(),         %% 10 seconds
         max_missed = 3 :: pos_integer()}).

-type keepalive_config() :: #keepalive_config{}.

%% Connection pool configuration
-record(pool_config,
        {size = 10 :: pos_integer(),
         max_overflow = 5 :: non_neg_integer(),
         strategy = round_robin :: round_robin | least_loaded | random,
         lease_timeout = 30000 :: pos_integer(),   %% 30 seconds
         idle_timeout = 60000 :: pos_integer()}).  %% 60 seconds

-type pool_config() :: #pool_config{}.

%% Health check configuration
-record(health_config,
        {enabled = true :: boolean(),
         interval = 30000 :: pos_integer(),        %% 30 seconds
         timeout = 5000 :: pos_integer(),          %% 5 seconds
         failure_threshold = 3 :: pos_integer(),   %% Failures before marking unhealthy
         success_threshold = 2 :: pos_integer()}). %% Successes before marking healthy

-type health_config() :: #health_config{}.

%% Connection FSM state
-record(conn_fsm_state,
        {id :: atom() | binary(),
         transport_type :: transport_type(),
         current_state :: conn_state(),
         owner :: pid() | undefined,

         %% Connection parameters
         host :: inet:hostname() | inet:ip_address() | undefined,
         port :: inet:port_number() | undefined,
         options = [] :: list(),

         %% Socket/connection handle
         socket :: term() | undefined,
         transport_pid :: pid() | undefined,

         %% Timers
         reconnect_timer :: reference() | undefined,
         keepalive_timer :: reference() | undefined,
         health_timer :: reference() | undefined,
         lease_timer :: reference() | undefined,

         %% Reconnection state
         reconnect_attempts = 0 :: non_neg_integer(),
         backoff_config :: backoff_config(),

         %% Circuit breaker
         circuit_breaker :: circuit_breaker(),

         %% Flow control
         flow_control :: flow_control(),

         %% Keep-alive
         keepalive_config :: keepalive_config(),

         %% Health monitoring
         health_config :: health_config(),
         health_status = healthy :: healthy | unhealthy | unknown,
         consecutive_failures = 0 :: non_neg_integer(),
         consecutive_successes = 0 :: non_neg_integer(),

         %% Pool management
         pool_config :: pool_config(),
         pool_name :: atom() | undefined,
         leased_by :: pid() | undefined,

         %% Metrics
         metrics :: conn_metrics(),

         %% Data buffering
         send_buffer = queue:new() :: queue:queue(),
         send_buffer_size = 0 :: non_neg_integer(),
         max_buffer_size = 1048576 :: pos_integer(), %% 1MB

         %% Multiplexing
         multiplexed = false :: boolean(),
         multiplex_id :: reference() | undefined,
         parent_connection :: pid() | undefined,

         %% Context for error recovery
         last_error :: term() | undefined,
         error_count = 0 :: non_neg_integer(),

         %% Monitoring
         monitor_refs = #{} :: #{pid() => reference()},

         %% Timestamps
         connect_start_time :: integer() | undefined,
         state_enter_time :: integer() | undefined}).

-type conn_fsm_state() :: #conn_fsm_state{}.

%% Connection events
-type conn_event() ::
    {connect, map()}                                    |
    {connected, term()}                                  |
    {disconnect, term()}                                 |
    {reconnect, term()}                                  |
    {send, iodata()}                                     |
    {data, binary()}                                     |
    {error, term()}                                      |
    timeout                                              |
    {keepalive, timeout}                                 |
    {health_check, result}                               |
    {circuit_breaker, circuit_state()}                  |
    {flow_control, flow_control_state()}                 |
    {lease_timeout, pid()}                               |
    {owner_down, term()}                                 |
    {transport_down, term()}                             |
    {backpressure, boolean()}                            |
    {pool_lease, pid(), timeout()}                       |
    {pool_release, pid()}.

%% Connection FSM responses
-type conn_fsm_response() ::
    {next_state, conn_state(), conn_fsm_state()}                      |
    {next_state, conn_state(), conn_fsm_state(), [action()]}          |
    {keep_state, conn_fsm_state()}                                     |
    {keep_state, conn_fsm_state(), [action()]}                         |
    {stop, term(), conn_fsm_state()}.

-type action() ::
    {reply, term()}                                                   |
    {reply, term(), non_neg_integer() | infinity}                     |
    {reply, gen_statem:from(), term()}                                |
    {state_timeout, non_neg_integer() | infinity, term()}             |
    {timeout, non_neg_integer() | infinity}                           |
    {hibernate, non_neg_integer() | infinity}                         |
    {event_timeout, non_neg_integer() | infinity}                     |
    {postpone, boolean()}.

-endif.
