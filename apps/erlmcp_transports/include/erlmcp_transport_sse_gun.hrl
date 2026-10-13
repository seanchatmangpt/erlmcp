%%%-------------------------------------------------------------------
%%% @doc
%%% Header file for erlmcp_transport_sse_gun
%%%
%%% Contains record definitions for SSE state management
%%% using Gun HTTP/2 backend for efficient streaming.
%%% @end
%%%-------------------------------------------------------------------

%% SSE state record for individual stream management
-record(sse_state, {
    transport_id :: binary(),          %% Transport identifier
    mode :: client | server,            %% Transport mode
    gun_pid :: pid() | undefined,      %% Gun process PID
    gun_monitor :: reference() | undefined,  %% Gun process monitor
    session_id :: binary(),            %% Unique session identifier
    client_pid :: pid() | undefined,    %% Client process PID (server mode)
    connect_time :: integer(),         %% Connection timestamp
    last_activity :: integer(),         %% Last activity timestamp
    messages_sent = 0 :: non_neg_integer(),  %% Total messages sent
    bytes_sent = 0 :: non_neg_integer(),     %% Total bytes sent
    bytes_received = 0 :: non_neg_integer(), %% Total bytes received
    retry_count = 0 :: non_neg_integer(),   %% Retry attempt count
    backpressure_state :: inactive | active, %% Backpressure state
    backpressure_timer :: reference() | undefined, %% Backpressure timer
    transport_opts :: map(),           %% Transport options
    stats :: map()                     %% Connection statistics
}).

%% SSE metrics record for monitoring and statistics
-record(sse_metrics, {
    connections_created = 0 :: non_neg_integer(),    %% Total connections created
    connections_closed = 0 :: non_neg_integer(),      %% Total connections closed
    events_sent = 0 :: non_neg_integer(),            %% Total events sent
    events_received = 0 :: non_neg_integer(),       %% Total events received
    bytes_sent = 0 :: non_neg_integer(),            %% Total bytes sent
    bytes_received = 0 :: non_neg_integer(),       %% Total bytes received
    errors = 0 :: non_neg_integer(),                %% Total errors encountered
    last_activity :: integer() | undefined,         %% Last activity timestamp
    avg_response_time = 0.0 :: float(),            %% Average response time
    connection_uptime :: integer() | undefined     %% Connection uptime
}).

%% Pool configuration record
-record(sse_pool_config, {
    pool_name :: atom(),               %% Pool name
    max_connections :: pos_integer(),   %% Maximum connections
    min_connections :: pos_integer(),   %% Minimum connections
    idle_timeout :: timeout(),         %% Connection idle timeout
    health_check_interval :: timeout(),%% Health check interval
    retry_attempts :: pos_integer(),    %% Maximum retry attempts
    enable_metrics :: boolean(),       %% Enable metrics collection
    backpressure_threshold :: pos_integer(), %% Backpressure threshold
    strategy :: round_robin | least_loaded | random %% Load balancing strategy
}).

%% Transport configuration record
-record(sse_transport_config, {
    transport_id :: binary(),          %% Transport identifier
    host :: inet:hostname(),           %% Target hostname
    port :: inet:port_number(),        Target port
    path :: string(),                 SSE endpoint path
    protocol :: http | https,         Protocol scheme
    ssl_options :: [ssl:tls_client_option()], SSL configuration
    timeout :: timeout(),             Operation timeout
    max_message_size :: pos_integer(), Maximum message size
    compression :: boolean(),         Enable compression
    flow_control :: boolean(),         Enable flow control
    authentication :: map(),          Authentication configuration
    headers :: [{binary(), binary()}], Custom headers
}).

%% SSE event record for structured event handling
-record(sse_event, {
    type :: binary(),                  %% Event type
    data :: binary(),                  Event data
    id :: binary() | undefined,        Event ID (for reconnection)
    retry :: pos_integer() | undefined, Retry interval in milliseconds
    timestamp :: integer()            Event timestamp
}).

%% Connection health check result
-record(health_status, {
    connection_id :: binary(),        %% Connection identifier
    status :: healthy | unhealthy | unknown, %% Health status
    last_check :: integer(),           Last check timestamp
    response_time :: integer() | undefined, %% Response time in ms
    error_count :: non_neg_integer(),  Error count
    error_message :: binary() | undefined %% Last error message
}).

%% Stream management records
-record(stream_info, {
    stream_ref :: gun:stream_ref(),   %% Gun stream reference
    state :: active | idle | closed,  %% Stream state
    created :: integer(),             %% Stream creation timestamp
    last_data :: integer() | undefined, Last data timestamp
    message_count :: non_neg_integer(), Message count
    byte_count :: non_neg_integer(),   Byte count
    window_size :: pos_integer()       Flow control window size
}).

%% Backpressure control record
-record(backpressure_info, {
    state :: normal | warning | critical, %% Backpressure state
    current_load :: float(),          Current load percentage
    max_load :: float(),              Maximum allowed load
    active_since :: integer() | undefined, %% When backpressure started
    recovery_timer :: reference() | undefined %% Recovery timer
}).

%% Statistics collection records
-record(connection_stats, {
    total_connections :: non_neg_integer(),    %% Total connections
    successful_connections :: non_neg_integer(), %% Successful connections
    failed_connections :: non_neg_integer(),  %% Failed connections
    average_connection_time :: float(),      %% Average connection time
    max_concurrent_connections :: non_neg_integer(), %% Max concurrent connections
    messages_per_second :: float(),           %% Messages per second
    bytes_per_second :: float(),             %% Bytes per second
    error_rate :: float(),                   %% Error rate (percentage)
    recovery_rate :: float()                %% Recovery rate (percentage)
}).

%% Configuration constants
-define(DEFAULT_SSE_PORT, 8080).
-define(DEFAULT_SSE_SSL_PORT, 8443).
-define(DEFAULT_SSE_PATH, "/sse").
-define(DEFAULT_MAX_CONNECTIONS, 100).
-define(DEFAULT_IDLE_TIMEOUT, 300000).  %% 5 minutes
-define(DEFAULT_HEALTH_CHECK_INTERVAL, 60000). %% 1 minute
-define(DEFAULT_MAX_RETRIES, 5).
-define(DEFAULT_RETRY_TIMEOUT, 5000).
-define(DEFAULT_FLOW_CONTROL_WINDOW, 65535).
-define(DEFAULT_BACKPRESSURE_THRESHOLD, 80). %% 80%
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16MB
-define(DEFAULT_COMPRESSION, true).
-define(DEFAULT_ENABLE_METRICS, true).
-define(DEFAULT_LOAD_BALANCING_STRATEGY, round_robin).

%% SSE protocol constants
-define(SSE_DATA_PREFIX, "data: ").
-define(SSE_EVENT_PREFIX, "event: ").
-define(SSE_ID_PREFIX, "id: ").
-define(SSE_RETRY_PREFIX, "retry: ").
-define(SSE_COMMENT_PREFIX, ": ").
-define(SSE_SEPARATOR, "\n").
-define(SSE_DOUBLE_SEPARATOR, "\n\n").

%% Gun HTTP/2 constants
-define(DEFAULT_GUN_RETRY, 3).
-define(DEFAULT_GUN_TRACE, false).
-define(DEFAULT_HTTP2_MAX_STREAMS, 100).
-define(DEFAULT_HTTP2_WINDOW_SIZE, 65535).

%% Error codes
-define(SSE_ERROR_CONNECTION_FAILED, 1001).
-define(SSE_ERROR_STREAM_RESET, 1002).
-define(SSE_ERROR_TIMEOUT, 1003).
-define(SSE_ERROR_BACKPRESSURE, 1004).
-define(SSE_ERROR_PROTOCOL_VIOLATION, 1005).
-define(SSE_ERROR_MESSAGE_TOO_LARGE, 1006).