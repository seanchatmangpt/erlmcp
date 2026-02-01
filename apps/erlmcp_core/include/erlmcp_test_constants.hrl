%%%-------------------------------------------------------------------
%%% @doc erlmcp_test_constants - Test Constants and Fixtures
%%%
%%% This header file defines all constants used across test suites,
%%% including timeouts, thresholds, test configurations, and fixture data.
%%%
%%% Usage in test files:
%%%   -include_lib("erlmcp_core/include/erlmcp_test_constants.hrl").
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_TEST_CONSTANTS_HRL).

-define(ERLMCP_TEST_CONSTANTS_HRL, 1).

%%====================================================================
%% Test Timeout Constants
%%====================================================================

%% Default timeout for test operations
-define(TEST_TIMEOUT_DEFAULT, 5000).           % 5 seconds
-define(TEST_TIMEOUT_SHORT, 1000).             % 1 second
-define(TEST_TIMEOUT_MEDIUM, 10000).           % 10 seconds
-define(TEST_TIMEOUT_LONG, 30000).             % 30 seconds
-define(TEST_TIMEOUT_EXTENDED, 60000).         % 60 seconds
%% Operation-specific timeouts
-define(TEST_TIMEOUT_INIT, 5000).              % Initialization timeout
-define(TEST_TIMEOUT_CALL, 3000).              % Tool call timeout
-define(TEST_TIMEOUT_RESOURCE_READ, 3000).     % Resource read timeout
-define(TEST_TIMEOUT_PROMPT_GET, 3000).        % Prompt get timeout
-define(TEST_TIMEOUT_NOTIFICATION, 2000).      % Notification delivery timeout

%%====================================================================
%% Test Load and Concurrency Constants
%%====================================================================

%% Concurrency levels for load testing
-define(TEST_CONCURRENT_CLIENTS, 50).          % Default concurrent client count
-define(TEST_CONCURRENT_CLIENTS_LOW, 10).      % Low concurrency
-define(TEST_CONCURRENT_CLIENTS_HIGH, 100).    % High concurrency
-define(TEST_CONCURRENT_CLIENTS_STRESS, 500).  % Stress test concurrency
%% Message burst sizes
-define(TEST_MESSAGE_BURST_SIZE, 100).         % Default burst size
-define(TEST_MESSAGE_BURST_SMALL, 10).         % Small burst
-define(TEST_MESSAGE_BURST_LARGE, 1000).       % Large burst
%% Load test durations
-define(TEST_LOAD_DURATION_SHORT, 5000).       % 5 seconds
-define(TEST_LOAD_DURATION_MEDIUM, 10000).     % 10 seconds
-define(TEST_LOAD_DURATION_LONG, 30000).       % 30 seconds

%%====================================================================
%% Test Server and Transport Naming
%%====================================================================

%% Server process name prefixes
-define(TEST_SERVER_PREFIX, integration_test_server).
-define(TEST_TRANSPORT_PREFIX, integration_test_transport).
-define(TEST_CLIENT_PREFIX, integration_test_client).
-define(TEST_REGISTRY_PREFIX, test_registry).
%% Pool name prefixes
-define(TEST_POOL_PREFIX, test_pool).
-define(TEST_WORKER_PREFIX, test_worker).

%%====================================================================
%% Test Port and Network Constants
%%====================================================================

%% Test ports for various services
-define(TEST_PORT_MIN, 8000).                  % Minimum test port
-define(TEST_PORT_MAX, 9999).                  % Maximum test port
-define(TEST_PORT_DEFAULT, 9091).              % Default dashboard/test port
-define(TEST_PORT_HTTP, 9092).                 % HTTP transport test port
-define(TEST_PORT_TCP, 9093).                  % TCP transport test port
-define(TEST_PORT_WS, 9094).                   % WebSocket test port
%% Network test addresses
-define(TEST_HOST_LOCALHOST, "localhost").
-define(TEST_HOST_IPV4, "127.0.0.1").
-define(TEST_HOST_IPV6, "::1").

%%====================================================================
%% Test Message and Payload Constants
%%====================================================================

%% Default test payloads
-define(TEST_PAYLOAD_EMPTY, <<>>).
-define(TEST_PAYLOAD_JSON, <<"{}">>).
-define(TEST_PAYLOAD_TEXT, <<"test">>).
%% Message size limits for testing
-define(TEST_MAX_MESSAGE_SIZE, 16777216).      % 16 MB
-define(TEST_MAX_MESSAGE_SMALL, 1024).         % 1 KB
-define(TEST_MAX_MESSAGE_MEDIUM, 1048576).     % 1 MB
-define(TEST_MAX_MESSAGE_LARGE, 10485760).     % 10 MB

%%====================================================================
%% Test Resource and Tool Constants
%%====================================================================

%% Test resource URIs
-define(TEST_RESOURCE_URI_1, <<"test://resource1">>).
-define(TEST_RESOURCE_URI_2, <<"test://resource2">>).
-define(TEST_RESOURCE_URI_FILE, <<"file:///tmp/test.txt">>).
-define(TEST_RESOURCE_URI_HTTP, <<"http://example.com/resource">>).
%% Test tool names
-define(TEST_TOOL_NAME_1, <<"echo">>).
-define(TEST_TOOL_NAME_2, <<"reverse">>).
-define(TEST_TOOL_NAME_3, <<"calculator">>).
-define(TEST_TOOL_NAME_INVALID, <<"nonexistent_tool">>).
%% Test prompt names
-define(TEST_PROMPT_NAME_1, <<"greeting">>).
-define(TEST_PROMPT_NAME_2, <<"summary">>).
-define(TEST_PROMPT_NAME_INVALID, <<"nonexistent_prompt">>).

%%====================================================================
%% Test Session and Authentication Constants
%%====================================================================

%% Test session IDs
-define(TEST_SESSION_ID_1, <<"test_session_001">>).
-define(TEST_SESSION_ID_2, <<"test_session_002">>).
-define(TEST_SESSION_ID_HEX, <<"0123456789abcdef0123456789abcdef">>).
%% Test authentication tokens
-define(TEST_AUTH_TOKEN_VALID, <<"valid_token_12345">>).
-define(TEST_AUTH_TOKEN_EXPIRED, <<"expired_token_54321">>).
-define(TEST_AUTH_TOKEN_INVALID, <<"invalid_token_99999">>).

%%====================================================================
%% Test Threshold and Limit Constants
%%====================================================================

%% Rate limiting thresholds
-define(TEST_RATE_LIMIT_PER_SEC, 100).         % 100 requests/second
-define(TEST_RATE_LIMIT_PER_MIN, 5000).        % 5000 requests/minute
-define(TEST_RATE_LIMIT_BURST, 200).           % Burst allowance
%% Queue limits
-define(TEST_QUEUE_MAX_MESSAGES, 1000).        % Max messages in queue
-define(TEST_QUEUE_MAX_BYTES, 10485760).       % 10 MB queue byte limit
%% Connection limits
-define(TEST_MAX_CONNECTIONS, 100).            % Max concurrent connections
-define(TEST_MAX_CONNECTIONS_PER_IP, 10).      % Max per IP
%% Resource limits
-define(TEST_MAX_RESOURCES, 1000).             % Max resources
-define(TEST_MAX_TOOLS, 100).                  % Max tools
-define(TEST_MAX_PROMPTS, 100).                % Max prompts

%%====================================================================
%% Test Memory and Performance Constants
%%====================================================================

%% Memory thresholds (in bytes)
-define(TEST_MEMORY_THRESHOLD_LOW, 1048576).   % 1 MB
-define(TEST_MEMORY_THRESHOLD_MEDIUM, 10485760). % 10 MB
-define(TEST_MEMORY_THRESHOLD_HIGH, 104857600).  % 100 MB
-define(TEST_MEMORY_PAYLOAD_MAX, 16777216).    % 16 MB max payload
%% Performance thresholds
-define(TEST_LATENCY_THRESHOLD_MS, 100).       % 100ms latency threshold
-define(TEST_THROUGHPUT_MIN, 1000).            % Min 1000 ops/sec
-define(TEST_CPU_THRESHOLD, 80).               % 80% CPU threshold

%%====================================================================
%% Test Error and Validation Constants
%%====================================================================

%% Expected error codes for validation
-define(TEST_ERROR_PARSE, -32700).
-define(TEST_ERROR_INVALID_REQUEST, -32600).
-define(TEST_ERROR_METHOD_NOT_FOUND, -32601).
-define(TEST_ERROR_INVALID_PARAMS, -32602).
-define(TEST_ERROR_INTERNAL, -32603).
%% Test error messages
-define(TEST_ERROR_MSG_INVALID, <<"test_invalid_error">>).
-define(TEST_ERROR_MSG_TIMEOUT, <<"test_timeout">>).
-define(TEST_ERROR_MSG_NOT_FOUND, <<"test_not_found">>).

%%====================================================================
%% Test Retry and Backoff Constants
%%====================================================================

%% Retry configuration
-define(TEST_RETRY_MAX_ATTEMPTS, 3).
-define(TEST_RETRY_DELAY_MS, 100).
-define(TEST_RETRY_BACKOFF_MULTIPLIER, 2).
%% Exponential backoff calculation
-define(TEST_RETRY_BACKOFF(Attempt), ?TEST_RETRY_DELAY_MS * trunc(math:pow(2, Attempt - 1))).

%%====================================================================
%% Test Cleanup and Teardown Constants
%%====================================================================

%% Cleanup timeouts
-define(TEST_CLEANUP_TIMEOUT, 5000).           % Cleanup operation timeout
-define(TEST_SHUTDOWN_TIMEOUT, 10000).         % Shutdown timeout
-define(TEST_FLUSH_TIMEOUT, 3000).             % Log/buffer flush timeout
%% Cleanup intervals
-define(TEST_CLEANUP_INTERVAL_MS, 100).        % Cleanup check interval
-define(TEST_RETRY_INTERVAL_MS, 500).          % Retry interval

%%====================================================================
%% Test Assertion and Validation Helpers
%%====================================================================

%% NOTE: assertMatch and assertNotMatch are provided by EUnit
%% We don't redefine them here to avoid conflicts
%% Use ?assertMatch and ?assertNotMatch from eunit.hrl instead

%%====================================================================
%% Test Logging and Debug Constants
%%====================================================================

%% Test log levels
-define(TEST_LOG_LEVELS, [debug, info, notice, warning, error, critical, alert, emergency]).
-define(TEST_LOG_LEVEL_DEFAULT, info).
%% Test log categories
-define(TEST_LOG_CATEGORY_API, <<"api">>).
-define(TEST_LOG_CATEGORY_TRANSPORT, <<"transport">>).
-define(TEST_LOG_CATEGORY_PROTOCOL, <<"protocol">>).
-define(TEST_LOG_CATEGORY_PERF, <<"performance">>).

%%====================================================================
%% Test Fixture Data
%%====================================================================

%% Sample JSON-RPC requests
-define(TEST_JSONRPC_REQUEST_INIT,
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"id">> => 1,
          <<"method">> => <<"initialize">>,
          <<"params">> =>
              #{<<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> =>
                    #{<<"name">> => <<"test_client">>, <<"version">> => <<"1.0.0">>}}}).
-define(TEST_JSONRPC_REQUEST_TOOLS_LIST,
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"id">> => 2,
          <<"method">> => <<"tools/list">>,
          <<"params">> => #{}}).
-define(TEST_JSONRPC_REQUEST_TOOL_CALL,
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"id">> => 3,
          <<"method">> => <<"tools/call">>,
          <<"params">> =>
              #{<<"name">> => <<"echo">>, <<"arguments">> => #{<<"message">> => <<"hello">>}}}).
%% Sample capabilities
-define(TEST_CAPABILITIES_ALL,
        #{<<"resources">> => #{},
          <<"tools">> => #{},
          <<"prompts">> => #{},
          <<"logging">> => #{},
          <<"roots">> => #{}}).
%% Sample tool definitions
-define(TEST_TOOL_ECHO,
        #{<<"name">> => <<"echo">>,
          <<"description">> => <<"Echoes the input message">>,
          <<"inputSchema">> =>
              #{<<"type">> => <<"object">>,
                <<"properties">> => #{<<"message">> => #{<<"type">> => <<"string">>}},
                <<"required">> => [<<"message">>]}}).

%%====================================================================
%% Test Stress and Chaos Constants
%%====================================================================

%% Stress test configuration
-define(TEST_STRESS_DURATION_MS, 60000).       % 1 minute stress test
-define(TEST_STRESS_OPS_PER_SEC, 10000).       % Target ops/sec
-define(TEST_STRESS_MAX_ERRORS, 10).           % Max errors before abort
%% Chaos test failure injection rates
-define(TEST_CHAOS_CRASH_RATE, 0.01).          % 1% crash rate
-define(TEST_CHAOS_FAILURE_RATE, 0.05).        % 5% failure rate
-define(TEST_CHAOS_LATENCY_MS, 1000).          % 1000ms injected latency

%%====================================================================
%% Benchmark Constants
%%====================================================================

%% Benchmark workloads
-define(TEST_BENCH_WARMUP_ITERS, 100).         % Warmup iterations
-define(TEST_BENCH_MEASURE_ITERS, 1000).       % Measurement iterations
-define(TEST_BENCH_MIN_DURATION_MS, 5000).     % Min benchmark duration
%% Benchmark thresholds
-define(TEST_BENCH_MAX_REGRESSION, 10).        % 10% max regression
-define(TEST_BENCH_MIN_IMPROVEMENT, 5).        % 5% min improvement to note

%%====================================================================
%% File System Test Constants
%%====================================================================

%% Test file paths
-define(TEST_FILE_DIR, "/tmp/erlmcp_test").
-define(TEST_FILE_PATH, "/tmp/erlmcp_test/test.txt").
-define(TEST_FILE_TEMP, "/tmp/erlmcp_test_temp").
%% Test file sizes
-define(TEST_FILE_SIZE_SMALL, 1024).           % 1 KB
-define(TEST_FILE_SIZE_MEDIUM, 1048576).       % 1 MB
-define(TEST_FILE_SIZE_LARGE, 10485760).       % 10 MB

%%====================================================================
%% Metrology and Metrics Constants
%%====================================================================

%% Metric collection intervals
-define(TEST_METRICS_COLLECTION_INTERVAL_MS, 1000). % 1 second
-define(TEST_METRICS_RETENTION_SEC, 3600).     % Retain for 1 hour
%% Metric thresholds
-define(TEST_METRIC_THROUGHPUT_MIN, 100).      % Min 100 ops/sec
-define(TEST_METRIC_LATENCY_P99_MAX_MS, 500).  % P99 latency < 500ms
-define(TEST_METRIC_ERROR_RATE_MAX, 0.01).     % Max 1% error rate

%%====================================================================
%% Deprecated/Legacy Constants (for backward compatibility)
%%====================================================================

%% @deprecated Use TEST_TIMEOUT_DEFAULT instead
-define(TIMEOUT, 5000).
%% @deprecated Use TEST_CONCURRENT_CLIENTS instead
-define(CONCURRENT_CLIENTS, 50).
%% @deprecated Use TEST_PORT_DEFAULT instead
-define(PORT, 9091).

-endif.
