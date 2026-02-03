%%%-------------------------------------------------------------------
%%% @include
%%% Header file for erlmcp_load_testing application
%%%-------------------------------------------------------------------

%% Load Test Configuration Types
-type load_test_id() :: binary().
-type load_test_config() :: #{
    protocol => http | websocket | sse | tcp,
    target_endpoint => string(),
    target_host => string(),
    target_port => pos_integer(),
    user_count => pos_integer(),
    duration => pos_integer(),
    ramp_up_time => pos_integer(),
    think_time => pos_integer(),
    request_rate => pos_integer(),
    payload_size => pos_integer(),
    auth_config => map(),
    headers => map(),
    custom_scenarios => [map()],
    error_rate_threshold => float(),
    latency_threshold => pos_integer(),
    throughput_threshold => pos_integer(),
    connection_pool_size => pos_integer(),
    follow_redirects => boolean(),
    tls_options => map()
}.

-type load_test_status() :: #{
    id => load_test_id(),
    status => running | paused | completed | failed,
    start_time => pos_integer(),
    progress => float(),
    current_users => pos_integer(),
    current_rate => pos_integer(),
    estimated_completion => pos_integer(),
    warnings => [binary()],
    errors => [binary()]
}.

%% Metrics Types
-type metrics() :: #{
    timestamp => pos_integer(),
    throughput => #{
        requests_per_second => float(),
        successful_requests => pos_integer(),
        failed_requests => pos_integer(),
        success_rate => float()
    },
    latency => #{
        average => float(),
        p50 => float(),
        p90 => float(),
        p95 => float(),
        p99 => float(),
        max => float()
    },
    resource_usage => #{
        cpu => float(),
        memory => float(),
        connections => pos_integer(),
        file_descriptors => pos_integer()
    },
    network => #{
        bytes_sent => pos_integer(),
        bytes_received => pos_integer(),
        packets_sent => pos_integer(),
        packets_received => pos_integer()
    },
    errors => #{
        connection_errors => pos_integer(),
        timeout_errors => pos_integer(),
        protocol_errors => pos_integer(),
        other_errors => pos_integer()
    }
}.

%% Benchmark Types
-type benchmark_type() :: throughput | latency | scalability | resource_usage | mixed.
-type benchmark_result() :: #{
    type => benchmark_type(),
    duration => pos_integer(),
    results => metrics(),
    analysis => map(),
    recommendations => [binary()]
}.

%% Stress Test Types
-type stress_test_config() :: #{
    max_users => pos_integer(),
    max_rate => pos_integer(),
    ramp_up_interval => pos_integer(),
    duration_at_peak => pos_integer(),
    degrade_point => float(),
    recovery_point => float(),
    test_phases => [stress_phase()]
}.

-type stress_phase() :: #{
    phase_name => binary(),
    user_count => pos_integer(),
    request_rate => pos_integer(),
    duration => pos_integer(),
    ramp_up => pos_integer(),
    think_time => pos_integer()
}.

%% Load Generator Types
-type load_generator_config() :: #{
    protocol => atom(),
    target => string(),
    concurrency => pos_integer(),
    rate => pos_integer(),
    payload => binary(),
    headers => map(),
    auth => map()
}.

-type load_generator_stats() :: #{
    active => pos_integer(),
    completed => pos_integer(),
    failed => pos_integer(),
    in_progress => pos_integer(),
    avg_latency => float(),
    p95_latency => float(),
    p99_latency => float()
}.

%% Performance Analysis Types
-type performance_profile() :: #{
    normal => metrics(),
    peak => metrics(),
    threshold => metrics(),
    regression => {pos_integer(), metrics(), metrics()}
}.

-type bottleneck_type() :: cpu | memory | network | disk | connection | protocol.

-type bottleneck_info() :: #{
    type => bottleneck_type(),
    severity => low | medium | high | critical,
    current_value => float(),
    threshold => float(),
    impact => binary(),
    recommendations => [binary()]
}.

%% Database Load Testing Types
-type db_config() :: #{
    type => ets | dets | mnesia | pgsql | mysql,
    connection_string => string(),
    pool_size => pos_integer(),
    query_timeout => pos_integer(),
    batch_size => pos_integer()
}.

-type db_load_metrics() :: #{
    queries_per_second => float(),
    average_query_time => float(),
    connection_pool_usage => float(),
    query_success_rate => float(),
    memory_usage => float()
}.

%% System Constants
-define(DEFAULT_USER_COUNT, 1000).
-define(DEFAULT_DURATION, 300).  % 5 minutes
-define(DEFAULT_RAMP_UP, 60).     % 1 minute
-defineDEFAULT_THINK_TIME, 1000). % 1 second
-define(DEFAULT_RATE, 10).        % 10 requests/second
-define(DEFAULT_PAYLOAD_SIZE, 1024).
-define(DEFAULT_POOL_SIZE, 100).
-define(DEFAULT_ERROR_THRESHOLD, 0.05).  % 5%
-define(DEFAULT_LATENCY_THRESHOLD, 5000). % 5 seconds
-define(DEFAULT_THROUGHPUT_THRESHOLD, 1000). % 1000 req/s

%% Protocol Constants
-define(HTTP_TIMEOUT, 30000).
-define(WEB_SOCKET_TIMEOUT, 30000).
-define(SSE_TIMEOUT, 30000).
-define(TCP_TIMEOUT, 30000).

%% Performance Thresholds
-define(CPU_THRESHOLD, 80.0).     % 80% CPU usage
-define(MEMORY_THRESHOLD, 80.0).  % 80% memory usage
-define(NETWORK_THRESHOLD, 100).  % 100 Mbps
-define(CONNECTION_THRESHOLD, 90). % 90% connection usage

%% Test Status Codes
-define(TEST_STATUS_RUNNING, running).
-define(TEST_STATUS_PAUSED, paused).
-define(TEST_STATUS_COMPLETED, completed).
-define(TEST_STATUS_FAILED, failed).
-define(TEST_STATUS_CANCELLED, cancelled).

%% Error Codes
-define(ERROR_CONFIG_INVALID, 400).
-define(ERROR_TEST_NOT_FOUND, 404).
-define(ERROR_TEST_RUNNING, 409).
-define(ERROR_RESOURCE_LIMIT, 429).
-define(ERROR_INTERNAL_SERVER_ERROR, 500).