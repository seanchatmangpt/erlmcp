%%%-------------------------------------------------------------------
%%% @doc
%%% Header file for monitoring metrics module
%%% @end
%%%-------------------------------------------------------------------

-ifndef(erlmcp_monitoring_metrics).
-define(erlmcp_monitoring_metrics, true).

%%====================================================================
%% Record Definitions
%%====================================================================

%% Metric definition record
-record(metric_def, {
    name :: binary(),             % Metric name (e.g., "request_duration")
    type :: counter | gauge | histogram,  % Metric type
    description :: binary(),     % Human-readable description
    tags :: [binary()],           % Allowed tag keys
    registered_at :: integer(),   % Registration timestamp
    cardinality_limit :: integer() | undefined  % High cardinality limit
}).

%% Metric data record
-record(metric_data, {
    name :: binary(),
    type :: counter | gauge | histogram,
    value :: number(),
    tags :: map(),
    timestamp :: integer(),
    labels :: [binary()]        % Formatted Prometheus labels
}).

%% Security event record
-record(security_event, {
    timestamp :: integer(),
    event_type :: binary(),
    severity :: low | medium | high | critical,
    source :: binary(),
    details :: map(),
    tags :: map()
}).

%% SLA metrics record
-record(sla_metrics, {
    uptime :: float(),           % Uptime percentage
    response_time :: float(),    % Average response time in ms
    error_rate :: float(),       % Error rate percentage
    throughput :: integer(),     % Requests per second
    timestamp :: integer()
}).

%% Capacity metrics record
-record(capacity_metrics, {
    cpu_usage :: float(),        % CPU usage percentage
    memory_usage :: float(),     % Memory usage percentage
    disk_usage :: float(),       % Disk usage percentage
    network_io :: map(),         % Network I/O metrics
    connection_count :: integer(), % Current connections
    timestamp :: integer()
}).

%% Business metrics record
-record(business_metrics, {
    active_users :: integer(),   % Active users count
    tool_calls :: integer(),     % Tool calls count
    session_duration :: float(), % Average session duration
    conversion_rate :: float(),  % Conversion rate
    revenue_impact :: float(),   % Revenue impact
    timestamp :: integer()
}).

%% Export format types
-export_type([
    metric_def/0,
    metric_data/0,
    security_event/0,
    sla_metrics/0,
    capacity_metrics/0,
    business_metrics/0
]).

%%====================================================================
%% Constants
%%====================================================================

%% Metric namespaces
-define(METRIC_NAMESPACE_CORE, "erlmcp_core").
-define(METRIC_NAMESPACE_TRANSPORT, "erlmcp_transport").
-define(METRIC_NAMESPACE_SESSION, "erlmcp_session").
-define(METRIC_NAMESPACE_SECURITY, "erlmcp_security").
-define(METRIC_NAMESPACE_BUSINESS, "erlmcp_business").
-define(METRIC_NAMESPACE_COMPLIANCE, "erlmcp_compliance").

%% Standard tags
-define(TAG_TRANSPORT, <<"transport">>).
-define(TAG_PROTOCOL, <<"protocol">>).
-define(TAG_STATUS, <<"status">>).
-define(TAG_ENDPOINT, <<"endpoint">>).
-define(TAG_METHOD, <<"method">>).
-define(TAG_SERVICE, <<"service">>).
-define(TAG_VERSION, <<"version">>).
-define(TAG_ENVIRONMENT, <<"environment">>).

%% Security event types
-define(EVENT_AUTH_FAILURE, "auth_failure").
-define(EVENT_AUTH_SUCCESS, "auth_success").
-define(EVENT_PERMISSION_DENIED, "permission_denied").
-define(EVENT_RATE_LIMIT_EXCEEDED, "rate_limit_exceeded").
-define(EVENT_SUSPICIOUS_ACTIVITY, "suspicious_activity").
-define(EVENT_DATA_BREACH, "data_breach").

%% SLA thresholds
-define(SLA_UPTIME_MIN, 99.9).           % 99.9% uptime
-define(SLA_RESPONSE_TIME_MAX, 100).     % 100ms max response time
-define(SLA_ERROR_RATE_MAX, 0.01).       % 1% max error rate

-endif.