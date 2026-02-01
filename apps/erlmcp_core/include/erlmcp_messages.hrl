-ifndef(ERLMCP_MESSAGES_HRL).
-define(ERLMCP_MESSAGES_HRL, 1).

%%% ====================================================================
%%% Control Plane Priority Messages
%%% ====================================================================
%%%
%%% These messages MUST be handled with priority, bypassing normal
%%% message queue depth to ensure system responsiveness under load.
%%%
%%% Priority levels:
%%%   1. Circuit breaker (immediate - system protection)
%%%   2. Health check (SLO: <100ms even under 100K msg/s)
%%%   3. Drain session (preempts all data traffic)
%%%   4. Cancel task (bypasses queue depth)
%%%
%%% Usage: send as {control, MessageType, Data} tuple
%%% ====================================================================

%% Priority message wrapper
-type control_message() ::
    {control, health_check, health_check_data()} |
    {control, drain_session, drain_session_data()} |
    {control, cancel_task, cancel_task_data()} |
    {control, circuit_breaker, circuit_breaker_data()}.

-export_type([control_message/0]).

%%% ====================================================================
%%% Circuit Breaker (Priority Level 1 - IMMEDIATE)
%%% ====================================================================
%%% Immediate response required to prevent cascading failures
%%% Must bypass ALL queue depth limits
%%% ====================================================================

-type circuit_breaker_action() :: open | close | half_open | query.
-type circuit_breaker_data() :: #{
    action := circuit_breaker_action(),
    component := atom(),           % Component ID (e.g., http_transport, tool_executor)
    reason => term(),               % Why circuit opened
    timestamp => integer(),         % When event occurred
    from => pid() | reference()     % Reply destination
}.

%%% ====================================================================
%%% Health Check (Priority Level 2 - SLO <100ms)
%%% ====================================================================
%%% Must complete within 100ms even under extreme load (100K msg/s)
%%% Used by orchestration systems (k8s, systemd, etc)
%%% ====================================================================

-type health_check_type() :: liveness | readiness | full.
-type health_check_data() :: #{
    type := health_check_type(),
    timeout_ms => pos_integer(),    % Max wait time (default: 100ms)
    from => pid() | reference(),    % Reply destination
    requested_at => integer()       % Timestamp for latency tracking
}.

%%% ====================================================================
%%% Drain Session (Priority Level 3 - PREEMPTS DATA)
%%% ====================================================================
%%% Gracefully shut down session, preempting all normal data traffic
%%% No new requests accepted, existing requests complete or timeout
%%% ====================================================================

-type drain_reason() :: shutdown | maintenance | overload | error.
-type drain_session_data() :: #{
    session_id := binary() | atom(),
    reason := drain_reason(),
    timeout_ms => pos_integer(),    % Max time to wait for completion (default: 5000ms)
    force => boolean(),             % Force immediate termination if timeout exceeded
    from => pid() | reference()     % Reply destination
}.

%%% ====================================================================
%%% Cancel Task (Priority Level 4 - BYPASS QUEUE)
%%% ====================================================================
%%% Cancel long-running operation, bypassing queue depth
%%% Must be delivered even if process mailbox is at capacity
%%% ====================================================================

-type cancel_task_data() :: #{
    task_id := term(),              % Request ID or task identifier
    reason => term(),               % Cancellation reason
    timeout_ms => pos_integer(),    % Max wait for cancellation (default: 1000ms)
    from => pid() | reference()     % Reply destination
}.

%%% ====================================================================
%%% Priority Message Metadata
%%% ====================================================================
%%% Used by control plane to track priority message delivery
%%% ====================================================================

-type priority_level() :: 1..4.
-type priority_metadata() :: #{
    level := priority_level(),
    queued_at := integer(),         % Timestamp when queued (microseconds)
    delivered_at => integer(),      % Timestamp when delivered (microseconds)
    latency_us => non_neg_integer() % Delivery latency in microseconds
}.

%%% ====================================================================
%%% Control Plane Statistics
%%% ====================================================================
%%% Metrics for monitoring priority message handling
%%% ====================================================================

-type control_plane_stats() :: #{
    total_delivered := non_neg_integer(),
    by_type := #{
        health_check := non_neg_integer(),
        drain_session := non_neg_integer(),
        cancel_task := non_neg_integer(),
        circuit_breaker := non_neg_integer()
    },
    latency_p50_us := non_neg_integer(),
    latency_p95_us := non_neg_integer(),
    latency_p99_us := non_neg_integer(),
    max_latency_us := non_neg_integer(),
    slo_violations := non_neg_integer()  % Health checks > 100ms
}.

-endif.
