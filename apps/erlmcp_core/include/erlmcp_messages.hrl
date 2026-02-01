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
-type circuit_breaker_data() ::
    #{action := circuit_breaker_action(),
      component := atom(),
      reason => term(),
      timestamp => integer(),
      from => pid() | reference()}.

                                   % Component ID (e.g., http_transport, tool_executor)
               % Why circuit opened

                                    % When event occurred
     % Reply destination

%%% ====================================================================
%%% Health Check (Priority Level 2 - SLO <100ms)
%%% ====================================================================
%%% Must complete within 100ms even under extreme load (100K msg/s)
%%% Used by orchestration systems (k8s, systemd, etc)
%%% ====================================================================

-type health_check_type() :: liveness | readiness | full.
-type health_check_data() ::
    #{type := health_check_type(),
      timeout_ms => pos_integer(),
      from => pid() | reference(),
      requested_at => integer()}.

                                    % Max wait time (default: 100ms)
    % Reply destination

                                    % Timestamp for latency tracking

%%% ====================================================================
%%% Drain Session (Priority Level 3 - PREEMPTS DATA)
%%% ====================================================================
%%% Gracefully shut down session, preempting all normal data traffic
%%% No new requests accepted, existing requests complete or timeout
%%% ====================================================================

-type drain_reason() :: shutdown | maintenance | overload | error.
-type drain_session_data() ::
    #{session_id := binary() | atom(),
      reason := drain_reason(),
      timeout_ms => pos_integer(),
      force => boolean(),
      from => pid() | reference()}.

                                    % Max time to wait for completion (default: 5000ms)
             % Force immediate termination if timeout exceeded

                                    % Reply destination

%%% ====================================================================
%%% Cancel Task (Priority Level 4 - BYPASS QUEUE)
%%% ====================================================================
%%% Cancel long-running operation, bypassing queue depth
%%% Must be delivered even if process mailbox is at capacity
%%% ====================================================================

-type cancel_task_data() ::
    #{task_id := term(),
      reason => term(),
      timeout_ms => pos_integer(),
      from => pid() | reference()}.

                                                 % Request ID or task identifier

                                    % Cancellation reason
    % Max wait for cancellation (default: 1000ms)

                                    % Reply destination

%%% ====================================================================
%%% Priority Message Metadata
%%% ====================================================================
%%% Used by control plane to track priority message delivery
%%% ====================================================================

-type priority_level() :: 1..4.
-type priority_metadata() ::
    #{level := priority_level(),
      queued_at := integer(),
      delivered_at => integer(),
      latency_us => non_neg_integer()}.

                                    % Timestamp when queued (microseconds)
      % Timestamp when delivered (microseconds)

                                    % Delivery latency in microseconds

%%% ====================================================================
%%% Control Plane Statistics
%%% ====================================================================
%%% Metrics for monitoring priority message handling
%%% ====================================================================

-type control_plane_stats() ::
    #{total_delivered := non_neg_integer(),
      by_type :=
          #{health_check := non_neg_integer(),
            drain_session := non_neg_integer(),
            cancel_task := non_neg_integer(),
            circuit_breaker := non_neg_integer()},
      latency_p50_us := non_neg_integer(),
      latency_p95_us := non_neg_integer(),
      latency_p99_us := non_neg_integer(),
      max_latency_us := non_neg_integer(),
      slo_violations := non_neg_integer()}.

                                         % Health checks > 100ms

-endif.
