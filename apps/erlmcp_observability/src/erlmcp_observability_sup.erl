%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_observability_sup - Observability Subsystem Supervisor
%%%
%%% Manages observability infrastructure in isolated supervision tree.
%%% Failures in observability do not affect core MCP protocol operation.
%%%
%%% Supervision Strategy: one_for_one
%%% - Metrics server crash: restart independently
%%% - Health monitor crash: restart independently
%%% - Recovery manager crash: restart independently
%%% - Chaos framework crash: restart independently
%%% - Receipt chain is ETS-based (survives restarts)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the observability supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the observability supervisor with options
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Initialize the supervisor with observability workers
%% Strategy: one_for_one - isolated failures, no cascading restarts
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Opts) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,      % Max 10 restarts
          period => 60},          % Within 60 seconds

    %% Child specifications
    Children =
        [%% Event Manager - gen_event based event handling
         %% Must start before other observability components that emit events
         #{id => erlmcp_event_manager,
           start => {erlmcp_event_manager, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_event_manager]},
         %% Metrics Server - Core metrics collection
         #{id => erlmcp_metrics,
           start => {erlmcp_metrics, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_metrics]},
         %% Metrics HTTP Server - HTTP metrics endpoint
         #{id => erlmcp_metrics_server,
           start => {erlmcp_metrics_server, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_metrics_server]},
         %% Metrics Aggregator - Time-series aggregation and percentiles
         #{id => erlmcp_metrics_aggregator,
           start => {erlmcp_metrics_aggregator, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_metrics_aggregator]},
         %% Dashboard Server - Real-time metrics dashboard (Cowboy + WebSocket)
         #{id => erlmcp_dashboard_server,
           start => {erlmcp_dashboard_server, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_dashboard_server]},
         %% Health Monitor - Component health tracking
         #{id => erlmcp_health_monitor,
           start => {erlmcp_health_monitor, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_health_monitor]},
         %% Recovery Manager - Automatic recovery and circuit breakers
         #{id => erlmcp_recovery_manager,
           start => {erlmcp_recovery_manager, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_recovery_manager]},
         %% OTP 28 Native Debugger (optional, only if +D flag is set)
         %% This is a transient worker - it may fail if +D flag is not set
         #{id => erlmcp_otp_debugger,
           start => {erlmcp_otp_debugger, start_link, []},
           restart => transient,      % Don't restart if not supported
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_otp_debugger]},
         %% Chaos Engineering Framework - Resilience testing
         #{id => erlmcp_chaos,
           start => {erlmcp_chaos, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_chaos]},
         %% Chaos Worker Supervisor - Supervises chaos experiment workers
         #{id => erlmcp_chaos_worker_sup,
           start => {erlmcp_chaos_worker_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_chaos_worker_sup]},
         %% Process Monitor - Process count monitoring and capacity planning
         #{id => erlmcp_process_monitor,
           start => {erlmcp_process_monitor, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_process_monitor]},
         %% OTP 28 Tracer - Distributed tracing with trace:system/3
         %% Uses OTP 28 trace sessions for tool invocation chains
         #{id => erlmcp_tracer,
           start => {erlmcp_tracer, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_tracer]},
         %% Audit Log - Tamper-proof audit trail with hash chain
         %% Critical: Maintains compliance trail for GDPR, SOC2, HIPAA
         #{id => erlmcp_audit_log,
           start => {erlmcp_audit_log, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_audit_log]}],

    %% Note: Receipt chain (erlmcp_receipt_chain) is ETS-based with no process
    %% Note: OTEL (erlmcp_otel) is a library module with no supervision
    %% Note: Evidence path (erlmcp_evidence_path) is a library module
    %% Note: Chaos primitives are library modules (no supervision needed)
    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal Functions
%%====================================================================
