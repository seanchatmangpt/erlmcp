%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_observability_sup - Observability Subsystem Supervisor
%%%
%%% Top-level supervisor for the observability layer in erlmcp v3.
%%% Manages metrics, tracing, health monitoring, and SLA tracking.
%%%
%%% == Supervision Strategy ==
%%% Strategy: one_for_one
%%% - Isolated failures prevent cascading restarts
%%% - Intensity: 10 restarts within 60 seconds
%%%
%%% == Core Observability Components ==
%%% - erlmcp_metrics_collector: Prometheus-compatible metrics
%%% - erlmcp_tracer: OTP 28 distributed tracing
%%% - erlmcp_prometheus_exporter: HTTP /metrics endpoint
%%% - erlmcp_health_monitor: Component health tracking
%%% - erlmcp_sla_monitor: Service Level Agreement monitoring
%%%
%%% == Dependency Graph ==
%%% ```
%%% prometheus_exporter -> metrics_collector
%%% prometheus_exporter -> tracer
%%% sla_monitor -> metrics_collector
%%% sla_monitor -> health_monitor
%%% opentelemetry -> tracer
%%% opentelemetry -> metrics_collector
%%% ```
%%%
%%% == Generated from ggen Ontology ==
%%% Instance: ggen/ontology/instances/observability_supervisor.ttl
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%% Child IDs - matching ontology instance definitions
-define(CHILD_METRICS_COLLECTOR, erlmcp_metrics_collector).
-define(CHILD_TRACER, erlmcp_tracer).
-define(CHILD_PROMETHEUS_EXPORTER, erlmcp_prometheus_exporter).
-define(CHILD_HEALTH_MONITOR, erlmcp_health_monitor).
-define(CHILD_SLA_MONITOR, erlmcp_sla_monitor).
-define(CHILD_EVENT_MANAGER, erlmcp_event_manager).
-define(CHILD_OTEL, erlmcp_otel).

%%====================================================================
%% Type Definitions
%%====================================================================

-type child_id() :: ?CHILD_METRICS_COLLECTOR |
                    ?CHILD_TRACER |
                    ?CHILD_PROMETHEUS_EXPORTER |
                    ?CHILD_HEALTH_MONITOR |
                    ?CHILD_SLA_MONITOR |
                    ?CHILD_EVENT_MANAGER |
                    ?CHILD_OTEL |
                    atom().

-type restart() :: permanent | transient | temporary.
-type shutdown() :: 5000 | infinity | pos_integer().
-type child_type() :: worker | supervisor.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the observability supervisor with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the observability supervisor with options
%% Options:
%%   - enable_otel: boolean() - Enable OpenTelemetry integration (default: false)
%%   - prometheus_port: pos_integer() - Prometheus exporter port (default: 9090)
%%   - health_check_interval: pos_integer() - Health check interval in ms (default: 30000)
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Initialize the supervisor with observability workers
%% Returns child specs in dependency order - children with no
%% dependencies start first.
%%
%% == Supervision Flags ==
%% - Strategy: one_for_one - each child restarted independently
%% - Intensity: 10 - max restarts before shutdown
%% - Period: 60 - time window for restart intensity
%%
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Opts) ->
    ?LOG_INFO("Initializing observability supervisor with options: ~p", [Opts]),

    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 60},

    %% Build child specs in dependency order
    Children = child_specs(Opts),

    ?LOG_INFO("Starting ~p observability children", [length(Children)]),

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Build child specifications in dependency order.
%% Level 1: No dependencies
%% Level 2: Depends on Level 1
%% Level 3: Depends on Level 1-2
-spec child_specs(list()) -> [supervisor:child_spec()].
child_specs(Opts) ->
    EnableOtel = proplists:get_value(enable_otel, Opts, false),

    %% Level 1: Core components with no dependencies
    Level1 = [
        child_spec_event_manager(),
        child_spec_metrics_collector(),
        child_spec_tracer(),
        child_spec_health_monitor()
    ],

    %% Level 2: Components depending on Level 1
    Level2 = [
        child_spec_prometheus_exporter(Opts),
        child_spec_sla_monitor()
    ],

    %% Level 3: Optional components depending on Level 1-2
    Level3 =
        case EnableOtel of
            true -> [child_spec_otel()];
            false -> []
        end,

    Level1 ++ Level2 ++ Level3.

%%====================================================================
%% Child Specification Builders
%%====================================================================

%% @private
%% Event Manager - gen_event based event handling
%% Must start before other observability components that emit events
-spec child_spec_event_manager() -> supervisor:child_spec().
child_spec_event_manager() ->
    #{id => ?CHILD_EVENT_MANAGER,
      start => {erlmcp_event_manager, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_event_manager]}.

%% @private
%% Metrics Collector - Prometheus-compatible metrics collection
%% Provides counter, gauge, histogram metric types
-spec child_spec_metrics_collector() -> supervisor:child_spec().
child_spec_metrics_collector() ->
    #{id => ?CHILD_METRICS_COLLECTOR,
      start => {erlmcp_metrics_collector, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_metrics_collector]}.

%% @private
%% Distributed Tracer - OTP 28 trace:system/3 based tracing
%% Manages trace sessions for tool invocation chains
-spec child_spec_tracer() -> supervisor:child_spec().
child_spec_tracer() ->
    #{id => ?CHILD_TRACER,
      start => {erlmcp_tracer, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_tracer]}.

%% @private
%% Health Monitor - Component health tracking with circuit breaker integration
%% Supports OTP 28 priority messages for urgent health updates
-spec child_spec_health_monitor() -> supervisor:child_spec().
child_spec_health_monitor() ->
    #{id => ?CHILD_HEALTH_MONITOR,
      start => {erlmcp_health_monitor, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_health_monitor]}.

%% @private
%% Prometheus Exporter - HTTP server for metrics scraping
%% Depends on: metrics_collector, tracer
-spec child_spec_prometheus_exporter(list()) -> supervisor:child_spec().
child_spec_prometheus_exporter(Opts) ->
    Port = proplists:get_value(prometheus_port, Opts, 9090),
    Config = #{port => Port, path => "/metrics"},
    #{id => ?CHILD_PROMETHEUS_EXPORTER,
      start => {erlmcp_prometheus_exporter, start_link, [Config]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_prometheus_exporter]}.

%% @private
%% SLA Monitor - Service Level Agreement monitoring
%% Tracks: uptime, response_time, error_rate, throughput, session_success
%% Depends on: metrics_collector, health_monitor
-spec child_spec_sla_monitor() -> supervisor:child_spec().
child_spec_sla_monitor() ->
    #{id => ?CHILD_SLA_MONITOR,
      start => {erlmcp_sla_monitor, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_sla_monitor]}.

%% @private
%% OpenTelemetry Integration (optional)
%% Supports exporters: console, datadog, honeycomb, jaeger
%% Depends on: tracer, metrics_collector
-spec child_spec_otel() -> supervisor:child_spec().
child_spec_otel() ->
    #{id => ?CHILD_OTEL,
      start => {erlmcp_otel, init, []},
      restart => transient,  % Don't restart if not supported
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_otel]}.
