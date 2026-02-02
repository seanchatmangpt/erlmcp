%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_observability_supervisor - Observability Supervisor
%%%
%%% OTP supervisor for managing observability infrastructure.
%%% Provides OTP 28.3.1 compliant supervision with isolation patterns.
%%%
%%% Supervision Strategy: one_for_one
%%% - Each observability component fails independently
%%% - No cascading restarts (isolated fault domains)
%%% - Automatic restart with intensity/period limits
%%%
%%% Child Processes:
%%% - Event Manager - gen_event based event handling
%%% - Metrics - Collection and aggregation
%%% - Health Monitor - Component health tracking
%%% - Dashboard - Real-time observability UI
%%% - Recovery Manager - Automatic recovery
%%% - Chaos - Failure injection and resilience testing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, start_link/2]).

%% Supervisor callbacks (also exported for testing)
-export([init/1, child_spec/5]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_INTENSITY, 10).
-define(DEFAULT_PERIOD, 60).

%%====================================================================
%% Type Definitions
%%====================================================================

-type opts() :: [{intensity, pos_integer()} |
                 {period, pos_integer()}].
-type child_id() :: atom().
-type child_spec() :: #{id := child_id(),
                       start := {module(), atom(), [term()]},
                       restart := permanent | transient,
                       shutdown := non_neg_integer() | infinity,
                       type := worker | supervisor,
                       modules := [module() | dynamic]}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the observability supervisor with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the observability supervisor with options
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    start_link(Opts, ?SERVER).

%% @doc Start the observability supervisor with custom name
-spec start_link(opts(), atom()) -> {ok, pid()} | {error, term()}.
start_link(Opts, Name) when is_atom(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, Opts).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Initialize the supervisor with observability children
%%
%% Strategy: one_for_one - isolated failures, no cascading restarts
%%
%% OTP Pattern: init/1 MUST NEVER BLOCK
%% - All children must start quickly (< 5s)
%% - Use async initialization for slow setup
%% - Return {ok, {SupFlags, ChildSpecs}} immediately
-spec init(opts()) -> {ok, {supervisor:sup_flags(), [child_spec()]}}.
init(Opts) ->
    %% Extract restart strategy options
    Intensity = proplists:get_value(intensity, Opts, ?DEFAULT_INTENSITY),
    Period = proplists:get_value(period, Opts, ?DEFAULT_PERIOD),

    %% Supervisor flags - one_for_one for isolated restarts
    SupFlags = #{
        strategy => one_for_one,
        intensity => Intensity,
        period => Period
    },

    %% Build child specifications
    Children = build_child_specs(Opts),

    ?LOG_INFO("Observability supervisor init: strategy=one_for_one, "
              "intensity=~p, period=~p, children=~p",
              [Intensity, Period, length(Children)]),

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Build child specifications for observability components
-spec build_child_specs(opts()) -> [child_spec()].
build_child_specs(_Opts) ->
    %% Build child specs in order (start order matters)
    [
        %% ================================================================
        %% Event Manager - gen_event based event handling
        %% ================================================================
        %% Must start before other observability components that emit events
        child_spec(erlmcp_event_manager, worker, permanent, 5000,
                   [erlmcp_event_manager]),

        %% ================================================================
        %% Metrics Core - Metrics collection and aggregation
        %% ================================================================
        child_spec(erlmcp_metrics, worker, permanent, 5000,
                   [erlmcp_metrics]),

        %% ================================================================
        %% Metrics HTTP Server - HTTP metrics endpoint
        %% ================================================================
        child_spec(erlmcp_metrics_server, worker, permanent, 5000,
                   [erlmcp_metrics_server]),

        %% ================================================================
        %% Metrics Aggregator - Time-series aggregation and percentiles
        %% ================================================================
        child_spec(erlmcp_metrics_aggregator, worker, permanent, 5000,
                   [erlmcp_metrics_aggregator]),

        %% ================================================================
        %% Dashboard Server - Real-time metrics dashboard
        %% ================================================================
        %% Cowboy + WebSocket based dashboard
        child_spec(erlmcp_dashboard_server, worker, permanent, 5000,
                   [erlmcp_dashboard_server]),

        %% ================================================================
        %% Health Monitor - Component health tracking
        %% ================================================================
        child_spec(erlmcp_health_monitor, worker, permanent, 5000,
                   [erlmcp_health_monitor]),

        %% ================================================================
        %% Recovery Manager - Automatic recovery and circuit breakers
        %% ================================================================
        child_spec(erlmcp_recovery_manager, worker, permanent, 5000,
                   [erlmcp_recovery_manager]),

        %% ================================================================
        %% Chaos Engineering Framework - Resilience testing
        %% ================================================================
        child_spec(erlmcp_chaos, worker, permanent, 5000,
                   [erlmcp_chaos]),

        %% ================================================================
        %% Chaos Worker Supervisor - Dynamic chaos experiment workers
        %% ================================================================
        %% simple_one_for_one strategy for transient experiment workers
        #{id => erlmcp_chaos_worker_sup,
          start => {erlmcp_chaos_worker_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_chaos_worker_sup]},

        %% ================================================================
        %% Process Monitor - Process count monitoring and capacity planning
        %% ================================================================
        child_spec(erlmcp_process_monitor, worker, permanent, 5000,
                   [erlmcp_process_monitor]),

        %% ================================================================
        %% Audit Log - Tamper-proof audit trail with hash chain
        %% ================================================================
        %% Critical: Maintains compliance trail for GDPR, SOC2, HIPAA
        child_spec(erlmcp_audit_log, worker, permanent, 5000,
                   [erlmcp_audit_log])
    ].

%% @doc Build a child specification map
-spec child_spec(atom(), worker | supervisor, permanent | transient,
                 non_neg_integer(), [module()]) -> child_spec().
child_spec(Id, Type, Restart, Shutdown, Modules) ->
    #{
        id => Id,
        start => {Id, start_link, []},
        restart => Restart,
        shutdown => Shutdown,
        type => Type,
        modules => Modules
    }.
