%%%-------------------------------------------------------------------
%%% @doc SwarmFlow OS Top-Level Supervisor
%%%
%%% Tier-1 supervisor managing core SwarmFlow OS services.
%%% Uses one_for_one restart strategy to allow independent restarts.
%%%
%%% Architecture (start order matters for dependencies):
%%% 1. swf_net_registry     - Workflow net definition storage (dependency: none)
%%% 2. swf_event_log        - Event log for process mining (dependency: none)
%%% 3. swf_case_sup         - Supervisor for case processes (dependency: net_registry)
%%% 4. swf_swarm_coordinator - Process mining swarm (dependency: event_log)
%%% 5. swf_promotion_engine - Autonomic patch promotion (dependency: net_registry, event_log)
%%% 6. swf_a2a_bridge       - A2A protocol bridge (dependency: case_sup)
%%% 7. swf_mcp_bridge       - MCP protocol bridge (dependency: case_sup)
%%%
%%% Restart Strategy:
%%% - one_for_one: Each child can restart independently
%%% - This allows case execution to continue if event_log restarts
%%% - Critical services (net_registry) should be started first
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swarmflow_os_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits for top-level supervisor
%% More conservative than child supervisors
-define(MAX_RESTARTS, 5).
-define(RESTART_PERIOD, 60).  % seconds

%%--------------------------------------------------------------------
%% @doc Start the top-level supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @private
%% @doc Supervisor callback
%%
%% Initializes the supervision tree with all SwarmFlow OS components.
%% Order is important - dependencies must start before dependents.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => ?MAX_RESTARTS,
        period => ?RESTART_PERIOD,
        auto_shutdown => never
    },

    %% Core workflow net registry - stores validated workflow definitions
    %% Must start first as other components depend on it
    NetRegistry = #{
        id => swf_net_registry,
        start => {swf_net_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [swf_net_registry]
    },

    %% Event log service - append-only event log for process mining
    %% Critical for replay, conformance checking, and auditing
    EventLog = #{
        id => swf_event_log,
        start => {swf_event_log, start_link, []},
        restart => permanent,
        shutdown => 10000,  % Allow time to flush pending events
        type => worker,
        modules => [swf_event_log]
    },

    %% Case supervisor - simple_one_for_one for dynamic workflow cases
    %% Each workflow instance runs under this supervisor
    CaseSup = #{
        id => swf_case_sup,
        start => {swf_case_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % Supervisor - wait for children
        type => supervisor,
        modules => [swf_case_sup]
    },

    %% Swarm coordinator - manages process mining workers
    %% Coordinates conformance checkers, pattern miners, anomaly detectors
    SwarmCoordinator = #{
        id => swf_swarm_coordinator,
        start => {swf_swarm_coordinator, start_link, [#{}]},
        restart => permanent,
        shutdown => 10000,  % Allow time for workers to complete
        type => worker,
        modules => [swf_swarm_coordinator]
    },

    %% Promotion engine - autonomic workflow improvement
    %% Evaluates and promotes patch proposals based on policies
    PromotionEngine = #{
        id => swf_promotion_engine,
        start => {swf_promotion_engine, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [swf_promotion_engine]
    },

    %% Build child spec list, conditionally including optional bridges
    ChildSpecs = build_child_specs([
        NetRegistry,
        EventLog,
        CaseSup,
        SwarmCoordinator,
        PromotionEngine
    ]),

    {ok, {SupFlags, ChildSpecs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Build the final child specs list, adding optional modules if present
%%--------------------------------------------------------------------
-spec build_child_specs([supervisor:child_spec()]) -> [supervisor:child_spec()].
build_child_specs(BaseSpecs) ->
    %% Add A2A bridge if module exists
    Specs1 = maybe_add_child(swf_a2a_bridge, a2a_bridge_spec(), BaseSpecs),

    %% Add MCP bridge if module exists
    Specs2 = maybe_add_child(swf_mcp_bridge, mcp_bridge_spec(), Specs1),

    Specs2.

%%--------------------------------------------------------------------
%% @private
%% @doc Add a child spec if the module is available
%%--------------------------------------------------------------------
-spec maybe_add_child(module(), supervisor:child_spec(), [supervisor:child_spec()]) ->
    [supervisor:child_spec()].
maybe_add_child(Module, ChildSpec, Specs) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Specs ++ [ChildSpec];
        {error, _} ->
            %% Module not available - skip
            Specs
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Child spec for A2A bridge (optional)
%% Bridges SwarmFlow workflows with A2A agent protocol
%%--------------------------------------------------------------------
-spec a2a_bridge_spec() -> supervisor:child_spec().
a2a_bridge_spec() ->
    #{
        id => swf_a2a_bridge,
        start => {swf_a2a_bridge, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [swf_a2a_bridge]
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Child spec for MCP bridge (optional)
%% Bridges SwarmFlow workflows with MCP tools/resources
%%--------------------------------------------------------------------
-spec mcp_bridge_spec() -> supervisor:child_spec().
mcp_bridge_spec() ->
    #{
        id => swf_mcp_bridge,
        start => {swf_mcp_bridge, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [swf_mcp_bridge]
    }.
