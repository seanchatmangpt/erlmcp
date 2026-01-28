%%%-----------------------------------------------------------------------------
%%% @doc TCPS Top-Level Supervisor
%%%
%%% Simplified supervision tree for TCPS core components.
%%% Only includes modules with start_link/0 and gen_server behaviors.
%%%
%%% Included:
%%%   - tcps_work_order (work order management)
%%%   - tcps_kanban (WIP limits)
%%%   - tcps_quality_gates (8 quality gates)
%%%   - tcps_sku (SKU release management)
%%%   - tcps_dashboard (web dashboard)
%%%   - tcps_sse_manager (SSE broadcasting)
%%%   - tcps_metrics_aggregator (metrics collection)
%%%
%%% Strategy: one_for_one (independent workers)
%%% Intensity: 5 restarts in 60 seconds
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_erlmcp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%==============================================================================
%% API
%%==============================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

-spec init(Args :: term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent workers, don't restart all on single failure
        intensity => 5,
        period => 60
    },

    %% Layer 1: Work Order Management
    WorkOrderSpec = #{
        id => tcps_work_order,
        start => {tcps_work_order, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_work_order]
    },

    %% Layer 2: Flow Control - Kanban
    KanbanSpec = #{
        id => tcps_kanban,
        start => {tcps_kanban, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_kanban]
    },

    %% Layer 3: Quality Gates (Jidoka)
    QualityGatesSpec = #{
        id => tcps_quality_gates,
        start => {tcps_quality_gates, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_quality_gates]
    },

    %% Layer 4: SKU Release Management
    SkuSpec = #{
        id => tcps_sku,
        start => {tcps_sku, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_sku]
    },

    %% Layer 5: Observability - Dashboard
    DashboardSpec = #{
        id => tcps_dashboard,
        start => {tcps_dashboard, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_dashboard]
    },

    %% Layer 6: Observability - SSE Manager
    SseManagerSpec = #{
        id => tcps_sse_manager,
        start => {tcps_sse_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_sse_manager]
    },

    %% Layer 7: Observability - Metrics Aggregator
    MetricsAggregatorSpec = #{
        id => tcps_metrics_aggregator,
        start => {tcps_metrics_aggregator, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcps_metrics_aggregator]
    },

    %% Only include modules that have start_link/0 and are gen_server behaviors
    %% Other TCPS modules (heijunka, andon, receipt, kaizen, metrics_cache) are
    %% utility modules called directly without supervision
    ChildSpecs = [
        WorkOrderSpec,
        KanbanSpec,
        QualityGatesSpec,
        SkuSpec,
        DashboardSpec,
        SseManagerSpec,
        MetricsAggregatorSpec
    ],

    {ok, {SupFlags, ChildSpecs}}.
