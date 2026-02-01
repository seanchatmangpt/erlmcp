-module(erlmcp_infrastructure_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Infrastructure Supervisor
%%
%% Supervises core infrastructure services:
%% - Health checks (orchestration support)
%% - Hot code reload
%% - Hooks (Claude Code integration)
%% - Caching (icons, general cache, cache warmers)
%% - Request cancellation
%% - Pagination
%% - Client supervisor (dynamic client processes)
%% - Plugin supervisor (plugin system)
%%
%% Strategy: one_for_one - independent infrastructure components
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% ================================================================
        %% HEALTH CHECKS: Simple health check aggregator for orchestration
        %% Follows Joe Armstrong's principle: "Health checks are for orchestration"
        %% ================================================================
        #{
            id => erlmcp_health,
            start => {erlmcp_health, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_health]
        },

        %% ================================================================
        %% HOT RELOAD SUPERVISOR: Code reload infrastructure
        %% ================================================================
        #{
            id => erlmcp_reload_sup,
            start => {erlmcp_reload_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_reload_sup]
        },

        %% ================================================================
        %% HOOKS: Claude Code integration hooks
        %% NOTE: Replaces erlmcp_task_manager (module was never implemented)
        %% ================================================================
        #{
            id => erlmcp_hooks,
            start => {erlmcp_hooks, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_hooks]
        },

        %% ================================================================
        %% ICON CACHE: Cache for icon resources
        %% ================================================================
        #{
            id => erlmcp_icon_cache,
            start => {erlmcp_icon_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_icon_cache]
        },

        %% ================================================================
        %% CACHE: Multi-level intelligent caching (L1: ETS, L2: Mnesia, L3: External)
        %% ================================================================
        #{
            id => erlmcp_cache,
            start => {erlmcp_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cache]
        },

        %% ================================================================
        %% CACHE WARMER SUPERVISOR: Supervises async cache warming workers
        %% ================================================================
        #{
            id => erlmcp_cache_warmer_sup,
            start => {erlmcp_cache_warmer_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_cache_warmer_sup]
        },

        %% ================================================================
        %% CANCELLATION: Request cancellation for long-running operations (TASK #142)
        %% ================================================================
        #{
            id => erlmcp_cancellation,
            start => {erlmcp_cancellation, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cancellation]
        },

        %% ================================================================
        %% PAGINATION: Cursor-based pagination for list operations (TASK #146)
        %% ================================================================
        #{
            id => erlmcp_pagination,
            start => {erlmcp_pagination, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_pagination]
        },

        %% ================================================================
        %% CLIENT SUPERVISOR: Dynamic client process management (TIER 2)
        %% Manages client connections using simple_one_for_one strategy
        %% Each client is a separate gen_server process (process-per-connection)
        %% ================================================================
        #{
            id => erlmcp_client_sup,
            start => {erlmcp_client_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Supervisor - wait for all children
            type => supervisor,
            modules => [erlmcp_client_sup]
        },

        %% ================================================================
        %% PLUGIN SUPERVISOR: Plugin system management
        %% Manages plugin discovery, loading, registry, and execution
        %% Each plugin runs in isolated process (let-it-crash)
        %% ================================================================
        #{
            id => erlmcp_plugin_sup,
            start => {erlmcp_plugin_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Supervisor - wait for all children
            type => supervisor,
            modules => [erlmcp_plugin_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
