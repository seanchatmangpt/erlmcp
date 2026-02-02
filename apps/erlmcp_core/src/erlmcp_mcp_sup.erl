-module(erlmcp_mcp_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% MCP Protocol Supervisor
%%
%% Manages MCP protocol-related infrastructure and services
%% These are MCP-specific services but not core protocol servers
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,  % Each MCP component fails independently
          intensity => 5,
          period => 60},

    ChildSpecs =
        %% ================================================================
        %% HEALTH CHECKS: Simple health check aggregator for orchestration
        %% Follows Joe Armstrong's principle: "Health checks are for orchestration"
        %% ================================================================
        [#{id => erlmcp_health,
           start => {erlmcp_health, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_health]},
         %% ================================================================
        %% MCP INFRASTRUCTURE: Hot reload, resource subscriptions, SSE events
        %% ================================================================
        #{id => erlmcp_reload_sup,
           start => {erlmcp_reload_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_reload_sup]},
        #{id => erlmcp_resource_subscriptions,
           start => {erlmcp_resource_subscriptions, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_resource_subscriptions]},
        #{id => erlmcp_sse_event_store,
           start => {erlmcp_sse_event_store, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_sse_event_store]},
        #{id => erlmcp_icon_cache,
           start => {erlmcp_icon_cache, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_icon_cache]},
        %% ================================================================
        %% CACHING: Multi-level intelligent caching (L1: ETS, L2: Mnesia, L3: External)
        %% ================================================================
        #{id => erlmcp_cache,
           start => {erlmcp_cache, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cache]},
        %% Cache Warmer Supervisor - Supervises async cache warming workers
        #{id => erlmcp_cache_warmer_sup,
           start => {erlmcp_cache_warmer_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_cache_warmer_sup]},
        %% ================================================================
        %% MCP SERVICES: Roots, apps, completion, elicitation
        %% ================================================================
        #{id => erlmcp_roots_server,
           start => {erlmcp_roots_server, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_roots_server]},
        #{id => erlmcp_apps_server,
           start => {erlmcp_apps_server, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_apps_server]},
        #{id => erlmcp_completion,
           start => {erlmcp_completion, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_completion]},
        #{id => erlmcp_elicitation,
           start => {erlmcp_elicitation, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_elicitation]},
        %% ================================================================
        %% CLIENT MANAGEMENT: Dynamic client process management
        ================================================================
        #{id => erlmcp_client_sup,
           start => {erlmcp_client_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_client_sup]}],

    {ok, {SupFlags, ChildSpecs}}.