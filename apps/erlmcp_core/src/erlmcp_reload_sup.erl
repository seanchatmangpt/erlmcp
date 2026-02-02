-module(erlmcp_reload_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Hot code reload subsystem (OTP 27-28)
    %% Strategy: one_for_all - ensures consistent state between components
    %%
    %% Components:
    %% 1. Code loader - Optimized module loading (OTP 27)
    %% 2. Tool registry - Version tracking for MCP tools
    %% 3. Reload coordinator - Cluster-wide reload coordination
    %% 4. Graceful drain - Pauses new requests during reload
    %% 5. Code reload manager - Legacy reload operations
    SupFlags =
        #{strategy => one_for_all,
          intensity => 3,
          period => 60},

    ChildSpecs =
        [%% OTP 27-28 Code Loader - Optimized module loading
         #{id => erlmcp_code_loader,
           start => {erlmcp_code_loader, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_code_loader]},
         %% Tool Registry - MCP tool version tracking
         #{id => erlmcp_tool_registry,
           start => {erlmcp_tool_registry, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_tool_registry]},
         %% Reload Coordinator - Cluster-wide reload coordination
         #{id => erlmcp_reload_coordinator,
           start => {erlmcp_reload_coordinator, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_reload_coordinator]},
         %% Graceful drain service - pauses new requests during reload
         #{id => erlmcp_graceful_drain,
           start => {erlmcp_graceful_drain, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_graceful_drain]},
         %% Code reload coordinator - manages reload operations (legacy)
         #{id => erlmcp_code_reload,
           start => {erlmcp_code_reload, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_code_reload]}],

    {ok, {SupFlags, ChildSpecs}}.
