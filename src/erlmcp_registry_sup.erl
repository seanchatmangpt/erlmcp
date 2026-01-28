-module(erlmcp_registry_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v1.3.0: Registry Subsystem Supervisor
%%
%% Supervises core registry infrastructure with no external dependencies.
%% This is TIER 1 - no other components depend on being up first.
%%
%% Strategy: one_for_one - each registry component can fail independently
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Each registry shard/component fails independently
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Central registry - message router using gproc
        #{
            id => erlmcp_registry,
            start => {erlmcp_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry]
        },

        %% Registry health checks
        #{
            id => erlmcp_registry_health_check,
            start => {erlmcp_registry_health_check, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry_health_check]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
