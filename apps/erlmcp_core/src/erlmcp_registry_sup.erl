-module(erlmcp_registry_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Registry Infrastructure Supervisor
%%
%% Manages core registry and messaging infrastructure
%% This is the foundation - no external dependencies
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,  % Each registry component fails independently
          intensity => 5,
          period => 60},

    ChildSpecs =
        %% ================================================================
        %% REGISTRY: Message routing using gproc
        %% ================================================================
        [#{id => erlmcp_registry,
           start => {erlmcp_registry, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_registry]}],

    {ok, {SupFlags, ChildSpecs}}.