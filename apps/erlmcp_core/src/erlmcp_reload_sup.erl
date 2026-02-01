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
    %% Hot code reload subsystem
    %% Strategy: one_for_all - if reload coordinator crashes, restart drain service too
    %% This ensures consistent state between coordinator and drain manager
    SupFlags =
        #{strategy => one_for_all,
          intensity => 3,
          period => 60},

    ChildSpecs =
        [%% Graceful drain service - pauses new requests during reload
         #{id => erlmcp_graceful_drain,
           start => {erlmcp_graceful_drain, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_graceful_drain]},
         %% Code reload coordinator - manages reload operations
         #{id => erlmcp_code_reload,
           start => {erlmcp_code_reload, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_code_reload]}],

    {ok, {SupFlags, ChildSpecs}}.
