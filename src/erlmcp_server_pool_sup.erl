%% @doc Individual Server Pool Supervisor
%% Manages a single pool of ~1,500 server connections
%% One of 10 pools in the erlmcp_connection_pool_sup tree

-module(erlmcp_server_pool_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("erlmcp.hrl").

-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(PoolName) ->
    supervisor:start_link({local, PoolName}, ?MODULE, [PoolName]).

-spec init([atom()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([_PoolName]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        #{
            id => erlmcp_server,
            start => {erlmcp_server, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
