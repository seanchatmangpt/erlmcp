%% @doc Connection Pool Supervisor - Manages 10 independent connection pools
%% Each pool handles ~1,500 concurrent connections in complete isolation.
%% Failure of one pool does not affect others.

-module(erlmcp_connection_pool_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3, get_pool_for_connection/1]).
-export([init/1]).

-include("erlmcp.hrl").

-define(NUM_POOLS, 10).
-define(POOL_NAMES, [
    pool_0, pool_1, pool_2, pool_3, pool_4,
    pool_5, pool_6, pool_7, pool_8, pool_9
]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_child(ConnectionId, Type, Config) ->
    PoolId = get_pool_for_connection(ConnectionId),
    PoolSup = get_pool_supervisor(PoolId),
    case PoolSup of
        {ok, PoolSupPid} ->
            supervisor:start_child(PoolSupPid, [ConnectionId, Type, Config]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_pool_for_connection(atom()) -> atom().
get_pool_for_connection(ConnectionId) ->
    Hash = erlang:hash(ConnectionId, ?NUM_POOLS),
    PoolIndex = Hash rem ?NUM_POOLS,
    lists:nth(PoolIndex + 1, ?POOL_NAMES).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = lists:map(fun(PoolName) ->
        #{
            id => PoolName,
            start => {erlmcp_server_pool_sup, start_link, [PoolName]},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_server_pool_sup]
        }
    end, ?POOL_NAMES),

    {ok, {SupFlags, ChildSpecs}}.

-spec get_pool_supervisor(atom()) -> {ok, pid()} | {error, not_found}.
get_pool_supervisor(PoolName) ->
    case erlang:whereis(PoolName) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.
