-module(erlmcp_api_gateway_redis_pool).
-behaviour(supervisor).

-export([start_link/0, pool_name/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

pool_name() ->
    erlmcp_api_gateway_redis_pool.

init([]) ->
    SupFlags = #{strategy => one_for_all,
                intensity => 10,
                period => 10},
    PoolArgs = [
        {name, {local, pool_name()}},
        {worker_module, erlmcp_api_gateway_redis_worker},
        {size, 10},
        {max_overflow, 20}
    ],
    ChildSpecs = [
        {redis_poolboy,
         {poolboy, start_link, [PoolArgs]},
         permanent,
         5000,
         worker,
         [erlmcp_api_gateway_redis_worker]}
    ],
    {ok, {SupFlags, ChildSpecs}}.