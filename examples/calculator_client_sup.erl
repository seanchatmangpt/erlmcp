-module(calculator_client_sup).
-behaviour(supervisor).

-export([start_link/0, start_calculator/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_calculator() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 10
    },
    
    ChildSpecs = [
        #{
            id => calculator_client,
            start => {calculator_client, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker,
            modules => [calculator_client]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.