-module(memory_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    
    ChildSpecs = [
        #{
            id => memory_server,
            start => {memory_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [memory_server]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
