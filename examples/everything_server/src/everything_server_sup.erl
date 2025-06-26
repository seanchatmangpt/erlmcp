-module(everything_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5
    },
    
    %% Get configuration from application environment
    TransportConfig = case application:get_env(everything_server, transport, stdio) of
        stdio -> {stdio, []};
        {tcp, Port} -> {tcp, #{port => Port}};
        {http, Url} -> {http, #{url => Url}}
    end,
    
    ChildSpecs = [
        #{
            id => everything_server,
            start => {everything_server, start_link, [TransportConfig]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [everything_server]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
