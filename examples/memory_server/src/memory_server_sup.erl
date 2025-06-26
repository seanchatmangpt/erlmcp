-module(memory_server_sup).
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
    MemoryFilePath = application:get_env(memory_server, memory_file_path, "memory.json"),
    TransportConfig = case application:get_env(memory_server, transport, stdio) of
        stdio -> {stdio, []};
        {tcp, Port} -> {tcp, #{port => Port}};
        {http, Url} -> {http, #{url => Url}}
    end,
    AutoSave = application:get_env(memory_server, auto_save, true),
    SaveInterval = application:get_env(memory_server, save_interval, 5000),
    
    ChildSpecs = [
        #{
            id => memory_server,
            start => {memory_server, start_link, [#{
                memory_file_path => MemoryFilePath,
                auto_save => AutoSave,
                save_interval => SaveInterval
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [memory_server]
        },
        #{
            id => memory_server_mcp,
            start => {memory_server_mcp, start_link, [TransportConfig]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [memory_server_mcp]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.