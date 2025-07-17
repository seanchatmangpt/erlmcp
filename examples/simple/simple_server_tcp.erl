-module(simple_server_tcp).

-include("erlmcp.hrl").

-export([start/0, main/1]).

start() ->
    main([]).

main(_Args) ->
    %% Start applications
    application:ensure_all_started(erlmcp),
    
    %% Define server capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    
    %% Start TCP server
    TransportOpts = {tcp, #{
        host => "127.0.0.1",
        port => 8080,
        owner => self()
    }},
    
    {ok, Server} = erlmcp_server:start_link(TransportOpts, Capabilities),
    
    %% Add some example tools
    erlmcp_server:add_tool(Server, <<"echo">>, 
        fun(#{<<"message">> := Message}) ->
            <<"Echo: ", Message/binary>>
        end),
    
    erlmcp_server:add_tool(Server, <<"add">>, 
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            integer_to_binary(A + B)
        end),
    
    %% Add example resource
    erlmcp_server:add_resource(Server, <<"file://example.txt">>, 
        fun(_Uri) ->
            <<"This is example content from TCP server.">>
        end),
    
    %% Keep the server running
    receive
        stop -> ok
    end.