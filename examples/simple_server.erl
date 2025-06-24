-module(simple_server).

-include("erlmcp.hrl").

-export([start/0]).

start() ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{name = <<"resources">>, enabled = true},
        tools = #mcp_capability{name = <<"tools">>, enabled = true}
    },
    
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),
    
    erlmcp_server:add_resource(Server, <<"file://example.txt">>, 
        fun() -> <<"This is example content from a resource.">> end),
    
    erlmcp_server:add_tool(Server, <<"echo">>, 
        fun(#{<<"message">> := Message}) -> 
            <<"Echo: ", Message/binary>>
        end),
    
    erlmcp_server:add_tool(Server, <<"add">>, 
        fun(#{<<"a">> := A, <<"b">> := B}) -> 
            Result = A + B,
            integer_to_binary(Result)
        end),
    
    io:format("MCP Server started. Send initialize request to begin.~n"),
    
    receive
        stop -> ok
    end.
