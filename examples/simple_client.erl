-module(simple_client).

-include("erlmcp.hrl").

-export([start/0]).

start() ->
    Capabilities = #mcp_client_capabilities{
        roots = #mcp_capability{name = <<"roots">>, enabled = true}
    },
    
    {ok, Client} = erlmcp_client:start_link({stdio, []}),
    
    case erlmcp_client:initialize(Client, Capabilities) of
        {ok, ServerInfo} ->
            io:format("Connected to server: ~p~n", [ServerInfo]),
            
            case erlmcp_client:list_resources(Client) of
                {ok, Resources} ->
                    io:format("Available resources: ~p~n", [Resources]);
                {error, Reason} ->
                    io:format("Failed to list resources: ~p~n", [Reason])
            end,
            
            case erlmcp_client:list_tools(Client) of
                {ok, Tools} ->
                    io:format("Available tools: ~p~n", [Tools]);
                {error, Reason} ->
                    io:format("Failed to list tools: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Failed to initialize: ~p~n", [Reason])
    end,
    
    erlmcp_client:stop(Client).
