-module(simple_client).
-export([run/0, run_advanced/0]).

-include("erlmcp.hrl").

run() ->
    TransportOpts = {stdio, []},
    {ok, Client} = erlmcp_client:start_link(TransportOpts),
    
    Capabilities = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false}
    },
    
    {ok, _InitResult} = erlmcp_client:initialize(Client, Capabilities),
    
    {ok, Resources} = erlmcp_client:list_resources(Client),
    io:format("Resources: ~p~n", [Resources]),
    
    {ok, Tools} = erlmcp_client:list_tools(Client),
    io:format("Tools: ~p~n", [Tools]),
    
    erlmcp_client:stop(Client).

run_advanced() ->
    TransportOpts = {stdio, []},
    Options = #{strict_mode => false},
    {ok, Client} = erlmcp_client:start_link(TransportOpts, Options),
    
    Capabilities = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true},
        experimental = #{<<"customFeature">> => #{<<"enabled">> => true}}
    },
    
    {ok, _InitResult} = erlmcp_client:initialize(Client, Capabilities, #{}),
    
    NotificationHandler = fun(Method, Params) ->
        io:format("Received notification: ~p with params: ~p~n", [Method, Params])
    end,
    ok = erlmcp_client:set_notification_handler(Client, <<"resources/updated">>, NotificationHandler),
    
    SamplingHandler = fun(Method, Params) ->
        io:format("Received sampling request: ~p with params: ~p~n", [Method, Params])
    end,
    ok = erlmcp_client:set_sampling_handler(Client, SamplingHandler),
    
    ok = erlmcp_client:subscribe_to_resource(Client, <<"example://resource">>),
    
    BatchResult = erlmcp_client:with_batch(Client, fun(BatchId) ->
        {ok, _} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
        {ok, _} = erlmcp_client:send_batch_request(Client, BatchId, <<"tools/list">>, #{}),
        {ok, _} = erlmcp_client:send_batch_request(Client, BatchId, <<"prompts/list">>, #{})
    end),
    io:format("Batch result: ~p~n", [BatchResult]),
    
    {ok, ResourceTemplates} = erlmcp_client:list_resource_templates(Client),
    io:format("Resource templates: ~p~n", [ResourceTemplates]),
    
    {ok, PromptWithArgs} = erlmcp_client:get_prompt(Client, <<"example">>, #{<<"arg1">> => <<"value1">>}),
    io:format("Prompt with arguments: ~p~n", [PromptWithArgs]),
    
    ok = erlmcp_client:unsubscribe_from_resource(Client, <<"example://resource">>),
    
    erlmcp_client:stop(Client).
