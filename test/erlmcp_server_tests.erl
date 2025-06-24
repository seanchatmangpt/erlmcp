-module(erlmcp_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Server) ->
         [
             ?_test(test_add_resource(Server)),
             ?_test(test_add_tool(Server)),
             ?_test(test_add_prompt(Server))
         ]
     end}.

setup() ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{name = <<"resources">>, enabled = true},
        tools = #mcp_capability{name = <<"tools">>, enabled = true},
        prompts = #mcp_capability{name = <<"prompts">>, enabled = true}
    },
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),
    Server.

cleanup(Server) ->
    erlmcp_server:stop(Server).

test_add_resource(Server) ->
    Handler = fun() -> <<"test resource content">> end,
    ?assertEqual(ok, erlmcp_server:add_resource(Server, <<"test_resource">>, Handler)).

test_add_tool(Server) ->
    Handler = fun(Args) -> <<"tool result: ", (jsx:encode(Args))/binary>> end,
    ?assertEqual(ok, erlmcp_server:add_tool(Server, <<"test_tool">>, Handler)).

test_add_prompt(Server) ->
    Handler = fun(Args) -> <<"prompt result: ", (jsx:encode(Args))/binary>> end,
    ?assertEqual(ok, erlmcp_server:add_prompt(Server, <<"test_prompt">>, Handler)).
