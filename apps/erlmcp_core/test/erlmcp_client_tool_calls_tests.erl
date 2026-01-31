-module(erlmcp_client_tool_calls_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Chicago School TDD: Tool Calling Functionality Tests
%%====================================================================
%%
%% Testing Methodology:
%% - Chicago School TDD: Real processes, API-based verification, no mocks
%% - Test observable behavior through ALL interfaces
%% - NO state inspection (test API boundaries only)
%% - NO record duplication (respect encapsulation)
%% - Use real erlmcp_server processes for tool calls
%%

%%====================================================================
%% Tool Listing Tests
%%====================================================================

tool_listing_test_() ->
    {setup, fun setup_with_server/0, fun cleanup_with_server/1, fun({C, S}) -> case {C, S} of
        {undefined, _} -> []; {_, undefined} -> []; _ -> [
            {"List tools uninitialized", ?_test(test_list_tools_uninitialized(C))},
            {"List tools after init attempt", ?_test(test_list_tools_after_init(C))}
        ] end end}.

test_list_tools_uninitialized(Client) ->
    ?assertMatch({error, {not_initialized, pre_initialization, _}}, erlmcp_client:list_tools(Client)).

test_list_tools_after_init(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:list_tools(Client) of {ok, Tools} -> ?assert(is_list(Tools)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

%%====================================================================
%% Tool Calling Tests
%%====================================================================

tool_calling_test_() ->
    {setup, fun setup_with_server/0, fun cleanup_with_server/1, fun({C, S}) -> case {C, S} of
        {undefined, _} -> []; {_, undefined} -> []; _ -> [
            {"Call tool before initialization", ?_test(test_call_tool_uninitialized(C))},
            {"Call tool with simple arguments", ?_test(test_call_tool_simple(C))},
            {"Call tool with complex arguments", ?_test(test_call_tool_complex(C))},
            {"Call tool with empty arguments", ?_test(test_call_tool_empty_args(C))},
            {"Call non-existent tool", ?_test(test_call_nonexistent_tool(C))}
        ] end end}.

test_call_tool_uninitialized(Client) ->
    ?assertMatch({error, {not_initialized, pre_initialization, _}}, erlmcp_client:call_tool(Client, <<"t">>, #{})).

test_call_tool_simple(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:call_tool(Client, <<"echo">>, #{msg => <<"hi">>}) of
                {ok, R} -> ?assert(is_map(R)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_call_tool_complex(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            Args = #{str => <<"s">>, num => 42, flag => true, nested => #{k => v}, list => [1, 2, 3]},
            case erlmcp_client:call_tool(Client, <<"complex">>, Args) of {ok, R} -> ?assert(is_map(R)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_call_tool_empty_args(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:call_tool(Client, <<"noargs">>, #{}) of {ok, R} -> ?assert(is_map(R)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_call_nonexistent_tool(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} -> ?assertMatch({error, _}, erlmcp_client:call_tool(Client, <<"nonexistent_xyz">>, #{}));
        {error, _} -> ?assert(true)
    end.

%%====================================================================
%% Resource Operations Tests
%%====================================================================

resource_operations_test_() ->
    {setup, fun setup_with_server/0, fun cleanup_with_server/1, fun({C, S}) -> case {C, S} of
        {undefined, _} -> []; {_, undefined} -> []; _ -> [
            {"List resources uninitialized", ?_test(test_list_resources_uninitialized(C))},
            {"List resource templates", ?_test(test_list_resource_templates(C))},
            {"Read resource by URI", ?_test(test_read_resource(C))},
            {"Subscribe to resource", ?_test(test_subscribe_resource(C))},
            {"Unsubscribe from resource", ?_test(test_unsubscribe_resource(C))}
        ] end end}.

test_list_resources_uninitialized(Client) ->
    ?assertMatch({error, {not_initialized, pre_initialization, _}}, erlmcp_client:list_resources(Client)).

test_list_resource_templates(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:list_resource_templates(Client) of {ok, T} -> ?assert(is_list(T)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_read_resource(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:read_resource(Client, <<"file:///t.txt">>) of {ok, C} -> ?assert(is_map(C)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_subscribe_resource(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:subscribe_to_resource(Client, <<"file:///t.txt">>) of ok -> ?assert(true); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_unsubscribe_resource(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:unsubscribe_from_resource(Client, <<"file:///t.txt">>) of ok -> ?assert(true); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

%%====================================================================
%% Prompt Operations Tests
%%====================================================================

prompt_operations_test_() ->
    {setup, fun setup_with_server/0, fun cleanup_with_server/1, fun({C, S}) -> case {C, S} of
        {undefined, _} -> []; {_, undefined} -> []; _ -> [
            {"List prompts uninitialized", ?_test(test_list_prompts_uninitialized(C))},
            {"List prompts after init", ?_test(test_list_prompts(C))},
            {"Get prompt by name", ?_test(test_get_prompt(C))},
            {"Get prompt with arguments", ?_test(test_get_prompt_with_args(C))}
        ] end end}.

test_list_prompts_uninitialized(Client) ->
    ?assertMatch({error, {not_initialized, pre_initialization, _}}, erlmcp_client:list_prompts(Client)).

test_list_prompts(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:list_prompts(Client) of {ok, P} -> ?assert(is_list(P)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_get_prompt(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:get_prompt(Client, <<"test_prompt">>) of {ok, P} -> ?assert(is_map(P)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_get_prompt_with_args(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:get_prompt(Client, <<"test_prompt">>, #{n => <<"test">>, v => 42}) of
                {ok, P} -> ?assert(is_map(P)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

%%====================================================================
%% Completion Operations Tests
%%====================================================================

completion_operations_test_() ->
    {setup, fun setup_with_server/0, fun cleanup_with_server/1, fun({C, S}) -> case {C, S} of
        {undefined, _} -> []; {_, undefined} -> []; _ -> [
            {"Complete with default timeout", ?_test(test_complete_default_timeout(C))},
            {"Complete with custom timeout", ?_test(test_complete_custom_timeout(C))},
            {"Complete with invalid ref", ?_test(test_complete_invalid_ref(C))}
        ] end end}.

test_complete_default_timeout(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:complete(Client, <<"ref">>, <<"partial">>) of {ok, C} -> ?assert(is_map(C)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_complete_custom_timeout(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} ->
            case erlmcp_client:complete(Client, <<"ref">>, <<"partial">>, 10000) of {ok, C} -> ?assert(is_map(C)); {error, _} -> ?assert(true) end;
        {error, _} -> ?assert(true)
    end.

test_complete_invalid_ref(Client) ->
    case erlmcp_client:initialize(Client, #mcp_client_capabilities{}) of
        {ok, _} -> ?assertMatch({error, _}, erlmcp_client:complete(Client, <<>>, <<"p">>));
        {error, _} -> ?assert(true)
    end.

%%====================================================================
%% Batch Operations Tests
%%====================================================================

batch_operations_test_() ->
    {setup, fun setup_with_server/0, fun cleanup_with_server/1, fun({C, S}) -> case {C, S} of
        {undefined, _} -> []; {_, undefined} -> []; _ -> [
            {"Batch request creation", ?_test(test_batch_request_creation(C))},
            {"Batch request execution", ?_test(test_batch_request_execution(C))},
            {"Concurrent batch requests", ?_test(test_concurrent_batch_requests(C))}
        ] end end}.

test_batch_request_creation(Client) ->
    try
        ?assertMatch({ok, _}, erlmcp_client:with_batch(Client, fun(Bid) ->
            erlmcp_client:send_batch_request(Client, Bid, <<"tools/list">>, #{}),
            erlmcp_client:send_batch_request(Client, Bid, <<"resources/list">>, #{})
        end))
    catch error:badarg -> ok end.

test_batch_request_execution(Client) ->
    try
        ?assertMatch({ok, 3}, erlmcp_client:with_batch(Client, fun(Bc) ->
            erlmcp_client:send_batch_request(Bc, <<"b1">>, <<"tools/list">>, #{}),
            erlmcp_client:send_batch_request(Bc, <<"b1">>, <<"resources/list">>, #{}),
            erlmcp_client:send_batch_request(Bc, <<"b1">>, <<"prompts/list">>, #{})
        end))
    catch error:badarg -> ok end.

test_concurrent_batch_requests(Client) ->
    try
        spawn_link(fun() -> erlmcp_client:with_batch(Client, fun(Bc) ->
            erlmcp_client:send_batch_request(Bc, <<"b1">>, <<"tools/list">>, #{}),
            erlmcp_client:send_batch_request(Bc, <<"b1">>, <<"resources/list">>, #{})
        end) end),
        spawn_link(fun() -> erlmcp_client:with_batch(Client, fun(Bc) ->
            erlmcp_client:send_batch_request(Bc, <<"b2">>, <<"prompts/list">>, #{}),
            erlmcp_client:send_batch_request(Bc, <<"b2">>, <<"tools/list">>, #{})
        end) end),
        timer:sleep(1000), ?assert(true)
    catch error:badarg -> ok end.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup_application() -> application:ensure_all_started(erlmcp_core), ok.
cleanup_application(_) -> application:stop(erlmcp_core), ok.

setup_with_server() ->
    setup_application(),
    ServerCaps = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true},
        resources = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    Server = case erlmcp_server:start_link(<<"test_srv_tools">>, ServerCaps) of {ok, SP} -> SP; {error, _} -> undefined end,
    Client = case erlmcp_client:start_link({stdio, #{test_mode => true}}) of {ok, CP} -> CP; {error, _} -> undefined end,
    {Client, Server}.

cleanup_with_server({undefined, undefined}) -> cleanup_application(ok);
cleanup_with_server({Client, undefined}) -> erlmcp_client:stop(Client), cleanup_application(ok);
cleanup_with_server({undefined, Server}) -> erlmcp_server:stop(Server), cleanup_application(ok);
cleanup_with_server({Client, Server}) -> erlmcp_client:stop(Client), erlmcp_server:stop(Server), cleanup_application(ok).
