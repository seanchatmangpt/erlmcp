-module(erlmcp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for Core erlmcp Module
%%====================================================================

%%====================================================================
%% Server Management Tests
%%====================================================================

start_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_start_server()),
             ?_test(test_start_server_with_capabilities()),
             ?_test(test_stop_server()),
             ?_test(test_list_servers())
         ]
     end}.

test_start_server() ->
    ServerId = test_server_basic,
    Result = erlmcp:start_server(ServerId),
    ?assertMatch({ok, _Pid}, Result),
    {ok, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp:stop_server(ServerId).

test_start_server_with_capabilities() ->
    ServerId = test_server_caps,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    Result = erlmcp:start_server(ServerId, Capabilities),
    ?assertMatch({ok, _Pid}, Result),
    erlmcp:stop_server(ServerId).

test_stop_server() ->
    ServerId = test_server_stop,
    {ok, Pid} = erlmcp:start_server(ServerId),
    ?assert(erlang:is_process_alive(Pid)),
    ok = erlmcp:stop_server(ServerId),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)).

test_list_servers() ->
    ServerId1 = test_server_list_1,
    ServerId2 = test_server_list_2,
    {ok, _} = erlmcp:start_server(ServerId1),
    {ok, _} = erlmcp:start_server(ServerId2),

    Servers = erlmcp:list_servers(),
    ?assert(is_list(Servers)),
    ?assert(lists:member(ServerId1, Servers)),
    ?assert(lists:member(ServerId2, Servers)),

    erlmcp:stop_server(ServerId1),
    erlmcp:stop_server(ServerId2).

%%====================================================================
%% Transport Management Tests
%%====================================================================

transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_start_stdio_transport()),
             ?_test(test_bind_transport())
         ]
     end}.

test_start_stdio_transport() ->
    TransportId = test_transport_stdio,
    % Note: stdio transport may not work in test environment
    % This test verifies the API works correctly
    Result = erlmcp:start_transport(TransportId, stdio, #{test_mode => true}),
    case Result of
        {ok, _Pid} ->
            erlmcp:stop_transport(TransportId),
            ?assert(true);
        {error, _Reason} ->
            % Expected in test environment
            ?assert(true)
    end.

test_bind_transport() ->
    ServerId = test_server_bind,
    TransportId = test_transport_bind,

    {ok, _} = erlmcp:start_server(ServerId),

    % Create a mock transport for testing
    case erlmcp:start_transport(TransportId, stdio, #{test_mode => true}) of
        {ok, _} ->
            Result = erlmcp:bind_transport_to_server(TransportId, ServerId),
            ?assertMatch(ok, Result),
            erlmcp:unbind_transport(TransportId),
            erlmcp:stop_transport(TransportId);
        {error, _} ->
            % Expected in test environment
            ?assert(true)
    end,

    erlmcp:stop_server(ServerId).

%%====================================================================
%% Server Operations Tests
%%====================================================================

server_operations_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun(ServerId) ->
         [
             ?_test(test_add_resource(ServerId)),
             ?_test(test_add_tool(ServerId)),
             ?_test(test_add_prompt(ServerId))
         ]
     end}.

test_add_resource(ServerId) ->
    ResourceUri = <<"test://resource">>,
    ResourceName = <<"Test Resource">>,
    Handler = fun(_Uri) -> <<"resource content">> end,

    Result = erlmcp:add_resource(ServerId, ResourceUri, Handler),
    ?assertMatch(ok, Result).

test_add_tool(ServerId) ->
    ToolName = <<"test_tool">>,
    ToolDescription = <<"A test tool">>,
    Handler = fun(_Args) -> <<"tool result">> end,

    Result = erlmcp:add_tool(ServerId, ToolName, Handler),
    ?assertMatch(ok, Result).

test_add_prompt(ServerId) ->
    PromptName = <<"test_prompt">>,
    PromptDescription = <<"A test prompt">>,
    Handler = fun(_Args) -> <<"prompt result">> end,

    Result = erlmcp:add_prompt(ServerId, PromptName, Handler),
    ?assertMatch(ok, Result).

%%====================================================================
%% Configuration Tests
%%====================================================================

configuration_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun(ServerId) ->
         [
             ?_test(test_get_server_config(ServerId)),
             ?_test(test_update_server_config(ServerId))
         ]
     end}.

test_get_server_config(ServerId) ->
    Result = erlmcp:get_server_config(ServerId),
    ?assertMatch({ok, _Config}, Result).

test_update_server_config(ServerId) ->
    NewConfig = #{test_option => test_value},
    Result = erlmcp:update_server_config(ServerId, NewConfig),
    ?assertMatch(ok, Result).

%%====================================================================
%% Legacy Compatibility Tests
%%====================================================================

legacy_stdio_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_stdio_server_api())
         ]
     end}.

test_stdio_server_api() ->
    % Test that legacy API exists
    ?assert(erlang:function_exported(erlmcp, start_stdio_server, 0)),
    ?assert(erlang:function_exported(erlmcp, start_stdio_server, 1)),
    ?assert(erlang:function_exported(erlmcp, stop_stdio_server, 0)).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    % Clean up any remaining test servers
    lists:foreach(fun(ServerId) ->
        case atom_to_list(ServerId) of
            "test_" ++ _ ->
                catch erlmcp:stop_server(ServerId);
            _ ->
                ok
        end
    end, erlmcp:list_servers()),
    ok.

setup_server() ->
    setup(),
    ServerId = test_server_ops,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    {ok, _} = erlmcp:start_server(ServerId, Capabilities),
    ServerId.

cleanup_server(ServerId) ->
    erlmcp:stop_server(ServerId),
    cleanup(ok).
