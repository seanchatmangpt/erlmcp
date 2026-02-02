%% @doc Comprehensive coverage tests for erlmcp_server
%% Tests all public API functions to achieve 80%+ coverage
%% Chicago School TDD: Real gen_server, no mocks, state-based verification
-module(erlmcp_server_coverage_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

server_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun run_server_tests/1}.

setup_server() ->
    % Start application dependencies
    application:ensure_all_started(erlmcp),

    % Start server with minimal config
    Config = #{
        server_id => test_coverage_server,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),
    ServerPid.

cleanup_server(_ServerPid) ->
    % Stop server
    catch erlmcp_server:stop(test_coverage_server),

    % Clean up any registered processes
    gproc:cleanup(),
    ok.

run_server_tests(_ServerPid) ->
    [
     {"Server lifecycle", fun test_server_lifecycle/0},
     {"Server registration", fun test_server_registration/0},
     {"Resource operations", fun test_resource_operations/0},
     {"Tool operations", fun test_tool_operations/0},
     {"Prompt operations", fun test_prompt_operations/0},
     {"Subscription operations", fun test_subscription_operations/0},
     {"Capability queries", fun test_capability_queries/0},
     {"Phase enforcement", fun test_phase_enforcement/0},
     {"Process info", fun test_process_info/0},
     {"Cleanup on stop", fun test_cleanup/0}
    ].

%%%====================================================================
%%% Server Lifecycle Tests
%%%====================================================================

test_server_lifecycle() ->
    ServerId = lifecycle_server,

    % Start server
    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),
    ?assert(is_pid(ServerPid)),

    % Verify server is in registry
    {ok, {_Pid, _Config}} = erlmcp_registry:find_server(ServerId),

    % Stop server
    ok = erlmcp_server:stop(ServerId),

    % Verify server removed from registry
    {error, not_found} = erlmcp_registry:find_server(ServerId).

%%%====================================================================
%%% Server Registration Tests
%%%====================================================================

test_server_registration() ->
    ServerId = registration_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Verify registered in registry
    {ok, {RegisteredPid, RegisteredConfig}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(ServerPid, RegisteredPid),
    ?assertEqual(ServerId, RegisteredConfig),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Resource Operations Tests
%%%====================================================================

test_resource_operations() ->
    ServerId = resource_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Add resource
    Uri = <<"file://test.txt">>,
    Resource = #mcp_resource{
        uri = Uri,
        name = <<"test">>,
        description => <<"Test resource">>
    },
    Handler = fun(_Uri) -> #mcp_content{type = <<"text">>, text => <<"test">>} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Resource, Handler),

    % Verify resource added
    {ok, ResourceList} = erlmcp_server:list_resources(ServerPid),
    ?assert(lists:member(Uri, ResourceList)),

    % Read resource
    {ok, Content} = erlmcp_server:read_resource(ServerPid, Uri),
    ?assertMatch(#mcp_content{}, Content),

    % Remove resource
    ok = erlmcp_server:remove_resource(ServerPid, Uri),

    % Verify resource removed
    {ok, UpdatedResourceList} = erlmcp_server:list_resources(ServerPid),
    ?assertNot(lists:member(Uri, UpdatedResourceList)),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Tool Operations Tests
%%%====================================================================

test_tool_operations() ->
    ServerId = tool_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Add tool
    ToolName = <<"test_tool">>,
    Tool = #mcp_tool{
        name = ToolName,
        description = <<"Test tool">>,
        input_schema => #{}
    },
    Handler = fun(_Args) -> #mcp_content{type = <<"text">>, text => <<"result">>} end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Tool, Handler),

    % Verify tool added
    {ok, ToolList} = erlmcp_server:list_tools(ServerPid),
    ?assert(lists:member(ToolName, ToolList)),

    % Call tool
    {ok, Result} = erlmcp_server:call_tool(ServerPid, ToolName, #{}),
    ?assertMatch(#mcp_content{}, Result),

    % Remove tool
    ok = erlmcp_server:remove_tool(ServerPid, ToolName),

    % Verify tool removed
    {ok, UpdatedToolList} = erlmcp_server:list_tools(ServerPid),
    ?assertNot(lists:member(ToolName, UpdatedToolList)),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Prompt Operations Tests
%%%====================================================================

test_prompt_operations() ->
    ServerId = prompt_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Add prompt
    PromptName = <<"test_prompt">>,
    Prompt = #mcp_prompt{
        name = PromptName,
        description = <<"Test prompt">>,
        arguments = [
            #mcp_prompt_argument{name = <<"arg1">>, description = <<"Argument 1">>, required = false}
        ]
    },
    Handler = fun(_Args) -> [#{<<"role">> => <<"user">>, <<"content">> => <<"test">>}] end,

    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Prompt, Handler),

    % Verify prompt added
    {ok, PromptList} = erlmcp_server:list_prompts(ServerPid),
    ?assert(lists:member(PromptName, PromptList)),

    % Get prompt
    {ok, _Result} = erlmcp_server:get_prompt(ServerPid, PromptName, #{}),

    % Remove prompt
    ok = erlmcp_server:remove_prompt(ServerPid, PromptName),

    % Verify prompt removed
    {ok, UpdatedPromptList} = erlmcp_server:list_prompts(ServerPid),
    ?assertNot(lists:member(PromptName, UpdatedPromptList)),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Subscription Operations Tests
%%%====================================================================

test_subscription_operations() ->
    ServerId = subscription_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{subscribe = true},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Add resource first
    Uri = <<"file://subscribed.txt">>,
    Resource = #mcp_resource{
        uri = Uri,
        name = <<"subscribed">>,
        description = <<"Test subscribed resource">>
    },
    Handler = fun(_Uri) -> #mcp_content{type => <<"text">>, text => <<"test">>} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Resource, Handler),

    % Subscribe to resource
    ClientPid = self(),
    ok = erlmcp_server:subscribe_to_resource(ServerPid, Uri, ClientPid),

    % Verify subscription
    {ok, Subscribers} = erlmcp_server:list_subscriptions(ServerPid, Uri),
    ?assert(lists:member(ClientPid, Subscribers)),

    % Unsubscribe from resource
    ok = erlmcp_server:unsubscribe_from_resource(ServerPid, Uri, ClientPid),

    % Verify unsubscription
    {ok, UpdatedSubscribers} = erlmcp_server:list_subscriptions(ServerPid, Uri),
    ?assertNot(lists:member(ClientPid, UpdatedSubscribers)),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Capability Queries Tests
%%%====================================================================

test_capability_queries() ->
    ServerId = capability_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{subscribe = true},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{},
            logging => #mcp_logging_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Get capabilities
    {ok, Caps} = erlmcp_server:get_capabilities(ServerPid),
    ?assertMatch(#mcp_server_capabilities{}, Caps),

    % Check individual capabilities
    ?assertEqual(true, Caps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(true, Caps#mcp_server_capabilities.tools#mcp_tools_capability.enabled),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Phase Enforcement Tests
%%%====================================================================

test_phase_enforcement() ->
    ServerId = phase_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Get server phase
    {ok, Phase} = erlmcp_server:get_phase(ServerPid),
    ?assertEqual(initialization, Phase),

    % Simulate initialization completion
    ServerPid ! {transport_message, jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                                                   <<"method">> => <<"notifications/initialized">>,
                                                   <<"params">> => #{}})},

    % Wait for state update
    timer:sleep(50),

    % Verify phase changed
    {ok, UpdatedPhase} = erlmcp_server:get_phase(ServerPid),
    ?assertEqual(initialized, UpdatedPhase),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Process Info Tests
%%%====================================================================

test_process_info() ->
    ServerId = info_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Get server state
    {ok, State} = erlmcp_server:get_state(ServerPid),
    ?assertMatch(#mcp_server_state{}, State),
    ?assertEqual(ServerId, State#mcp_server_state.server_id),

    % Get process info
    Status = sys:get_status(ServerPid),
    ?assertMatch({status, _, _, _}, Status),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).

%%%====================================================================
%%% Cleanup Tests
%%%====================================================================

test_cleanup() ->
    ServerId = cleanup_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resourcesCapability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Add some resources, tools, and prompts
    Uri = <<"file://cleanup.txt">>,
    Resource = #mcp_resource{uri = Uri, name = <<"cleanup">>},
    ok = erlmcp_server:add_resource(ServerPid, Uri, Resource, fun(_) -> ok end),

    ToolName = <<"cleanup_tool">>,
    Tool = #mcp_tool{name = ToolName, description = <<"Cleanup tool">>},
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Tool, fun(_) -> ok end),

    PromptName = <<"cleanup_prompt">>,
    Prompt = #mcp_prompt{name = PromptName, description = <<"Cleanup prompt">>},
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Prompt, fun(_) -> ok end),

    % Stop server
    ok = erlmcp_server:stop(ServerId),

    % Verify cleanup - server should not be in registry
    {error, not_found} = erlmcp_registry:find_server(ServerId).

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

error_handling_test() ->
    ServerId = error_server,

    Config = #{
        server_id => ServerId,
        capabilities => #mcp_server_capabilities{
            resources => #mcp_resources_capability{},
            tools => #mcp_tools_capability{},
            prompts => #mcp_prompts_capability{}
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(Config),

    % Try to get non-existent resource
    {error, not_found} = erlmcp_server:read_resource(ServerPid, <<"file://nonexistent">>),

    % Try to call non-existent tool
    {error, not_found} = erlmcp_server:call_tool(ServerPid, <<"nonexistent_tool">>, #{}),

    % Try to get non-existent prompt
    {error, not_found} = erlmcp_server:get_prompt(ServerPid, <<"nonexistent_prompt">>, #{}),

    % Cleanup
    ok = erlmcp_server:stop(ServerId).
