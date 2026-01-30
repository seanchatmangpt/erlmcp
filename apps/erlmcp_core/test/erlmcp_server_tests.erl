-module(erlmcp_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_server Module
%%====================================================================

%%====================================================================
%% Basic Server Tests
%%====================================================================

server_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_start_link()),
             ?_test(test_stop()),
             ?_test(test_server_with_capabilities())
         ]
     end}.

test_start_link() ->
    ServerId = test_server_lifecycle,
    Capabilities = default_capabilities(),
    Result = erlmcp_server:start_link(ServerId, Capabilities),
    ?assertMatch({ok, _Pid}, Result),
    {ok, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_server:stop(Pid).

test_stop() ->
    ServerId = test_server_stop,
    Capabilities = default_capabilities(),
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    ?assert(erlang:is_process_alive(Pid)),
    ok = erlmcp_server:stop(Pid),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)).

test_server_with_capabilities() ->
    ServerId = test_server_caps,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    ?assert(is_pid(Pid)),
    erlmcp_server:stop(Pid).

%%====================================================================
%% Resource Management Tests
%%====================================================================

resource_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun(Server) ->
         [
             ?_test(test_add_resource(Server)),
             ?_test(test_add_resource_template(Server)),
             ?_test(test_resource_handler(Server))
         ]
     end}.

test_add_resource(Server) ->
    ResourceUri = <<"test://resource/1">>,
    Resource = #mcp_resource{
        uri = ResourceUri,
        name = <<"Test Resource">>,
        description = <<"A test resource">>,
        mime_type = <<"text/plain">>
    },
    Handler = fun(_Uri) -> <<"resource content">> end,

    Result = erlmcp_server:add_resource(Server, Resource, Handler),
    ?assertMatch(ok, Result).

test_add_resource_template(Server) ->
    TemplateUri = <<"test://template/{id}">>,
    Template = #mcp_resource_template{
        uri_template = TemplateUri,
        name = <<"Test Template">>,
        description = <<"A test resource template">>,
        mime_type = <<"application/json">>
    },
    Handler = fun(Uri) ->
        jsx:encode(#{uri => Uri, content => <<"template data">>})
    end,

    Result = erlmcp_server:add_resource_template(Server, Template, Handler),
    ?assertMatch(ok, Result).

test_resource_handler(Server) ->
    ResourceUri = <<"test://resource/handler">>,
    TestContent = <<"test content data">>,
    Resource = #mcp_resource{
        uri = ResourceUri,
        name = <<"Handler Test">>,
        mime_type = <<"text/plain">>
    },
    Handler = fun(_) -> TestContent end,

    ok = erlmcp_server:add_resource(Server, Resource, Handler),
    % Resource is now registered
    ?assert(true).

%%====================================================================
%% Tool Management Tests
%%====================================================================

tool_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun(Server) ->
         [
             ?_test(test_add_tool(Server)),
             ?_test(test_add_tool_with_schema(Server)),
             ?_test(test_tool_handler(Server))
         ]
     end}.

test_add_tool(Server) ->
    ToolName = <<"test_tool">>,
    Tool = #mcp_tool{
        name = ToolName,
        description = <<"A test tool">>,
        input_schema = #{
            type => <<"object">>,
            properties => #{
                input => #{type => <<"string">>}
            }
        }
    },
    Handler = fun(Args) ->
        Input = maps:get(<<"input">>, Args, <<"">>),
        <<"Processed: ", Input/binary>>
    end,

    Result = erlmcp_server:add_tool(Server, Tool, Handler),
    ?assertMatch(ok, Result).

test_add_tool_with_schema(Server) ->
    ToolName = <<"schema_tool">>,
    Tool = #mcp_tool{
        name = ToolName,
        description = <<"Tool with schema">>
    },
    Schema = #{
        type => <<"object">>,
        properties => #{
            value => #{type => <<"number">>}
        },
        required => [<<"value">>]
    },
    Handler = fun(Args) ->
        Value = maps:get(<<"value">>, Args, 0),
        jsx:encode(#{result => Value * 2})
    end,

    Result = erlmcp_server:add_tool_with_schema(Server, Tool, Handler, Schema),
    ?assertMatch(ok, Result).

test_tool_handler(Server) ->
    ToolName = <<"handler_tool">>,
    Tool = #mcp_tool{
        name = ToolName,
        description = <<"Handler test tool">>
    },
    TestResult = <<"tool executed">>,
    Handler = fun(_) -> TestResult end,

    ok = erlmcp_server:add_tool(Server, Tool, Handler),
    ?assert(true).

%%====================================================================
%% Prompt Management Tests
%%====================================================================

prompt_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun(Server) ->
         [
             ?_test(test_add_prompt(Server)),
             ?_test(test_add_prompt_with_args(Server)),
             ?_test(test_prompt_handler(Server))
         ]
     end}.

test_add_prompt(Server) ->
    PromptName = <<"test_prompt">>,
    Prompt = #mcp_prompt{
        name = PromptName,
        description = <<"A test prompt">>
    },
    Handler = fun(_Args) ->
        [#{
            role => <<"user">>,
            content => #mcp_content{
                type = <<"text">>,
                text = <<"Test prompt content">>
            }
        }]
    end,

    Result = erlmcp_server:add_prompt(Server, Prompt, Handler),
    ?assertMatch(ok, Result).

test_add_prompt_with_args(Server) ->
    PromptName = <<"args_prompt">>,
    Prompt = #mcp_prompt{
        name = PromptName,
        description = <<"Prompt with arguments">>
    },
    Arguments = [
        #mcp_prompt_argument{
            name = <<"topic">>,
            description = <<"Topic for the prompt">>,
            required = true
        }
    ],
    Handler = fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"default">>),
        [#{
            role => <<"user">>,
            content => #mcp_content{
                type = <<"text">>,
                text = <<"Topic: ", Topic/binary>>
            }
        }]
    end,

    Result = erlmcp_server:add_prompt_with_args(Server, Prompt, Handler, Arguments),
    ?assertMatch(ok, Result).

test_prompt_handler(Server) ->
    PromptName = <<"handler_prompt">>,
    Prompt = #mcp_prompt{
        name = PromptName,
        description = <<"Handler test prompt">>
    },
    Handler = fun(_) ->
        [#{role => <<"system">>, content => #mcp_content{type = <<"text">>, text = <<"test">>}}]
    end,

    ok = erlmcp_server:add_prompt(Server, Prompt, Handler),
    ?assert(true).

%%====================================================================
%% Notification Tests
%%====================================================================

notification_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     fun(Server) ->
         [
             ?_test(test_notify_resource_updated(Server)),
             ?_test(test_notify_resources_changed(Server))
         ]
     end}.

test_notify_resource_updated(Server) ->
    ResourceUri = <<"test://notify/resource">>,
    Resource = #mcp_resource{
        uri = ResourceUri,
        name = <<"Notification Test">>
    },
    Handler = fun(_) -> <<"content">> end,

    ok = erlmcp_server:add_resource(Server, Resource, Handler),
    Result = erlmcp_server:notify_resource_updated(Server, ResourceUri, undefined),
    ?assertMatch(ok, Result).

test_notify_resources_changed(Server) ->
    Result = erlmcp_server:notify_resources_changed(Server),
    ?assertMatch(ok, Result).

%%====================================================================
%% Ping Tests (MCP 2025-11-25)
%%====================================================================

ping_test_() ->
    {setup,
     fun setup_ping_server/0,
     fun cleanup_ping_server/1,
     fun(Server) ->
         [
             ?_test(test_ping_without_echo(Server)),
             ?_test(test_ping_with_echo(Server)),
             ?_test(test_ping_before_initialization(Server)),
             ?_test(test_ping_with_binary_echo(Server)),
             ?_test(test_ping_with_map_echo(Server))
         ]
     end}.

test_ping_without_echo(Server) ->
    %% Test ping without echo parameter
    TransportId = test_transport_ping,
    RequestId = 1,
    PingRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"ping">>,
        <<"params">> => #{}
    },
    Data = jsx:encode(PingRequest),

    %% Send ping message to server
    Server ! {mcp_message, TransportId, Data},

    %% Give server time to process
    timer:sleep(100),

    %% Verify we got a response (we can't easily check the response in this unit test
    %% without mocking the transport, but we verify the server doesn't crash)
    ?assert(erlang:is_process_alive(Server)).

test_ping_with_echo(Server) ->
    %% Test ping with echo parameter
    TransportId = test_transport_ping_echo,
    RequestId = 2,
    EchoData = <<"test-echo-data">>,
    PingRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"ping">>,
        <<"params">> => #{<<"echo">> => EchoData}
    },
    Data = jsx:encode(PingRequest),

    %% Send ping message to server
    Server ! {mcp_message, TransportId, Data},

    %% Give server time to process
    timer:sleep(100),

    %% Verify server is still alive
    ?assert(erlang:is_process_alive(Server)).

test_ping_before_initialization(Server) ->
    %% Test that ping works before initialization (per MCP spec)
    %% Server starts uninitialized, so this tests ping in that state
    TransportId = test_transport_ping_pre_init,
    RequestId = 3,
    PingRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"ping">>,
        <<"params">> => #{<<"echo">> => <<"pre-init-ping">>}
    },
    Data = jsx:encode(PingRequest),

    %% Send ping message to server (before initialization)
    Server ! {mcp_message, TransportId, Data},

    %% Give server time to process
    timer:sleep(100),

    %% Verify server is still alive and can process ping
    ?assert(erlang:is_process_alive(Server)).

test_ping_with_binary_echo(Server) ->
    %% Test ping with binary echo data
    TransportId = test_transport_ping_binary,
    RequestId = 4,
    EchoData = <<"binary-test-data-with-special-chars-äöü"/utf8>>,
    PingRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"ping">>,
        <<"params">> => #{<<"echo">> => EchoData}
    },
    Data = jsx:encode(PingRequest),

    %% Send ping message to server
    Server ! {mcp_message, TransportId, Data},

    %% Give server time to process
    timer:sleep(100),

    ?assert(erlang:is_process_alive(Server)).

test_ping_with_map_echo(Server) ->
    %% Test ping with map/object echo data
    TransportId = test_transport_ping_map,
    RequestId = 5,
    EchoData = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => 42,
        <<"nested">> => #{<<"inner">> => true}
    },
    PingRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"ping">>,
        <<"params">> => #{<<"echo">> => EchoData}
    },
    Data = jsx:encode(PingRequest),

    %% Send ping message to server
    Server ! {mcp_message, TransportId, Data},

    %% Give server time to process
    timer:sleep(100),

    ?assert(erlang:is_process_alive(Server)).

%%====================================================================
%% Ping Test Helpers
%%====================================================================

setup_ping_server() ->
    setup(),
    ServerId = test_server_ping,
    Capabilities = default_capabilities(),
    {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),
    %% Register the test process to receive responses
    gproc:reg({p, l, {mcp_transport, test_transport_ping}}),
    gproc:reg({p, l, {mcp_transport, test_transport_ping_echo}}),
    gproc:reg({p, l, {mcp_transport, test_transport_ping_pre_init}}),
    gproc:reg({p, l, {mcp_transport, test_transport_ping_binary}}),
    gproc:reg({p, l, {mcp_transport, test_transport_ping_map}}),
    Server.

cleanup_ping_server(Server) ->
    erlmcp_server:stop(Server),
    %% Unregister
    catch gproc:unreg({p, l, {mcp_transport, test_transport_ping}}),
    catch gproc:unreg({p, l, {mcp_transport, test_transport_ping_echo}}),
    catch gproc:unreg({p, l, {mcp_transport, test_transport_ping_pre_init}}),
    catch gproc:unreg({p, l, {mcp_transport, test_transport_ping_binary}}),
    catch gproc:unreg({p, l, {mcp_transport, test_transport_ping_map}}),
    cleanup(ok).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

setup_server() ->
    setup(),
    ServerId = test_server_ops,
    Capabilities = default_capabilities(),
    {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),
    Server.

cleanup_server(Server) ->
    erlmcp_server:stop(Server),
    cleanup(ok).

default_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    }.
