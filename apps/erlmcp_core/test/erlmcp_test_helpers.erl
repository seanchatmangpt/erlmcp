-module(erlmcp_test_helpers).

%%%====================================================================
%%% Test Helpers for MCP 2025-11-25 Compliance Testing
%%%====================================================================
%%% Chicago School TDD:
%%% - Real processes (no mocks)
%%% - State-based verification
%%% - Behavior verification
%%%
%%% This module provides common utilities for MCP testing:
%%% - Server setup/teardown
%%% - Message builders (requests, responses, notifications)
%%% - Custom assertions
%%% - Sample data generators
%%% - Transport simulation

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Setup/Teardown
-export([
    setup_server/0,
    setup_server/1,
    setup_client/0,
    setup_client/1,
    cleanup_server/1,
    cleanup_client/1
]).

%% Message Builders
-export([
    build_initialize_request/0,
    build_initialize_request/1,
    build_initialize_response/1,
    build_resources_list_request/0,
    build_resources_read_request/1,
    build_resources_subscribe_request/1,
    build_tools_list_request/0,
    build_tools_call_request/2,
    build_tools_call_request/3,
    build_prompts_list_request/0,
    build_prompts_get_request/2,
    build_notification/2,
    build_error_response/3
]).

%% Sample Data Generators
-export([
    sample_resource/0,
    sample_resource/1,
    sample_resource_template/0,
    sample_tool/0,
    sample_tool/1,
    sample_prompt/0,
    sample_prompt/1,
    sample_capabilities/0,
    sample_client_capabilities/0
]).

%% Custom Assertions
-export([
    assert_server_initialized/1,
    assert_server_phase/2,
    assert_resource_exists/2,
    assert_tool_exists/2,
    assert_prompt_exists/2,
    assert_error_code/2,
    assert_error_code/3,
    assert_json_rpc_response/1,
    assert_json_rpc_error/1,
    assert_capability_present/2
]).

%% Transport Simulation
-export([
    start_mock_transport/0,
    stop_mock_transport/1,
    send_to_transport/2,
    receive_from_transport/1
]).

%% Utility Functions
-export([
    wait_for_condition/2,
    wait_for_condition/3,
    generate_request_id/0,
    encode_message/1,
    decode_message/1
]).

%%====================================================================
%% Setup/Teardown Functions
%%====================================================================

%% @doc Setup a test server with default capabilities
-spec setup_server() -> pid().
setup_server() ->
    setup_server(#{}).

%% @doc Setup a test server with custom configuration
-spec setup_server(map()) -> pid().
setup_server(Config) ->
    ServerId = maps:get(server_id, Config, test_server),
    Capabilities = maps:get(capabilities, Config, sample_capabilities()),
    {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),
    Server.

%% @doc Setup a test client with default transport
-spec setup_client() -> pid().
setup_client() ->
    setup_client(#{}).

%% @doc Setup a test client with custom configuration
-spec setup_client(map()) -> pid().
setup_client(Config) ->
    Transport = maps:get(transport, Config, {stdio, []}),
    Options = maps:get(options, Config, #{}),
    {ok, Client} = erlmcp_client:start_link(Transport, Options),
    Client.

%% @doc Cleanup test server
-spec cleanup_server(pid()) -> ok.
cleanup_server(Server) when is_pid(Server) ->
    case erlang:is_process_alive(Server) of
        true ->
            erlmcp_server:stop(Server),
            timer:sleep(50);  % Allow cleanup to complete
        false ->
            ok
    end.

%% @doc Cleanup test client
-spec cleanup_client(pid()) -> ok.
cleanup_client(Client) when is_pid(Client) ->
    case erlang:is_process_alive(Client) of
        true ->
            erlmcp_client:stop(Client),
            timer:sleep(50);
        false ->
            ok
    end.

%%====================================================================
%% Message Builders
%%====================================================================

%% @doc Build a standard initialize request with default params
-spec build_initialize_request() -> map().
build_initialize_request() ->
    build_initialize_request(#{}).

%% @doc Build an initialize request with custom params
-spec build_initialize_request(map()) -> map().
build_initialize_request(Params) ->
    ProtocolVersion = maps:get(protocol_version, Params, <<"2025-11-25">>),
    ClientCapabilities = maps:get(client_capabilities, Params, #{}),
    ClientInfo = maps:get(client_info, Params, #{
        <<"name">> => <<"test-client">>,
        <<"version">> => <<"1.0.0">>
    }),
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => maps:get(id, Params, 1),
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => ProtocolVersion,
            <<"capabilities">> => ClientCapabilities,
            <<"clientInfo">> => ClientInfo
        }
    }.

%% @doc Build an initialize response with server capabilities
-spec build_initialize_response(map()) -> map().
build_initialize_response(Capabilities) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{
            <<"protocolVersion">> => <<"2025-11-25">>,
            <<"capabilities">> => Capabilities,
            <<"serverInfo">> => #{
                <<"name">> => <<"erlmcp">>,
                <<"version">> => <<"0.6.0">>
            }
        }
    }.

%% @doc Build resources/list request
-spec build_resources_list_request() -> map().
build_resources_list_request() ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"resources/list">>,
        <<"params">> => #{}
    }.

%% @doc Build resources/read request
-spec build_resources_read_request(binary()) -> map().
build_resources_read_request(Uri) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"resources/read">>,
        <<"params">> => #{<<"uri">> => Uri}
    }.

%% @doc Build resources/subscribe request
-spec build_resources_subscribe_request(binary()) -> map().
build_resources_subscribe_request(Uri) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"resources/subscribe">>,
        <<"params">> => #{<<"uri">> => Uri}
    }.

%% @doc Build tools/list request
-spec build_tools_list_request() -> map().
build_tools_list_request() ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{}
    }.

%% @doc Build tools/call request with name only
-spec build_tools_call_request(binary(), map()) -> map().
build_tools_call_request(Name, Arguments) ->
    build_tools_call_request(generate_request_id(), Name, Arguments).

%% @doc Build tools/call request with custom ID
-spec build_tools_call_request(integer() | binary(), binary(), map()) -> map().
build_tools_call_request(Id, Name, Arguments) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => Name,
            <<"arguments">> => Arguments
        }
    }.

%% @doc Build prompts/list request
-spec build_prompts_list_request() -> map().
build_prompts_list_request() ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"prompts/list">>,
        <<"params">> => #{}
    }.

%% @doc Build prompts/get request
-spec build_prompts_get_request(binary(), map()) -> map().
build_prompts_get_request(Name, Arguments) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"prompts/get">>,
        <<"params">> => #{
            <<"name">> => Name,
            <<"arguments">> => Arguments
        }
    }.

%% @doc Build notification
-spec build_notification(binary(), map()) -> map().
build_notification(Method, Params) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc Build error response
-spec build_error_response(integer() | binary(), integer(), binary()) -> map().
build_error_response(Id, Code, Message) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    }.

%%====================================================================
%% Sample Data Generators
%%====================================================================

%% @doc Generate a sample resource
-spec sample_resource() -> map().
sample_resource() ->
    sample_resource(#{}).

%% @doc Generate a sample resource with custom fields
-spec sample_resource(map()) -> map().
sample_resource(Overrides) ->
    Default = #{
        uri => <<"test://resource/1">>,
        name => <<"Test Resource">>,
        description => <<"A test resource">>,
        mime_type => <<"text/plain">>
    },
    maps:merge(Default, Overrides).

%% @doc Generate a sample resource template
-spec sample_resource_template() -> map().
sample_resource_template() ->
    #{
        uri_template => <<"test://resource/{id}">>,
        name => <<"Test Resource Template">>,
        description => <<"A test resource template">>,
        mime_type => <<"text/plain">>
    }.

%% @doc Generate a sample tool
-spec sample_tool() -> map().
sample_tool() ->
    sample_tool(#{}).

%% @doc Generate a sample tool with custom fields
-spec sample_tool(map()) -> map().
sample_tool(Overrides) ->
    Default = #{
        name => <<"test_tool">>,
        description => <<"A test tool">>,
        input_schema => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"input">> => #{<<"type">> => <<"string">>}
            }
        }
    },
    maps:merge(Default, Overrides).

%% @doc Generate a sample prompt
-spec sample_prompt() -> map().
sample_prompt() ->
    sample_prompt(#{}).

%% @doc Generate a sample prompt with custom fields
-spec sample_prompt(map()) -> map().
sample_prompt(Overrides) ->
    Default = #{
        name => <<"test_prompt">>,
        description => <<"A test prompt">>,
        arguments => []
    },
    maps:merge(Default, Overrides).

%% @doc Generate sample server capabilities
-spec sample_capabilities() -> #mcp_server_capabilities{}.
sample_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    }.

%% @doc Generate sample client capabilities
-spec sample_client_capabilities() -> #mcp_client_capabilities{}.
sample_client_capabilities() ->
    #mcp_client_capabilities{
        roots = #mcp_capability{enabled = false},
        sampling = #mcp_capability{enabled = false}
    }.

%%====================================================================
%% Custom Assertions
%%====================================================================

%% @doc Assert server is initialized
-spec assert_server_initialized(pid()) -> ok.
assert_server_initialized(Server) ->
    {ok, State} = gen_server:call(Server, get_state),
    ?assert(State#state.initialized),
    ok.

%% @doc Assert server is in expected phase
-spec assert_server_phase(pid(), atom()) -> ok.
assert_server_phase(Server, ExpectedPhase) ->
    {ok, State} = gen_server:call(Server, get_state),
    ?assertEqual(ExpectedPhase, State#state.phase),
    ok.

%% @doc Assert resource exists in server
-spec assert_resource_exists(pid(), binary()) -> ok.
assert_resource_exists(Server, Uri) ->
    {ok, State} = gen_server:call(Server, get_state),
    Resources = State#state.resources,
    ?assert(maps:is_key(Uri, Resources)),
    ok.

%% @doc Assert tool exists in server
-spec assert_tool_exists(pid(), binary()) -> ok.
assert_tool_exists(Server, Name) ->
    {ok, State} = gen_server:call(Server, get_state),
    Tools = State#state.tools,
    ?assert(maps:is_key(Name, Tools)),
    ok.

%% @doc Assert prompt exists in server
-spec assert_prompt_exists(pid(), binary()) -> ok.
assert_prompt_exists(Server, Name) ->
    {ok, State} = gen_server:call(Server, get_state),
    Prompts = State#state.prompts,
    ?assert(maps:is_key(Name, Prompts)),
    ok.

%% @doc Assert response has expected error code
-spec assert_error_code(map(), integer()) -> ok.
assert_error_code(Response, ExpectedCode) ->
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(ExpectedCode, maps:get(<<"code">>, Error)),
    ok.

%% @doc Assert response has expected error code and message
-spec assert_error_code(map(), integer(), binary()) -> ok.
assert_error_code(Response, ExpectedCode, ExpectedMessage) ->
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(ExpectedCode, maps:get(<<"code">>, Error)),
    ?assertEqual(ExpectedMessage, maps:get(<<"message">>, Error)),
    ok.

%% @doc Assert response is valid JSON-RPC 2.0 response
-spec assert_json_rpc_response(map()) -> ok.
assert_json_rpc_response(Response) ->
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assert(maps:is_key(<<"id">>, Response)),
    ?assert(maps:is_key(<<"result">>, Response) xor maps:is_key(<<"error">>, Response)),
    ok.

%% @doc Assert response is valid JSON-RPC 2.0 error
-spec assert_json_rpc_error(map()) -> ok.
assert_json_rpc_error(Response) ->
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assert(maps:is_key(<<"id">>, Response)),
    ?assert(maps:is_key(<<"error">>, Response)),
    ?assertNot(maps:is_key(<<"result">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assert(maps:is_key(<<"code">>, Error)),
    ?assert(maps:is_key(<<"message">>, Error)),
    ok.

%% @doc Assert capability is present in capabilities object
-spec assert_capability_present(map(), binary()) -> ok.
assert_capability_present(Capabilities, CapabilityName) ->
    ?assert(maps:is_key(CapabilityName, Capabilities)),
    Capability = maps:get(CapabilityName, Capabilities),
    ?assert(maps:is_key(<<"enabled">>, Capability)),
    ok.

%%====================================================================
%% Transport Simulation (Chicago School: Real Process)
%%====================================================================

%% @doc Start a mock transport process (real process, not a mock)
-spec start_mock_transport() -> {ok, pid()}.
start_mock_transport() ->
    Pid = spawn(fun() -> mock_transport_loop(#{messages => [], pending => #{}}) end),
    {ok, Pid}.

%% @doc Mock transport loop
mock_transport_loop(State) ->
    receive
        {send_data, Data} ->
            Messages = maps:get(messages, State, []),
            NewState = State#{messages := [Data | Messages]},
            mock_transport_loop(NewState);
        {get_messages, From} ->
            Messages = maps:get(messages, State, []),
            From ! {messages, lists:reverse(Messages)},
            mock_transport_loop(State);
        {clear_messages, From} ->
            From ! ok,
            mock_transport_loop(State#{messages => []});
        stop ->
            ok;
        _Other ->
            mock_transport_loop(State)
    end.

%% @doc Stop mock transport
-spec stop_mock_transport(pid()) -> ok.
stop_mock_transport(Pid) when is_pid(Pid) ->
    Pid ! stop,
    timer:sleep(10),
    ok.

%% @doc Send data to mock transport
-spec send_to_transport(pid(), binary()) -> ok.
send_to_transport(Pid, Data) ->
    Pid ! {send_data, Data},
    ok.

%% @doc Receive messages from mock transport
-spec receive_from_transport(pid()) -> [binary()].
receive_from_transport(Pid) ->
    Pid ! {get_messages, self()},
    receive
        {messages, Messages} -> Messages
    after 1000 ->
        []
    end.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Wait for condition to be true (default 5s timeout)
-spec wait_for_condition(fun(() -> boolean()), binary()) -> ok | {error, timeout}.
wait_for_condition(ConditionFun, Description) ->
    wait_for_condition(ConditionFun, Description, 5000).

%% @doc Wait for condition to be true with custom timeout
-spec wait_for_condition(fun(() -> boolean()), binary(), pos_integer()) -> ok | {error, timeout}.
wait_for_condition(ConditionFun, Description, TimeoutMs) ->
    EndTime = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_condition_loop(ConditionFun, Description, EndTime).

wait_for_condition_loop(ConditionFun, Description, EndTime) ->
    case ConditionFun() of
        true ->
            ok;
        false ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= EndTime of
                true ->
                    {error, {timeout, Description}};
                false ->
                    timer:sleep(50),
                    wait_for_condition_loop(ConditionFun, Description, EndTime)
            end
    end.

%% @doc Generate unique request ID
-spec generate_request_id() -> integer().
generate_request_id() ->
    erlang:system_time(microsecond).

%% @doc Encode message to binary
-spec encode_message(map()) -> binary().
encode_message(Message) ->
    jsx:encode(Message).

%% @doc Decode message from binary
-spec decode_message(binary()) -> map().
decode_message(Binary) ->
    jsx:decode(Binary, [return_maps]).

%%====================================================================
%% End of Module
%%====================================================================
