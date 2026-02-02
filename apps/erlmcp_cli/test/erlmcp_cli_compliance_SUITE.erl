%%%-------------------------------------------------------------------
%%% @doc
%%% CLI MCP Compliance Test Suite (Common Test)
%%%
%%% MCP specification compliance tests for erlmcp_cli
%%%
%%% Chicago School TDD:
%%% - Specification validation
%%% - Real protocol testing
%%% - 100% spec coverage required
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_compliance_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Suite Callbacks
%%%====================================================================

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_cli),
    Config.

end_per_suite(Config) ->
    application:stop(erlmcp_cli),
    Config.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc JSON-RPC 2.0 specification compliance
json_rpc_2_compliance_test(_Config) ->
    %% Test: All requests must have jsonrpc version "2.0"
    Request1 = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 1
    }),
    {ok, Response1} = erlmcp_cli_json_rpc:handle_json_rpc(Request1, #{}, <<"test">>),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response1)),

    %% Test: Responses must have jsonrpc version "2.0"
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response1)),

    %% Test: Invalid jsonrpc version rejected
    Request2 = jsx:encode(#{
        <<"jsonrpc">> => <<"1.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 2
    }),
    {ok, Response2} = erlmcp_cli_json_rpc:handle_json_rpc(Request2, #{}, <<"test">>),
    Error2 = maps:get(<<"error">>, Response2),
    ?assertEqual(-32600, maps:get(<<"code">>, Error2)),

    ok.

%% @doc MCP protocol validation
mcp_protocol_compliance_test(_Config) ->
    %% Test: mcp.health command exists and returns expected format
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ?assert(is_map(Result)),
    ?assert(maps:get(<<"status">>, Result, undefined) =/= undefined),

    %% Test: MCP commands properly categorized
    {ok, Commands} = erlmcp_cli_registry:lookup_by_category(<<"mcp">>),
    ?assert(length(Commands) > 0),

    ok.

%% @doc Transport τ-interface compliance
transport_interface_compliance_test(_Config) ->
    %% Test: All transports implement required behavior callbacks
    %% init/2, send/2, close/1

    %% Initialize stdio transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"compliance-test">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Verify transport can send
    Message = <<"{\"test\":\"message\"}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Verify transport can close
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Verify transport closed
    ?assertNot(erlmcp_cli_transport:is_active(<<"stdio">>)),

    ok.

%% @doc Resource management compliance
resource_management_compliance_test(_Config) ->
    %% Test: Resources properly registered and tracked
    %% (This is a placeholder - actual resource tests depend on implementation)

    %% Verify metrics system tracks resource usage
    ok = erlmcp_cli_metrics:increment_counter(<<"resource.test">>, 1),
    Value = erlmcp_cli_metrics:get_counter_value(<<"resource.test">>),
    ?assertEqual(1, Value),

    ok.

%% @doc Tool execution compliance
tool_execution_compliance_test(_Config) ->
    %% Test: Tools can be called and return proper results
    %% (Placeholder for actual tool execution tests)

    %% Verify registry supports tool registration
    ToolCommand = #{
        name => <<"tools.test">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Test tool">>,
        category => <<"tools">>,
        safety_level => safe
    },
    ok = erlmcp_cli_registry:register_command(ToolCommand),

    %% Verify tool can be executed
    Result = erlmcp_cli_registry:execute_command(<<"tools.test">>, []),
    ?assert(is_map(Result)),

    ok.

%% @doc Session state mapping compliance
session_state_compliance_test(_Config) ->
    %% Test: Session states map correctly to MCP specification
    SessionId = <<"compliance-session">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Verify initial state
    {ok, State1} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(initialized, maps:get(status, State1)),

    %% Start session
    ok = erlmcp_cli_session:start_session(SessionId),
    {ok, State2} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(started, maps:get(status, State2)),

    %% Stop session
    ok = erlmcp_cli_session:stop_session(SessionId),
    {ok, State3} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(stopped, maps:get(status, State3)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId),

    ok.

%% @doc Error response format compliance
error_response_compliance_test(_Config) ->
    %% Test: Error responses follow JSON-RPC 2.0 spec
    %% Error object must have: code (number), message (string), data (optional)

    %% Trigger method not found error
    Request = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"nonexistent.method">>,
        <<"params">> => null,
        <<"id">> => 1
    }),

    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(Request, #{}, <<"test">>),
    Error = maps:get(<<"error">>, Response),

    %% Verify error object structure
    ?assert(is_integer(maps:get(<<"code">>, Error))),
    ?assert(is_binary(maps:get(<<"message">>, Error))),

    ok.

%% @doc Capability negotiation compliance
capability_negotiation_compliance_test(_Config) ->
    %% Test: Capabilities properly advertised and negotiated
    %% (Placeholder for actual capability tests)

    %% Verify system advertises capabilities
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ?assert(is_map(Result)),

    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

test_function(_Args) ->
    #{<<"result">> => <<"ok">>}.
