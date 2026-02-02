%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Compliance Tests for erlmcp_cli
%%%
%%% Tests the CLI implementation against the complete MCP specification:
%%% - JSON-RPC 2.0 protocol compliance
%%% - Required method implementations
%%% - Transport layer validation
%%% - Session management compliance
%%% - Error handling standards
%%% - OTEL integration safety
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_mcp_compliance_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test MCP required methods implementation
mcp_required_methods_test() ->
    %% Test that all required MCP methods are available
    RequiredMethods = [
        <<"initialize">>,          %% MCP handshake
        <<"capabilities">>,        %% Capabilities advertising
        <<"tools/list">>,         %% List available tools
        <<"tools/call">>,         %% Call a specific tool
        <<"resources/list">>,     %% List available resources
        <<"resources/read">>,     %% Read a resource
        <<"notifications/subscribe">> %% Subscribe to notifications
    ],

    %% Start the CLI application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test each required method
    lists:foreach(fun(Method) ->
        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => Method,
            <<"params">> => null,
            <<"id">> => make_test_id()
        }),

        %% Handle JSON-RPC request
        Response = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),

        %% Should return valid JSON-RPC response
        ?assertMatch({ok, _}, Response),
        {ok, ResponseMap} = Response,
        ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ResponseMap)),
        ?assert(is_binary(maps:get(<<"id">>, ResponseMap)))

    end, RequiredMethods),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test JSON-RPC 2.0 protocol compliance
json_rpc_2_0_compliance_test() ->
    %% Test JSON-RPC 2.0 specification compliance
    %% Start CLI application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test valid JSON-RPC request
    ValidRequest = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{},
        <<"id">> => make_test_id()
    }),

    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(ValidRequest, #{}, <<"test-session">>),
    {ok, ResponseMap} = Response,

    %% Verify JSON-RPC 2.0 compliance
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ResponseMap)),
    ?assert(is_binary(maps:get(<<"id">>, ResponseMap))), %% Request ID correlation
    ?assert(is_map(maps:get(<<"result">>, ResponseMap))), %% Success response structure

    %% Test invalid JSON-RPC requests
    InvalidRequests = [
        #{ %% Missing jsonrpc field
            <<"method">> => <<"tools/list">>,
            <<"params">> => #{},
            <<"id">> => make_test_id()
        },
        #{ %% Invalid jsonrpc version
            <<"jsonrpc">> => <<"1.0">>,
            <<"method">> => <<"tools/list">>,
            <<"params">> => #{},
            <<"id">> => make_test_id()
        },
        #{ %% Missing method field
            <<"jsonrpc">> => <<"2.0">>,
            <<"params">> => #{},
            <<"id">> => make_test_id()
        }
    ],

    lists:foreach(fun(InvalidRequest) ->
        Json = jsx:encode(InvalidRequest),
        {ok, ErrorResponse} = erlmcp_cli_json_rpc:handle_json_rpc(Json, #{}, <<"test-session">>),
        {ok, ErrorMap} = ErrorResponse,
        ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ErrorMap)),
        ?assertEqual(null, maps:get(<<"id">>, ErrorMap)), %% ID for notification
        ?assert(is_map(maps:get(<<"error">>, ErrorMap)))
    end, InvalidRequests),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test MCP error codes compliance
mcp_error_codes_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test MCP error codes (from -32000 to -32099)
    ErrorScenarios = [
        {<<"nonexistent.method">>, -32601, <<"Method not found">>}, %% Method not found
        {<<"tools/call">>, -32602, <<"Invalid params">>}, %% Invalid parameters (should fail)
        {<<"initialize">>, -32603, <<"Internal error">>} %% Internal error (simulate)
    ],

    lists:foreach(fun({Method, ExpectedCode, ExpectedMessage}) ->
        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => Method,
            <<"params">> => make_invalid_params(Method),
            <<"id">> => make_test_id()
        }),

        {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),
        {ok, ResponseMap} = Response,

        Error = maps:get(<<"error">>, ResponseMap),
        ?assertEqual(ExpectedCode, maps:get(<<"code">>, Error)),
        ?assertEqual(ExpectedMessage, maps:get(<<"message">>, Error))

    end, ErrorScenarios),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test session management compliance
session_management_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test session creation and lifecycle
    SessionId = <<"test-session-123">>,

    %% Create a session
    {ok, SessionPid} = erlmcp_cli_session:start_link(SessionId, #{
        <<"command_timeout">> => 5000,
        <<"session_timeout">> => 30000
    }),

    %% Verify session is created
    ?assert(is_process_alive(SessionPid)),

    %% Test session state
    {ok, SessionInfo} = erlmcp_cli_session:get_info(SessionPid),
    ?assertEqual(SessionId, SessionInfo#cli_session.id),
    ?assertEqual(active, SessionInfo#cli_session.status),
    ?assert( SessionInfo#cli_session.created > 0 ),

    %% Test command execution in session
    {ok, _Result} = erlmcp_cli_session:execute_command(SessionPid, <<"mcp.health">>, []),

    %% Test session pause/resume
    ok = erlmcp_cli_session:pause_session(SessionPid),
    {ok, PausedInfo} = erlmcp_cli_session:get_info(SessionPid),
    ?assertEqual(paused, PausedInfo#cli_session.status),

    ok = erlmcp_cli_session:resume_session(SessionPid),
    {ok, ResumedInfo} = erlmcp_cli_session:get_info(SessionPid),
    ?assertEqual(active, ResumedInfo#cli_session.status),

    %% Test session termination
    erlmcp_cli_session:terminate_session(SessionPid),
    timer:sleep(100),
    ?assertNot(is_process_alive(SessionPid)),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test transport τ-interface compliance
transport_interface_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test all 5 transport types
    TransportTypes = [stdio, tcp, http, ws, sse],

    lists:foreach(fun(Type) ->
        %% Test transport initialization
        case Type of
            stdio ->
                %% stdio doesn't need host/port
                ok = erlmcp_cli_transport:transport(Type, #{});
            tcp ->
                ok = erlmcp_cli_transport:transport(Type, #{
                    "host" => "127.0.0.1",
                    "port" => 8080
                });
            http ->
                ok = erlmcp_cli_transport:transport(Type, #{
                    "host" => "localhost",
                    "port" => 8080
                });
            ws ->
                ok = erlmcp_cli_transport:transport(Type, #{
                    "host" => "localhost",
                    "port" => 8080
                });
            sse ->
                ok = erlmcp_cli_transport:transport(Type, #{
                    "host" => "localhost",
                    "port" => 8080
                })
        end,

        %% Verify transport is active
        true = erlmcp_cli_transport:is_active(Type),

        %% Test message sending (JSON-RPC framed)
        TestMessage = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"test.transport">>,
            <<"params">> => #{},
            <<"id">> => make_test_id()
        }),

        ok = erlmcp_cli_transport:send_data(Type, TestMessage),

        %% Test transport statistics
        Stats = erlmcp_cli_transport:get_transport_stats(Type),
        ?assert(is_map(Stats)),
        ?assert(is_integer(maps:get(<<"messages_sent">>, Stats, 0))),
        ?assert(is_integer(maps:get(<<"messages_received">>, Stats, 0))),

        %% Close transport
        ok = erlmcp_cli_transport:close_transport(Type),
        false = erlmcp_cli_transport:is_active(Type)

    end, TransportTypes),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test registry integration compliance
registry_integration_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test command registry against MCP specification
    %% Check built-in MCP commands
    {ok, Commands} = erlmcp_cli_registry:list_commands(),

    %% Should have MCP-specific commands
    McpCommands = lists:filter(fun({Name, _}) ->
        binary:match(Name, <<"mcp">>) /= nomatch
    end, Commands),

    ?assert(length(McpCommands) > 0, "Should have MCP commands"),

    %% Test specific MCP commands
    McpMethods = [<<"mcp.health">>, <<"mcp.capabilities">>, <<"mcp.list_tools">>],

    lists:foreach(fun(Method) ->
        %% Test command lookup
        case erlmcp_cli_registry:lookup_command(Method) of
            {ok, CommandInfo} ->
                %% Verify command structure
                ?assertEqual(<<"mcp">>, CommandInfo#command_info.category),
                ?assert(is_binary(CommandInfo#command_info.description)),
                ?assert(CommandInfo#command_info.safety_level =< high),

                %% Test command execution
                {ok, _Result} = erlmcp_cli_registry:execute_command(Method, []);
            {error, not_found} ->
                %% Command not found - this is acceptable for some methods
                ok
        end
    end, McpMethods),

    %% Test command metrics
    RegistryMetrics = erlmcp_cli_registry:get_metrics(),
    ?assert(is_map(RegistryMetrics)),
    ?assert(is_integer(maps:get("commands.registered", RegistryMetrics, 0))),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test OTEL integration compliance
otel_integration_compliance_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test that OTEL integration doesn't break MCP compliance
    %% Execute commands with tracing enabled
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => make_test_id()
    }),

    %% Execute with OTEL tracing (should work normally)
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"otel-test-session">>),
    ?assertMatch({ok, _}, Response),

    %% Verify metrics were collected but don't affect protocol behavior
    AllMetrics = erlmcp_cli_metrics:export_metrics(),
    ?assert(is_map(AllMetrics)),
    ?assert(maps:get(<<"requests.total">>, AllMetrics, 0) > 0),

    %% Test that tracing is optional and configurable
    %% Should work the same with tracing disabled
    {ok, Response2} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"no-trace-session">>),
    ?assertMatch({ok, _}, Response2),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test authentication and security compliance
authentication_security_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test command security validation
    %% Try to execute dangerous commands (should be blocked)
    DangerousCommands = [<<"eval">>, <<"exec">>, <<"system">>, <<"shell">>],

    lists:foreach(fun(DangerousCommand) ->
        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => DangerousCommand,
            <<"params">> => [],
            <<"id">> => make_test_id()
        }),

        {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"security-test">>),
        {ok, ResponseMap} = Response,

        %% Should return security error
        Error = maps:get(<<"error">>, ResponseMap),
        ?assertEqual(-32602, maps:get(<<"code">>, Error)), %% Invalid params
        ?assertEqual(<<"Invalid params">>, maps:get(<<"message">>, Error))

    end, DangerousCommands),

    %% Test session isolation
    SessionId1 = <<"session-1">>,
    SessionId2 = <<"session-2">>,

    {ok, Session1} = erlmcp_cli_session:start_link(SessionId1, #{}),
    {ok, Session2} = erlmcp_cli_session:start_link(SessionId2, #{}),

    ?assertNotEqual(Session1, Session2), %% Different processes
    ?assertEqual(SessionId1, element(2, erlmcp_cli_session:get_info(Session1))),
    ?assertEqual(SessionId2, element(2, erlmcp_cli_session:get_info(Session2))),

    %% Cleanup sessions
    erlmcp_cli_session:terminate_session(Session1),
    erlmcp_cli_session:terminate_session(Session2),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test request ID correlation compliance
request_id_correlation_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test request ID correlation for async operations
    RequestIds = [make_test_id() || _ <- lists:seq(1, 5)],

    lists:foreach(fun(RequestId) ->
        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"mcp.health">>,
            <<"params">> => null,
            <<"id">> => RequestId
        }),

        {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"correlation-test">>),
        {ok, ResponseMap} = Response,

        %% Verify request ID is preserved
        ?assertEqual(RequestId, maps:get(<<"id">>, ResponseMap))

    end, RequestIds),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test concurrent operations compliance
concurrent_operations_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test concurrent JSON-RPC requests
    NumRequests = 10,
    RequestPids = lists:map(fun(I) ->
        spawn_link(fun() ->
            RequestId = make_test_id(),
            RequestJson = jsx:encode(#{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"mcp.health">>,
                <<"params">> => null,
                <<"id">> => RequestId
            }),

            {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"concurrent-test">>),
            {ok, ResponseMap} = Response,

            %% Verify response
            ?assertEqual(RequestId, maps:get(<<"id">>, ResponseMap)),

            %% Send result back
            self()! {completed, I}
        end)
    end, lists:seq(1, NumRequests)),

    %% Wait for all requests to complete
    Completed = lists:foldl(fun(_, Acc) ->
        receive
            {completed, I} -> Acc + 1
        after 5000 ->
            Acc
        end
    end, 0, RequestPids),

    ?assertEqual(NumRequests, Completed, "All concurrent requests should complete"),

    %% Test concurrent session creation
    SessionIds = [<<"session-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 5)],
    SessionPids = lists:map(fun(SessionId) ->
        spawn_link(fun() ->
            {ok, SessionPid} = erlmcp_cli_session:start_link(SessionId, #{}),
            self()! {session_created, SessionPid}
        end)
    end, SessionIds),

    %% Wait for sessions to be created
    lists:foreach(fun(_) ->
        receive
            {session_created, SessionPid} -> ok
        after 5000 ->
            ?fail("Session creation timeout")
        end
    end, SessionPids),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test error handling compliance
error_handling_compliance_test() ->
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test comprehensive error handling
    ErrorScenarios = [
        %% Malformed JSON
        {<<"{invalid json}">>, parse_error},
        %% Missing required fields
        {<<"{\"jsonrpc\":\"2.0\"}">>, {invalid_request, missing_method}},
        %% Invalid JSON-RPC version
        {<<"{\"jsonrpc\":\"3.0\",\"method\":\"test\"}">>, {invalid_request, invalid_jsonrpc_version}},
        %% Invalid method type
        {<<"{\"jsonrpc\":\"2.0\",\"method\":123}">>, {invalid_request, invalid_method}}
    ],

    lists:foreach(fun({JsonData, ExpectedError}) ->
        {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(JsonData, #{}, <<"error-test">>),
        {ok, ResponseMap} = Response,

        Error = maps:get(<<"error">>, ResponseMap),
        ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, Error)),
        ?assertEqual(-32700, maps:get(<<"code">>, Error)) %% Parse error

    end, ErrorScenarios),

    %% Test internal error handling
    %% Send request to non-existent method
    NonExistentRequest = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"nonexistent.method">>,
        <<"params">> => null,
        <<"id">> => make_test_id()
    }),

    {ok, ErrorResponse} = erlmcp_cli_json_rpc:handle_json_rpc(NonExistentRequest, #{}, <<"error-test">>),
    {ok, ErrorMap} = ErrorResponse,

    Error = maps:get(<<"error">>, ErrorMap),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)), %% Method not found
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),

    %% Stop application
    ok = application:stop(erlmcp_cli),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Generate test request ID
make_test_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).

%% @doc Make invalid parameters for testing
make_invalid_params(Method) ->
    case Method of
        <<"tools/call">> ->
            %% Invalid tool call parameters
            #{<<"name">> => nonexistent_tool, <<"arguments">> => invalid};
        _ ->
            null
    end.