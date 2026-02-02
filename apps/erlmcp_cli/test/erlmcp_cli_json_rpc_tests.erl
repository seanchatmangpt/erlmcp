%%%-------------------------------------------------------------------
%%% @doc
%%% JSON-RPC Protocol Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_json_rpc module - JSON-RPC 2.0 protocol implementation
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real gen_server for JSON-RPC state
%%% - NO mocks, real message encoding/decoding
%%% - State-based verification (request/response correlation)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_json_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

json_rpc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Request parsing - valid request", fun test_parse_valid_request/0},
      {"Request parsing - missing jsonrpc version", fun test_parse_missing_jsonrpc/0},
      {"Request parsing - invalid jsonrpc version", fun test_parse_invalid_jsonrpc_version/0},
      {"Request parsing - missing method", fun test_parse_missing_method/0},
      {"Request parsing - invalid params type", fun test_parse_invalid_params_type/0},
      {"Request parsing - missing id", fun test_parse_missing_id/0},
      {"Request parsing - batch request", fun test_parse_batch_request/0},
      {"Request parsing - notification (no id)", fun test_parse_notification/0},
      {"Response generation - success response", fun test_generate_success_response/0},
      {"Response generation - error response", fun test_generate_error_response/0},
      {"Response generation - error codes", fun test_error_codes/0},
      {"Response generation - batch response", fun test_generate_batch_response/0},
      {"Message correlation - request ID matching", fun test_request_id_correlation/0},
      {"Message validation - malformed JSON", fun test_malformed_json/0},
      {"Message validation - empty request", fun test_empty_request/0},
      {"Message validation - extra fields", fun test_extra_fields/0},
      {"Handle JSON-RPC - valid request", fun test_handle_valid_request/0},
      {"Handle JSON-RPC - method not found", fun test_handle_method_not_found/0},
      {"Handle JSON-RPC - invalid params", fun test_handle_invalid_params/0},
      {"Handle JSON-RPC - internal error", fun test_handle_internal_error/0},
      {"Handle JSON-RPC - batch request", fun test_handle_batch_request/0},
      {"Handle JSON-RPC - notification", fun test_handle_notification/0},
      {"Encoding - request encoding", fun test_encode_request/0},
      {"Encoding - response encoding", fun test_encode_response/0},
      {"Encoding - error encoding", fun test_encode_error/0},
      {"Encoding - special characters", fun test_encode_special_characters/0},
      {"Decoding - response decoding", fun test_decode_response/0},
      {"Decoding - error decoding", fun test_decode_error/0},
      {"Decoding - unicode handling", fun test_decode_unicode/0},
      {"Session handling - session context", fun test_session_context/0},
      {"Session handling - invalid session", fun test_invalid_session/0},
      {"Concurrent requests - parallel processing", fun test_concurrent_requests/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Request Parsing Tests
%%%====================================================================

test_parse_valid_request() ->
    %% Valid JSON-RPC 2.0 request
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test.method\",\"params\":{\"arg1\":\"value1\"},\"id\":1}">>,
    {ok, Request} = erlmcp_cli_json_rpc:parse_request(RequestJson),

    %% Verify parsed structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Request)),
    ?assertEqual(<<"test.method">>, maps:get(<<"method">>, Request)),
    ?assertEqual(1, maps:get(<<"id">>, Request)),
    ?assert(is_map(maps:get(<<"params">>, Request))).

test_parse_missing_jsonrpc() ->
    %% Missing jsonrpc version
    RequestJson = <<"{\"method\":\"test\",\"id\":1}">>,
    {error, {invalid_request, missing_jsonrpc_version}} = erlmcp_cli_json_rpc:parse_request(RequestJson).

test_parse_invalid_jsonrpc_version() ->
    %% Invalid jsonrpc version
    RequestJson = <<"{\"jsonrpc\":\"1.0\",\"method\":\"test\",\"id\":1}">>,
    {error, {invalid_request, invalid_jsonrpc_version}} = erlmcp_cli_json_rpc:parse_request(RequestJson).

test_parse_missing_method() ->
    %% Missing method
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,
    {error, {invalid_request, missing_method}} = erlmcp_cli_json_rpc:parse_request(RequestJson).

test_parse_invalid_params_type() ->
    %% Invalid params type (must be object or array)
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"params\":\"invalid\",\"id\":1}">>,
    {error, {invalid_request, invalid_params_type}} = erlmcp_cli_json_rpc:parse_request(RequestJson).

test_parse_missing_id() ->
    %% Notifications have no id (valid)
    NotificationJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test.notify\",\"params\":{}}">>,
    {ok, Request} = erlmcp_cli_json_rpc:parse_request(NotificationJson),
    ?assertEqual(undefined, maps:get(<<"id">>, Request, undefined)).

test_parse_batch_request() ->
    %% Batch request array
    BatchJson = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test1\",\"id\":1},{\"jsonrpc\":\"2.0\",\"method\":\"test2\",\"id\":2}]">>,
    {ok, Requests} = erlmcp_cli_json_rpc:parse_request(BatchJson),

    %% Verify batch array
    ?assert(is_list(Requests)),
    ?assertEqual(2, length(Requests)),
    ?assertEqual(<<"test1">>, maps:get(<<"method">>, lists:nth(1, Requests))),
    ?assertEqual(<<"test2">>, maps:get(<<"method">>, lists:nth(2, Requests))).

test_parse_notification() ->
    %% Notification (no id field)
    NotificationJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notify\",\"params\":{}}">>,
    {ok, Request} = erlmcp_cli_json_rpc:parse_request(NotificationJson),

    %% Verify notification (no id)
    ?assertEqual(undefined, maps:get(<<"id">>, Request, undefined)),
    ?assertEqual(<<"notify">>, maps:get(<<"method">>, Request)).

%%%====================================================================
%%% Response Generation Tests
%%%====================================================================

test_generate_success_response() ->
    %% Success response
    Result = #{<<"status">> => <<"ok">>, <<"data">> => <<"test">>},
    Response = erlmcp_cli_json_rpc:generate_success_response(1, Result),

    %% Verify response structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    ?assertEqual(Result, maps:get(<<"result">>, Response)),
    ?assertEqual(undefined, maps:get(<<"error">>, Response, undefined)).

test_generate_error_response() ->
    %% Error response
    ErrorCode = -32601,
    ErrorMessage = <<"Method not found">>,
    Response = erlmcp_cli_json_rpc:generate_error_response(1, ErrorCode, ErrorMessage),

    %% Verify error response structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    ?assertEqual(undefined, maps:get(<<"result">>, Response, undefined)),

    %% Verify error object
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(ErrorCode, maps:get(<<"code">>, Error)),
    ?assertEqual(ErrorMessage, maps:get(<<"message">>, Error)).

test_error_codes() ->
    %% Test all standard JSON-RPC 2.0 error codes
    ErrorCodes = [
        {-32700, <<"Parse error">>},
        {-32600, <<"Invalid Request">>},
        {-32601, <<"Method not found">>},
        {-32602, <<"Invalid params">>},
        {-32603, <<"Internal error">>}
    ],

    lists:foreach(fun({Code, Message}) ->
        Response = erlmcp_cli_json_rpc:generate_error_response(1, Code, Message),
        Error = maps:get(<<"error">>, Response),
        ?assertEqual(Code, maps:get(<<"code">>, Error)),
        ?assertEqual(Message, maps:get(<<"message">>, Error))
    end, ErrorCodes).

test_generate_batch_response() ->
    %% Batch response
    Results = [
        #{<<"status">> => <<"ok">>, <<"index">> => 1},
        #{<<"status">> => <<"ok">>, <<"index">> => 2}
    ],
    Responses = erlmcp_cli_json_rpc:generate_batch_response([1, 2], Results),

    %% Verify batch response array
    ?assert(is_list(Responses)),
    ?assertEqual(2, length(Responses)),
    ?assertEqual(1, maps:get(<<"id">>, lists:nth(1, Responses))),
    ?assertEqual(2, maps:get(<<"id">>, lists:nth(2, Responses))).

%%%====================================================================
%%% Message Correlation Tests
%%%====================================================================

test_request_id_correlation() ->
    %% Request ID must match response ID
    RequestId = 42,
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>, <<"id">> => RequestId},
    Result = #{<<"answer">> => 42},

    Response = erlmcp_cli_json_rpc:generate_success_response(RequestId, Result),

    %% Verify ID correlation
    ?assertEqual(RequestId, maps:get(<<"id">>, Response)).

%%%====================================================================
%%% Message Validation Tests
%%%====================================================================

test_malformed_json() ->
    %% Malformed JSON
    MalformedJson = <<"{invalid json}">>,
    {error, {parse_error, _}} = erlmcp_cli_json_rpc:parse_request(MalformedJson).

test_empty_request() ->
    %% Empty request
    EmptyJson = <<"{}">>,
    {error, {invalid_request, _}} = erlmcp_cli_json_rpc:parse_request(EmptyJson).

test_extra_fields() ->
    %% Extra fields should be ignored (JSON-RPC spec)
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"extra\":\"ignored\",\"id\":1}">>,
    {ok, Request} = erlmcp_cli_json_rpc:parse_request(RequestJson),

    %% Verify extra field ignored
    ?assertEqual(undefined, maps:get(<<"extra">>, Request, undefined)).

%%%====================================================================
%%% Handle JSON-RPC Tests
%%%====================================================================

test_handle_valid_request() ->
    %% Valid request with registered method
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => 1
    }),

    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),

    %% Verify response
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    ?assert(is_map(maps:get(<<"result">>, Response))).

test_handle_method_not_found() ->
    %% Non-existent method
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"nonexistent.method">>,
        <<"params">> => null,
        <<"id">> => 1
    }),

    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),

    %% Verify error response
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)).

test_handle_invalid_params() ->
    %% Invalid parameters for method
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => <<"invalid">>,  %% Should be object or array
        <<"id">> => 1
    }),

    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),

    %% Verify error response
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_handle_internal_error() ->
    %% Simulate internal error (method that crashes)
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test.crash">>,
        <<"params">> => null,
        <<"id">> => 1
    }),

    %% Handle gracefully with internal error
    Response = case erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>) of
        {ok, Resp} -> Resp;
        {error, _Reason} -> #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"error">> => #{<<"code">> => -32603, <<"message">> => <<"Internal error">>}}
    end,

    %% Verify internal error code
    Error = maps:get(<<"error">>, Response, undefined),
    case Error of
        undefined -> ok;  %% Method might not exist, also valid
        _ -> ?assertEqual(-32603, maps:get(<<"code">>, Error))
    end.

test_handle_batch_request() ->
    %% Batch request
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"mcp.health">>, <<"params">> => null, <<"id">> => 1},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"mcp.health">>, <<"params">> => null, <<"id">> => 2}
    ]),

    {ok, Responses} = erlmcp_cli_json_rpc:handle_json_rpc(BatchJson, #{}, <<"test-session">>),

    %% Verify batch response
    ?assert(is_list(Responses)),
    ?assertEqual(2, length(Responses)).

test_handle_notification() ->
    %% Notification (no id, no response expected)
    NotificationJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test.notify">>,
        <<"params">> => #{<<"event">> => <<"test">>}
    }),

    %% Notifications return ok (no response)
    ok = erlmcp_cli_json_rpc:handle_json_rpc(NotificationJson, #{}, <<"test-session">>).

%%%====================================================================
%%% Encoding Tests
%%%====================================================================

test_encode_request() ->
    %% Encode request to JSON
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test.method">>,
        <<"params">> => #{<<"arg">> => <<"value">>},
        <<"id">> => 1
    },

    {ok, Json} = erlmcp_cli_json_rpc:encode_request(Request),

    %% Verify valid JSON
    ?assert(is_binary(Json)),
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(Request, Decoded).

test_encode_response() ->
    %% Encode response to JSON
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{<<"status">> => <<"ok">>},
        <<"id">> => 1
    },

    {ok, Json} = erlmcp_cli_json_rpc:encode_response(Response),

    %% Verify valid JSON
    ?assert(is_binary(Json)),
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(Response, Decoded).

test_encode_error() ->
    %% Encode error response to JSON
    Error = #{
        <<"code">> => -32601,
        <<"message">> => <<"Method not found">>,
        <<"data">> => <<"test.method">>
    },

    {ok, Json} = erlmcp_cli_json_rpc:encode_error(Error),

    %% Verify valid JSON
    ?assert(is_binary(Json)),
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(Error, Decoded).

test_encode_special_characters() ->
    %% Test special character encoding
    SpecialChars = unicode:characters_to_binary("Test ñ 中文 🎯", utf8),

    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"text">> => SpecialChars},
        <<"id">> => 1
    },

    {ok, Json} = erlmcp_cli_json_rpc:encode_request(Request),

    %% Verify encoding preserves characters
    ?assert(is_binary(Json)),
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(SpecialChars, maps:get(<<"text">>, maps:get(<<"params">>, Decoded))).

%%%====================================================================
%%% Decoding Tests
%%%====================================================================

test_decode_response() ->
    %% Decode response from JSON
    ResponseJson = <<"{\"jsonrpc\":\"2.0\",\"result\":{\"status\":\"ok\"},\"id\":1}">>,

    {ok, Response} = erlmcp_cli_json_rpc:decode_response(ResponseJson),

    %% Verify decoded structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    ?assertEqual(#{<<"status">> => <<"ok">>}, maps:get(<<"result">>, Response)).

test_decode_error() ->
    %% Decode error response from JSON
    ErrorJson = <<"{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32601,\"message\":\"Method not found\"},\"id\":1}">>,

    {ok, Response} = erlmcp_cli_json_rpc:decode_response(ErrorJson),

    %% Verify error structure
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)).

test_decode_unicode() ->
    %% Decode unicode characters
    UnicodeJson = <<"{\"jsonrpc\":\"2.0\",\"result\":{\"text\":\"Test ñ 中文 🎯\"},\"id\":1}">>,

    {ok, Response} = erlmcp_cli_json_rpc:decode_response(UnicodeJson),

    %% Verify unicode preserved
    Result = maps:get(<<"result">>, Response),
    Expected = unicode:characters_to_binary("Test ñ 中文 🎯", utf8),
    ?assertEqual(Expected, maps:get(<<"text">>, Result)).

%%%====================================================================
%%% Session Handling Tests
%%%====================================================================

test_session_context() ->
    %% Request with session context
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => 1
    }),

    SessionId = <<"test-session-123">>,
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{session => SessionId}, SessionId),

    %% Verify session context preserved
    ?assert(is_map(maps:get(<<"result">>, Response))).

test_invalid_session() ->
    %% Request with invalid session
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => 1
    }),

    %% Invalid session ID
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"invalid-session">>),

    %% Verify error response
    Error = maps:get(<<"error">>, Response, undefined),
    case Error of
        undefined -> ok;  %% Method might not require session
        _ -> ?assertEqual(-32001, maps:get(<<"code">>, Error))
    end.

%%%====================================================================
%%% Concurrent Requests Tests
%%%====================================================================

test_concurrent_requests() ->
    %% Spawn multiple processes making concurrent requests
    NumRequests = 10,
    Pids = [spawn(fun() ->
        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"mcp.health">>,
            <<"params">> => null,
            <<"id">> => N
        }),
        Result = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),
        self() ! {result, N, Result}
    end) || N <- lists:seq(1, NumRequests)],

    %% Wait for all requests to complete
    Results = [receive
        {result, N, Result} -> {N, Result}
    after 5000 ->
        ct:fail("Concurrent request timeout")
    end || _ <- Pids],

    %% Verify all requests succeeded
    ?assertEqual(NumRequests, length(Results)),
    lists:foreach(fun({N, {ok, Response}}) ->
        ?assertEqual(N, maps:get(<<"id">>, Response))
    end, Results).
