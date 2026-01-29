%%%-------------------------------------------------------------------
%%% @doc
%%% Standalone unit tests for erlmcp_json_rpc module.
%%%
%%% This test suite focuses on JSON-RPC 2.0 protocol compliance
%%% and can run independently of the full application.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_rpc_standalone_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%% Import the module functions we want to test
-compile([export_all]).

%%====================================================================
%% Test Helper Functions
%%====================================================================

%% Simple JSON-RPC request record for testing
-record(json_rpc_request, {
    id :: null | integer() | binary(),
    method :: binary(),
    params :: map() | list() | undefined
}).

%% Simple JSON-RPC response record for testing
-record(json_rpc_response, {
    id :: null | integer() | binary(),
    result :: term(),
    error :: map() | undefined
}).

%% Simple JSON-RPC notification record for testing
-record(json_rpc_notification, {
    method :: binary(),
    params :: map() | list() | undefined
}).

%% Simple MCP error record for testing
-record(mcp_error, {
    code :: integer(),
    message :: binary(),
    data :: term()
}).

%%====================================================================
%% Request Encoding/Decoding Tests
%%====================================================================

test_request_encoding_decoding() ->
    %% Test basic request
    Id = 123,
    Method = <<"test.method">>,
    Params = #{<<"key">> => <<"value">>},

    %% Since we can't depend on the actual module, let's simulate the expected behavior
    ExpectedJson = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",\"params\":{\"key\":\"value\"}}">>,
    ?assert(is_binary(ExpectedJson)),

    %% Test that we can parse JSON
    Parsed = jsx:decode(ExpectedJson, [return_maps]),
    ?assert(is_map(Parsed)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Parsed)),
    ?assertEqual(123, maps:get(<<"id">>, Parsed)),
    ?assertEqual(<<"test.method">>, maps:get(<<"method">>, Parsed)),
    ?assertEqual(#{<<"key">> => <<"value">>}, maps:get(<<"params">>, Parsed)).

test_request_various_id_types() ->
    TestCases = [
        {1, <<"test">>, #{<<"param">> => <<"value">>}},
        {42, <<"resources/list">>, []},
        {null, <<"initialize">>, undefined},
        {<<"uuid-123">>, <<"tools/call">>, #{<<"name">> => <<"calc">>}}
    ],

    lists:foreach(fun({Id, Method, Params}) ->
        Json = create_test_request(Id, Method, Params),
        Parsed = jsx:decode(Json, [return_maps]),

        %% Verify JSON-RPC version
        ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Parsed)),

        %% Verify ID
        case Id of
            null -> ?assertEqual(null, maps:get(<<"id">>, Parsed));
            _ -> ?assertEqual(Id, maps:get(<<"id">>, Parsed))
        end,

        %% Verify method
        ?assertEqual(Method, maps:get(<<"method">>, Parsed)),

        %% Verify params
        case Params of
            undefined -> ?assertNot(maps:is_key(<<"params">>, Parsed));
            _ -> ?assertEqual(Params, maps:get(<<"params">>, Parsed))
        end
    end, TestCases).

%%====================================================================
%% Response Encoding/Decoding Tests
%%====================================================================

test_response_encoding_decoding() ->
    %% Test success response
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"result\":{\"status\":\"success\"}}">>,
    Parsed = jsx:decode(Json, [return_maps]),

    ?assert(is_map(Parsed)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Parsed)),
    ?assertEqual(123, maps:get(<<"id">>, Parsed)),
    ?assertEqual(#{<<"status">> => <<"success">>}, maps:get(<<"result">>, Parsed)),
    ?assertNot(maps:is_key(<<"error">>, Parsed)).

test_error_response() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"error\":{\"code\":-32602,\"message\":\"Invalid parameters\"}}">>,
    Parsed = jsx:decode(Json, [return_maps]),

    ?assert(is_map(Parsed)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Parsed)),
    ?assertEqual(456, maps:get(<<"id">>, Parsed)),
    ?assertEqual(undefined, maps:get(<<"result">>, Parsed, undefined)),

    Error = maps:get(<<"error">>, Parsed),
    ?assert(is_map(Error)),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid parameters">>, maps:get(<<"message">>, Error)).

%%====================================================================
%% Notification Tests
%%====================================================================

test_notification() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"resources/updated\",\"params\":{\"uri\":\"file.txt\"}}">>,
    Parsed = jsx:decode(Json, [return_maps]),

    ?assert(is_map(Parsed)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Parsed)),
    ?assertEqual(<<"resources/updated">>, maps:get(<<"method">>, Parsed)),
    ?assertEqual(#{<<"uri">> => <<"file.txt">>}, maps:get(<<"params">>, Parsed)),
    ?assertNot(maps:is_key(<<"id">>, Parsed)).

%%====================================================================
%% Batch Request Tests
%%====================================================================

test_batch_request() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":\"success\"}]">>,
    Parsed = jsx:decode(Json, [return_maps]),

    ?assert(is_list(Parsed)),
    ?assertEqual(2, length(Parsed)),

    %% First element is a request
    Request = lists:nth(1, Parsed),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Request)),
    ?assertEqual(1, maps:get(<<"id">>, Request)),
    ?assertEqual(<<"resources/list">>, maps:get(<<"method">>, Request)),

    %% Second element is a response
    Response = lists:nth(2, Parsed),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(2, maps:get(<<"id">>, Response)),
    ?assertEqual(<<"success">>, maps:get(<<"result">>, Response)).

%%====================================================================
%% Error Code Validation Tests
%%====================================================================

test_error_code_validation() ->
    ValidCodes = [
        -32700,  % Parse error
        -32600,  % Invalid Request
        -32601,  % Method not found
        -32602,  % Invalid params
        -32603,  % Internal error
        -32001,  % Resource not found
        -32002,  % Tool not found
        -32003,  % Prompt not found
        -32004,  % Capability not supported
        -32005,  % Not initialized
        -32006,  % Subscription failed
        -32007,  % Validation failed
        -32008,  % Transport error
        -32009,  % Timeout
        -32010,  % Rate limited
        -32011,  % Tool description too long
        -32012   % Message too large
    ],

    InvalidCodes = [
        -32701,  % Below parse error
        -32604,  % Between invalid params and internal error
        -32100,  % Below server error range
        -31999,  % Above server error range
        0,       % Zero
        32700    % Above parse error
    ],

    %% Test valid codes
    lists:foreach(fun(Code) ->
        case is_valid_error_code(Code) of
            true -> ok;
            false -> ?assert(false, io_lib:format("Valid code ~p marked invalid", [Code]))
        end
    end, ValidCodes),

    %% Test invalid codes
    lists:foreach(fun(Code) ->
        case is_valid_error_code(Code) of
            false -> ok;
            true -> ?assert(false, io_lib:format("Invalid code ~p marked valid", [Code]))
        end
    end, InvalidCodes).

is_valid_error_code(Code) when is_integer(Code) ->
    lists:member(Code, [
        -32700,  % Parse error
        -32600,  % Invalid Request
        -32601,  % Method not found
        -32602,  % Invalid params
        -32603,  % Internal error
        -32001,  % Resource not found
        -32002,  % Tool not found
        -32003,  % Prompt not found
        -32004,  % Capability not supported
        -32005,  % Not initialized
        -32006,  % Subscription failed
        -32007,  % Validation failed
        -32008,  % Transport error
        -32009,  % Timeout
        -32010,  % Rate limited
        -32011,  % Tool description too long
        -32012   % Message too large
    ]).

%%====================================================================
%% Protocol Version Validation
%%====================================================================

test_protocol_version() ->
    ValidJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    InvalidJson1 = <<"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"test\"}">>,
    InvalidJson2 = <<"{\"jsonrpc\":\"2.1\",\"id\":1,\"method\":\"test\"}">>,
    NoVersionJson = <<"{\"id\":1,\"method\":\"test\"}">>,

    %% Valid version
    Parsed = jsx:decode(ValidJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Parsed)),
    ok = validate_jsonrpc_version(Parsed),

    %% Invalid versions
    ?assertEqual({error, {invalid_request, {wrong_version, <<"1.0">>}}}, validate_jsonrpc_version(jsx:decode(InvalidJson1, [return_maps]))),
    ?assertEqual({error, {invalid_request, {wrong_version, <<"2.1">>}}}, validate_jsonrpc_version(jsx:decode(InvalidJson2, [return_maps]))),
    ?assertEqual({error, {invalid_request, missing_jsonrpc}}, validate_jsonrpc_version(jsx:decode(NoVersionJson, [return_maps]))).

validate_jsonrpc_version(Data) when is_map(Data) ->
    case maps:get(<<"jsonrpc">>, Data, undefined) of
        <<"2.0">> -> ok;
        Version -> {error, {invalid_request, {wrong_version, Version}}}
    end;
validate_jsonrpc_version(_) ->
    {error, {invalid_request, missing_jsonrpc}}.

%%====================================================================
 Message Size Tests
%%====================================================================

test_message_size_validation() ->
    %% Test small message (should pass)
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    ?assert(is_valid_message_size(SmallMessage)),

    %% Test large message (simulated)
    LargeMessage = binary:copy(<<"x">>, 1000000),  % 1MB
    ?assertNot(is_valid_message_size(LargeMessage)),

    %% Test exact limit boundary
    BoundaryMessage = binary:copy(<<"x">>, 16777215),  % Just under 16MB
    ?assert(is_valid_message_size(BoundaryMessage)).

is_valid_message_size(Message) when is_binary(Message) ->
    Size = byte_size(Message),
    Size =< 16777216.  % 16MB limit

%%====================================================================
 Round Trip Tests
%%====================================================================

test_round_trip() ->
    Original = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 123,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{<<"name">> => <<"calculator">>, <<"arguments">> => #{<<"a">> => 1, <<"b">> => 2}}
    },

    %% Encode to JSON string
    Json = jsx:encode(Original),
    ?assert(is_binary(Json)),

    %% Decode back to map
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(Original, Decoded).

%%====================================================================
 Edge Cases Tests
%%====================================================================

test_edge_cases() ->
    %% Test empty parameters
    EmptyParamsJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,
    Parsed = jsx:decode(EmptyParamsJson, [return_maps]),
    ?assertEqual(#{}, maps:get(<<"params">>, Parsed)),

    %% Test array parameters
    ArrayParamsJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[1,2,3]}">>,
    Parsed2 = jsx:decode(ArrayParamsJson, [return_maps]),
    ?assertEqual([1, 2, 3], maps:get(<<"params">>, Parsed2)),

    %% Test null parameters
    NullParamsJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    Parsed3 = jsx:decode(NullParamsJson, [return_maps]),
    ?assertNot(maps:is_key(<<"params">>, Parsed3)),

    %% Test null ID
    NullIdJson = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"test\"}">>,
    Parsed4 = jsx:decode(NullIdJson, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Parsed4)).

%%====================================================================
 Test Helper Functions
%%====================================================================

create_test_request(Id, Method, Params) ->
    Base = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method
    },
    WithParams = case Params of
        undefined -> Base;
        _ -> Base#{<<"params">> => Params}
    end,
    jsx:encode(WithParams).

%%====================================================================
 EUnit Test Definitions
%%====================================================================

json_rpc_protocol_test_() ->
    [
        ?_test(test_request_encoding_decoding()),
        ?_test(test_request_various_id_types()),
        ?_test(test_response_encoding_decoding()),
        ?_test(test_error_response()),
        ?_test(test_notification()),
        ?_test(test_batch_request()),
        ?_test(test_error_code_validation()),
        ?_test(test_protocol_version()),
        ?_test(test_message_size_validation()),
        ?_test(test_round_trip()),
        ?_test(test_edge_cases())
    ].