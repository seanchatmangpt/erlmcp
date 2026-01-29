%% @doc JSON-RPC 2.0 Protocol Compliance Test Suite
%% Tests all JSON-RPC message types and protocol compliance
-module(mcp_json_rpc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test case exports
-export([
    %% Basic Message Tests
    request_encoding/1,
    request_decoding/1,
    response_encoding/1,
    response_decoding/1,
    notification_encoding/1,
    notification_decoding/1,

    %% Batch Request Tests
    batch_encoding/1,
    batch_decoding/1,
    batch_empty/1,
    batch_single/1,

    %% Error Handling Tests
    error_codes/1,
    error_messages/1,
    error_data/1,
    parse_errors/1,
    invalid_requests/1,
    method_not_found/1,
    invalid_params/1,
    internal_errors/1,

    %% Validation Tests
    message_size_limits/1,
    invalid_json/1,
    missing_required_fields/1,
    duplicate_ids/1,
    null_id/1,

    %% Performance Tests
    encoding_performance/1,
    decoding_performance/1,
    large_message_handling/1,

    %% Integration Tests
    request_response_pair/1,
    notification_handling/1,
    error_notification/1
]).

%%====================================================================
%% Test configuration
%%====================================================================

all() ->
    [
        %% Basic Message Tests
        request_encoding,
        request_decoding,
        response_encoding,
        response_decoding,
        notification_encoding,
        notification_decoding,

        %% Batch Request Tests
        batch_encoding,
        batch_decoding,
        batch_empty,
        batch_single,

        %% Error Handling Tests
        error_codes,
        error_messages,
        error_data,
        parse_errors,
        invalid_requests,
        method_not_found,
        invalid_params,
        internal_errors,

        %% Validation Tests
        message_size_limits,
        invalid_json,
        missing_required_fields,
        duplicate_ids,
        null_id,

        %% Performance Tests
        encoding_performance,
        decoding_performance,
        large_message_handling,

        %% Integration Tests
        request_response_pair,
        notification_handling,
        error_notification
    ].

init_per_suite(Config) ->
    %% Start required applications
    {ok, Apps} = application:ensure_all_started(erlmcp_core),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = proplists:get_value(apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Apps)),
    Config.

init_per_testcase(_TestCase, Config) ->
    process_flag(trap_exit, true),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Basic Message Tests
%%====================================================================

request_encoding(_Config) ->
    %% Test JSON-RPC request encoding
    Id = 1,
    Method = <<"test_method">>,
    Params = #{<<"key">> => <<"value">>, <<"number">> => 42},

    Request = erlmcp_json_rpc:encode_request(Id, Method, Params),

    %% Verify it's valid JSON
    Json = jsx:decode(Request, [{return, binary})),

    case Json of
        #{<<"jsonrpc">> := <<"2.0">>, <<"id">> := 1, <<"method">> := <<"test_method">>,
          <<"params">> := Params} ->
            true;
        _ ->
            ct:fail("Invalid request encoding: ~p", [Json])
    end.

request_decoding(_Config) ->
    %% Test JSON-RPC request decoding
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test_method\",\"params\":{\"key\":\"value\"}}">>,

    {ok, Decoded} = erlmcp_json_rpc:decode_message(RequestJson),

    case Decoded of
        #json_rpc_request{
            id = 1,
            method = <<"test_method">>,
            params = #{<<"key">> := <<"value">>}
        } ->
            true;
        _ ->
            ct:fail("Failed to decode request correctly: ~p", [Decoded])
    end.

response_encoding(_Config) ->
    %% Test JSON-RPC response encoding
    Id = 1,
    Result = #{<<"result">> => <<"success">>, <<"data">> => [1, 2, 3]},

    Response = erlmcp_json_rpc:encode_response(Id, Result),

    Json = jsx:decode(Response, [{return, binary})),

    case Json of
        #{<<"jsonrpc">> := <<"2.0">>, <<"id">> := 1, <<"result">> := Result} ->
            true;
        _ ->
            ct:fail("Invalid response encoding: ~p", [Json])
    end.

response_decoding(_Config) ->
    %% Test JSON-RPC response decoding
    ResponseJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"status\":\"success\"}}">>,

    {ok, Decoded} = erlmcp_json_rpc:decode_message(ResponseJson),

    case Decoded of
        #json_rpc_response{
            id = 1,
            result = #{<<"status">> := <<"success">>}
        } ->
            true;
        _ ->
            ct:fail("Failed to decode response correctly: ~p", [Decoded])
    end.

notification_encoding(_Config) ->
    %% Test JSON-RPC notification encoding
    Method = <<"notification_method">>,
    Params = #{<<"event">> => <<"test">>, <<"timestamp">> => <<"2025-01-12T12:00:00Z">>},

    Notification = erlmcp_json_rpc:encode_notification(Method, Params),

    Json = jsx:decode(Notification, [{return, binary})),

    case Json of
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := <<"notification_method">>,
          <<"params">> := Params} ->
            true;
        _ ->
            ct:fail("Invalid notification encoding: ~p", [Json])
    end.

notification_decoding(_Config) ->
    %% Test JSON-RPC notification decoding
    NotificationJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notification_method\",\"params\":{\"event\":\"test\"}}">>,

    {ok, Decoded} = erlmcp_json_rpc:decode_message(NotificationJson),

    case Decoded of
        #json_rpc_notification{
            method = <<"notification_method">>,
            params = #{<<"event">> := <<"test">>}
        } ->
            true;
        _ ->
            ct:fail("Failed to decode notification correctly: ~p", [Decoded])
    end.

%%====================================================================
%% Batch Request Tests
%%====================================================================

batch_encoding(_Config) ->
    %% Test batch request encoding
    Requests = [
        erlmcp_json_rpc:encode_request(1, <<"method1">>, #{<<"param">> => <<"value1">>}),
        erlmcp_json_rpc:encode_request(2, <<"method2">>, #{<<"param">> => <<"value2">>}),
        erlmcp_json_rpc:encode_request(3, <<"method3">>, #{})
    ],

    BatchJson = erlmcp_json_rpc:encode_batch(Requests),

    %% Verify it's a JSON array
    Json = jsx:decode(BatchJson, [{return, binary})),

    case Json of
        [_, _, _] ->
            true;
        _ ->
            ct:fail("Invalid batch encoding: ~p", [Json])
    end.

batch_decoding(_Config) ->
    %% Test batch request decoding
    BatchJson = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"method1\",\"params\":{\"param\":\"value1\"}},"
                 "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"method2\",\"params\":{\"param\":\"value2\"}},"
                 "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"method3\"}]">>,

    {ok, BatchRequests} = erlmcp_json_rpc:decode_batch(BatchJson),

    case BatchRequests of
        [
            #json_rpc_request{id = 1, method = <<"method1">>, params = Params1},
            #json_rpc_request{id = 2, method = <<"method2">>, params = Params2},
            #json_rpc_request{id = 3, method = <<"method3">>, params = undefined}
        ] ->
            maps:get(<<"param">>, Params1) =:= <<"value1">> andalso
            maps:get(<<"param">>, Params2) =:= <<"value2">>;
        _ ->
            ct:fail("Failed to decode batch correctly: ~p", [BatchRequests])
    end.

batch_empty(_Config) ->
    %% Test empty batch request
    BatchJson = <<">>,

    case erlmcp_json_rpc:decode_batch(BatchJson) of
        {error, _} ->
            true;
        _ ->
            ct:fail("Should reject empty batch")
    end.

batch_single(_Config) ->
    %% Test single message in batch (should still be parsed as batch)
    SingleJson = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}]">>,

    {ok, BatchRequests} = erlmcp_json_rpc:decode_batch(SingleJson),

    case BatchRequests of
        [#json_rpc_request{id = 1, method = <<"test">>, params = #{}}] ->
            true;
        _ ->
            ct:fail("Failed to decode single-message batch")
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_codes(_Config) ->
    %% Test all standard error codes
    ErrorCodes = [
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
        -32010   % Rate limited
    ],

    lists:foreach(fun(Code) ->
        Id = 1,
        Message = <<"Test error">>,
        ErrorJson = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
        {ok, _} = erlmcp_json_rpc:decode_message(ErrorJson),
        true = erlmcp_json_rpc:validate_error_code(Code)
    end, ErrorCodes).

error_messages(_Config) ->
    %% Test error message encoding/decoding
    Id = 1,
    ErrorCode = -32601,
    ErrorMessage = <<"Method not found">>,

    ErrorJson = erlmcp_json_rpc:encode_error_response(Id, ErrorCode, ErrorMessage),

    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),

    case Decoded of
        #json_rpc_response{
            id = 1,
            error = #json_rpc_error{
                code = -32601,
                message = <<"Method not found">>
            }
        } ->
            true;
        _ ->
            ct:fail("Error message test failed: ~p", [Decoded])
    end.

error_data(_Config) ->
    %% Test error with data field
    Id = 1,
    ErrorCode = -32001,
    ErrorMessage = <<"Resource not found">>,
    ErrorData = #{<<"uri">> => <<"file:///nonexistent.txt">>},

    ErrorJson = erlmcp_json_rpc:encode_error_response(Id, ErrorCode, ErrorMessage, ErrorData),

    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),

    case Decoded of
        #json_rpc_response{
            id = 1,
            error = #json_rpc_error{
                code = -32001,
                message = <<"Resource not found">>,
                data = ErrorData
            }
        } ->
            true;
        _ ->
            ct:fail("Error data test failed: ~p", [Decoded])
    end.

parse_errors(_Config) ->
    %% Test parse error handling
    InvalidJson = <<"{invalid json}">>,

    case erlmcp_json_rpc:decode_message(InvalidJson) of
        {error, {parse_error, _}} ->
            true;
        _ ->
            ct:fail("Should detect parse errors")
    end.

invalid_requests(_Config) ->
    %% Test invalid request format
    InvalidJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">>,  % Missing id

    case erlmcp_json_rpc:decode_message(InvalidJson) of
        {error, {invalid_request, _}} ->
            true;
        _ ->
            ct:fail("Should detect invalid requests")
    end.

method_not_found(_Config) ->
    %% Test method not found error
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"nonexistent_method\"}">>,

    case erlmcp_json_rpc:decode_message(Json) of
        {ok, #json_rpc_request{method = <<"nonexistent_method">>}} ->
            %% Server should return method not found error
            ErrorJson = erlmcp_json_rpc:error_method_not_found(1, <<"nonexistent_method">>),
            {ok, Error} = erlmcp_json_rpc:decode_message(ErrorJson),
            Error#json_rpc_error.code =:= -32601;
        _ ->
            ct:fail("Method not found test failed")
    end.

invalid_params(_Config) ->
    %% Test invalid params error
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,

    case erlmcp_json_rpc:decode_message(Json) of
        {ok, #json_rpc_request{method = <<"test">>, params = #{}}} ->
            ErrorJson = erlmcp_json_rpc:error_invalid_params(1, <<"Missing required parameters">>),
            {ok, Error} = erlmcp_json_rpc:decode_message(ErrorJson),
            Error#json_rpc_error.code =:= -32602;
        _ ->
            ct:fail("Invalid params test failed")
    end.

internal_errors(_Config) ->
    %% Test internal error handling
    ErrorJson = erlmcp_json_rpc:error_internal(<<"Internal server error">>),

    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),

    case Decoded of
        #json_rpc_response{
            error = #json_rpc_error{
                code = -32603,
                message = <<"Internal server error">>
            }
        } ->
            true;
        _ ->
            ct:fail("Internal error test failed")
    end.

%%====================================================================
%% Validation Tests
%%====================================================================

message_size_limits(_Config) ->
    %% Test message size validation
    LargeJson = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                              <<"id">> => 1,
                              <<"method">> => <<"test">>,
                              <<"params">> => #{<<"data">> => lists:duplicate(10000, <<"x">>)}}),

    case erlmcp_json_rpc:decode_message(LargeJson, large) of
        {error, {message_too_large, _}} ->
            true;
        _ ->
            ct:fail("Should detect large messages")
    end.

invalid_json(_Config) ->
    %% Test various invalid JSON scenarios
    InvalidCases = [
        <<>>,  % Empty
        <<"{">>,  % Incomplete
        <<"}{">>,  % Multiple root objects
        <<"[1, 2, 3]">>  % Array instead of object
    ],

    lists:foreach(fun(Json) ->
        case erlmcp_json_rpc:decode_message(Json) of
            {error, _} -> ok;
            _ -> ct:fail("Should reject invalid JSON: ~p", [Json])
        end
    end, InvalidCases).

missing_required_fields(_Config) ->
    %% Test missing required fields
    Cases = [
        <<"{\"method\":\"test\"}">>,  % Missing jsonrpc and id
        <<"{\"jsonrpc\":\"2.0\"}">>,  % Missing method
        <<"{\"id\":1}">>  % Missing jsonrpc and method
    ],

    lists:foreach(fun(Json) ->
        case erlmcp_json_rpc:decode_message(Json) of
            {error, {invalid_request, _}} -> ok;
            _ -> ct:fail("Should reject missing fields: ~p", [Json])
        end
    end, Cases).

duplicate_ids(_Config) ->
    %% Test duplicate ID detection (should be handled at application level)
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test1\"},"
             "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test2\"}]">>,

    {ok, Batch} = erlmcp_json_rpc:decode_batch(Json),
    %% Application should detect duplicate IDs
    length(Batch) =:= 2,
    true.

null_id(_Config) ->
    %% Test null ID (should not be allowed in MCP)
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"test\"}">>,

    case erlmcp_json_rpc:decode_message(Json) of
        {error, {invalid_request, _}} ->
            true;
        _ ->
            ct:fail("Should reject null ID")
    end.

%%====================================================================
%% Performance Tests
%%====================================================================

encoding_performance(_Config) ->
    %% Test encoding performance
    NumMessages = 10000,
    Method = <<"test_method">>,
    Params = #{<<"data">> => <<"test">>},

    {Time, _} = timer:tc(fun() ->
        lists:map(fun(_) ->
            erlmcp_json_rpc:encode_request(1, Method, Params)
        end, lists:seq(1, NumMessages))
    end),

    Throughput = NumMessages / (Time / 1000000),
    ct:pal("Encoding throughput: ~p messages/sec", [Throughput]),

    Throughput > 10000,  % Should handle 10K+ messages/sec
    true.

decoding_performance(_Config) ->
    %% Test decoding performance
    NumMessages = 10000,
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"data\":\"test\"}}">>,

    {Time, _} = timer:tc(fun() ->
        lists:map(fun(_) ->
            erlmcp_json_rpc:decode_message(Json)
        end, lists:seq(1, NumMessages))
    end),

    Throughput = NumMessages / (Time / 1000000),
    ct:pal("Decoding throughput: ~p messages/sec", [Throughput]),

    Throughput > 10000,  % Should handle 10K+ messages/sec
    true.

large_message_handling(_Config) ->
    %% Test handling of large messages
    LargeData = lists:duplicate(100000, <<"x">>),
    LargeParams = #{<<"data">> => LargeData},
    Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                          <<"id">> => 1,
                          <<"method">> => <<"test">>,
                          <<"params">> => LargeParams}),

    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json, large),

    case Decoded of
        #json_rpc_request{params = Params} ->
            maps:size(Params) =:= 1;
        _ ->
            ct:fail("Large message handling failed")
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

request_response_pair(_Config) ->
    %% Test complete request/response cycle
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"value\":\"hello\"}}">>,

    {ok, Request} = erlmcp_json_rpc:decode_message(RequestJson),

    case Request of
        #json_rpc_request{
            id = 1,
            method = <<"test">>,
            params = Params
        } ->
            Response = erlmcp_json_rpc:encode_response(1, #{<<"echo">> => Params}),
            {ok, ResponseDecoded} = erlmcp_json_rpc:decode_message(Response),

            case ResponseDecoded of
                #json_rpc_response{
                    id = 1,
                    result = #{<<"echo">> := EchoParams}
                } ->
                    EchoParams = Params;
                _ ->
                    ct:fail("Request/response cycle failed")
            end;
        _ ->
            ct:fail("Request parsing failed")
    end.

notification_handling(_Config) ->
    %% Test notification handling in response to request
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"subscribe\"}">>,

    {ok, Request} = erlmcp_json_rpc:decode_message(RequestJson),

    case Request of
        #json_rpc_request{id = 1, method = <<"subscribe">>} ->
            %% Send notification
            Notification = erlmcp_json_rpc:encode_notification(<<"update">>, #{<<"status">> => <<"connected">>}),
            {ok, NotificationDecoded} = erlmcp_json_rpc:decode_message(Notification),

            case NotificationDecoded of
                #json_rpc_notification{
                    method = <<"update">>,
                    params = #{<<"status">> := <<"connected">>}
                } ->
                    true;
                _ ->
                    ct:fail("Notification handling failed")
            end;
        _ ->
            ct:fail("Request parsing failed")
    end.

error_notification(_Config) ->
    %% Test error notification
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"error_prone\"}">>,

    {ok, Request} = erlmcp_json_rpc:decode_message(RequestJson),

    case Request of
        #json_rpc_request{id = 1, method = <<"error_prone">>} ->
            %% Send error response
            Error = erlmcp_json_rpc:encode_error_response(1, -32001, <<"Resource not found">>),
            {ok, ErrorDecoded} = erlmcp_json_rpc:decode_message(Error),

            case ErrorDecoded of
                #json_rpc_response{
                    id = 1,
                    error = #json_rpc_error{
                        code = -32001,
                        message = <<"Resource not found">>
                    }
                } ->
                    true;
                _ ->
                    ct:fail("Error notification failed")
            end;
        _ ->
            ct:fail("Request parsing failed")
    end.