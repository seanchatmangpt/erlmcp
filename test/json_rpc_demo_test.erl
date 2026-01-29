%%%-------------------------------------------------------------------
%%% @doc
%%% Demonstration of JSON-RPC 2.0 protocol testing
%%%
%%% This module demonstrates how to test JSON-RPC 2.0 protocol compliance
%%% without depending on the complex erlmcp infrastructure.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(json_rpc_demo_test).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% JSON-RPC Protocol Constants
%%====================================================================

-define(JSONRPC_VERSION, <<"2.0">>).
-define(JSONRPC_FIELD_JSONRPC, <<"jsonrpc">>).
-define(JSONRPC_FIELD_ID, <<"id">>).
-define(JSONRPC_FIELD_METHOD, <<"method">>).
-define(JSONRPC_FIELD_PARAMS, <<"params">>).
-define(JSONRPC_FIELD_RESULT, <<"result">>).
-define(JSONRPC_FIELD_ERROR, <<"error">>).
-define(JSONRPC_ERROR_FIELD_CODE, <<"code">>).
-define(JSONRPC_ERROR_FIELD_MESSAGE, <<"message">>).
-define(JSONRPC_ERROR_FIELD_DATA, <<"data">>).

%% JSON-RPC Error Codes
-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).

%%====================================================================
%% Test Records
%%====================================================================

-record(json_rpc_request, {
    id :: null | integer() | binary(),
    method :: binary(),
    params :: map() | list() | undefined
}).

-record(json_rpc_response, {
    id :: null | integer() | binary(),
    result :: term(),
    error :: map() | undefined
}).

-record(json_rpc_notification, {
    method :: binary(),
    params :: map() | list() | undefined
}).

%%====================================================================
%% Test Cases
%%====================================================================

json_rpc_protocol_test_() ->
    [
        ?_test(test_request_encoding()),
        ?_test(test_request_decoding()),
        ?_test(test_response_encoding()),
        ?_test(test_response_decoding()),
        ?_test(test_notification()),
        ?_test(test_batch_requests()),
        ?_test(test_error_responses()),
        ?_test(test_protocol_version_validation()),
        ?_test(test_edge_cases())
    ].

test_request_encoding() ->
    %% Test various request formats
    TestCases = [
        {1, <<"resources/list">>, undefined},
        {42, <<"tools/call">>, #{<<"name">> => <<"calculator">>}},
        {<<"req-123">>, <<"prompts/get">>, [<<"prompt1">>, <<"prompt2">>]},
        {null, <<"initialize">>, #{}}
    ],

    lists:foreach(fun({Id, Method, Params}) ->
        Json = encode_request(Id, Method, Params),
        ?assert(is_binary(Json)),
        Decoded = jsx:decode(Json, [return_maps]),
        verify_request(Decoded, Id, Method, Params)
    end, TestCases).

test_request_decoding() ->
    %% Test valid requests
    ValidRequests = [
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"calculator\"}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":\"req-123\",\"method\":\"prompts/get\",\"params\":[\"prompt1\",\"prompt2\"]}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"initialize\",\"params\":{}}">>
    ],

    lists:foreach(fun(Json) ->
        ?assertMatch({ok, _}, decode_request(Json))
    end, ValidRequests),

    %% Test invalid requests
    InvalidRequests = [
        <<"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"test\"}">>,  % Wrong version
        <<"{\"id\":1,\"method\":\"test\"}">>,  % Missing jsonrpc
        <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">>,  % Missing id - this is a notification, not an error
        <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,  % Missing method
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":123}">>,  % Invalid method
        <<"invalid json">>  % Invalid JSON
    ],

    lists:foreach(fun(Json) ->
        Result = decode_request(Json),
        case Json of
            <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">> ->
                % This is a valid notification, not an error
                ?assertMatch({ok, _}, Result);
            _ ->
                ?assertMatch({error, _}, Result)
        end
    end, InvalidRequests).

test_response_encoding() ->
    %% Test success response
    SuccessJson = encode_response(123, #{<<"status">> => <<"success">>, <<"data">> => [1, 2, 3]}),
    ?assert(is_binary(SuccessJson)),

    %% Test error response
    ErrorJson = encode_error_response(123, ?JSONRPC_METHOD_NOT_FOUND, <<"Method not found">>),
    ?assert(is_binary(ErrorJson)),

    %% Verify encoded responses
    SuccessDecoded = jsx:decode(SuccessJson, [return_maps]),
    ?assertEqual(123, maps:get(?JSONRPC_FIELD_ID, SuccessDecoded)),
    ?assertEqual(#{<<"status">> => <<"success">>, <<"data">> => [1, 2, 3]}, maps:get(?JSONRPC_FIELD_RESULT, SuccessDecoded)),
    ?assertNot(maps:is_key(?JSONRPC_FIELD_ERROR, SuccessDecoded)),

    ErrorDecoded = jsx:decode(ErrorJson, [return_maps]),
    ?assertEqual(123, maps:get(?JSONRPC_FIELD_ID, ErrorDecoded)),
    ?assertEqual(undefined, maps:get(?JSONRPC_FIELD_RESULT, ErrorDecoded, undefined)),
    Error = maps:get(?JSONRPC_FIELD_ERROR, ErrorDecoded),
    ?assertEqual(?JSONRPC_METHOD_NOT_FOUND, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error)).

test_response_decoding() ->
    %% Test valid responses
    ValidResponses = [
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"success\"}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"status\":\"ok\"}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}">>
    ],

    lists:foreach(fun(Json) ->
        ?assertMatch({ok, _}, decode_response(Json))
    end, ValidResponses).

test_notification() ->
    %% Test notification encoding/decoding
    NotificationJson = encode_notification(<<"resources/updated">>, #{<<"uri">> => <<"file.txt">>}),
    ?assert(is_binary(NotificationJson)),

    NotificationDecoded = jsx:decode(NotificationJson, [return_maps]),
    ?assertEqual(<<"resources/updated">>, maps:get(?JSONRPC_FIELD_METHOD, NotificationDecoded)),
    ?assertEqual(#{<<"uri">> => <<"file.txt">>}, maps:get(?JSONRPC_FIELD_PARAMS, NotificationDecoded)),
    ?assertNot(maps:is_key(?JSONRPC_FIELD_ID, NotificationDecoded)).

test_batch_requests() ->
    %% Test batch request encoding
    BatchJson = encode_batch([
        encode_request(1, <<"resources/list">>, undefined),
        encode_response(2, #{<<"status">> => success}),
        encode_notification(<<"progress">>, #{<<"current">> => 50})
    ]),
    ?assert(is_binary(BatchJson)),

    %% Verify batch structure
    BatchDecoded = jsx:decode(BatchJson, [return_maps]),
    ?assert(is_list(BatchDecoded)),
    ?assertEqual(3, length(BatchDecoded)),

    %% First element is request
    First = lists:nth(1, BatchDecoded),
    ?assertEqual(1, maps:get(?JSONRPC_FIELD_ID, First)),
    ?assertEqual(<<"resources/list">>, maps:get(?JSONRPC_FIELD_METHOD, First)),

    %% Second element is response
    Second = lists:nth(2, BatchDecoded),
    ?assertEqual(2, maps:get(?JSONRPC_FIELD_ID, Second)),
    ?assertEqual(#{<<"status">> => success}, maps:get(?JSONRPC_FIELD_RESULT, Second)).

test_error_responses() ->
    %% Test various error types
    ErrorTypes = [
        {?JSONRPC_PARSE_ERROR, <<"Parse error">>},
        {?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>},
        {?JSONRPC_METHOD_NOT_FOUND, <<"Method not found">>},
        {?JSONRPC_INVALID_PARAMS, <<"Invalid params">>},
        {?JSONRPC_INTERNAL_ERROR, <<"Internal error">>}
    ],

    lists:foreach(fun({Code, Message}) ->
        Json = encode_error_response(123, Code, Message),
        Decoded = jsx:decode(Json, [return_maps]),
        Error = maps:get(?JSONRPC_FIELD_ERROR, Decoded),
        ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error)),
        ?assertEqual(Message, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error))
    end, ErrorTypes).

test_protocol_version_validation() ->
    %% Test valid version
    ValidJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    ?assertEqual(ok, validate_version(jsx:decode(ValidJson, [return_maps]))),

    %% Test invalid versions
    InvalidVersions = [
        {<<"1.0">>, {wrong_version, <<"1.0">>}},
        {<<"2.1">>, {wrong_version, <<"2.1">>}},
        {undefined, missing_jsonrpc}
    ],

    lists:foreach(fun({Version, Expected}) ->
        Json = case Version of
            undefined -> <<"{\"id\":1,\"method\":\"test\"}">>;
            _ -> io_lib:format("{\"jsonrpc\":\"~s\",\"id\":1,\"method\":\"test\"}", [Version])
        end,
        Decoded = jsx:decode(iolist_to_binary(Json), [return_maps]),
        ?assertEqual({error, Expected}, validate_version(Decoded))
    end, InvalidVersions).

test_edge_cases() ->
    %% Test empty parameters
    EmptyParams = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,
    Decoded = jsx:decode(EmptyParams, [return_maps]),
    ?assertEqual(#{}, maps:get(?JSONRPC_FIELD_PARAMS, Decoded)),

    %% Test null parameters (should not appear in JSON)
    NoParams = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    Decoded2 = jsx:decode(NoParams, [return_maps]),
    ?assertNot(maps:is_key(?JSONRPC_FIELD_PARAMS, Decoded2)),

    %% Test array parameters
    ArrayParams = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[1,2,3]}">>,
    Decoded3 = jsx:decode(ArrayParams, [return_maps]),
    ?assertEqual([1, 2, 3], maps:get(?JSONRPC_FIELD_PARAMS, Decoded3)).

%%====================================================================
%% Helper Functions
%%====================================================================

encode_request(Id, Method, Params) when is_binary(Method) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => Id,
        ?JSONRPC_FIELD_METHOD => Method
    },
    WithParams = case Params of
        undefined -> Base;
        _ -> Base#{?JSONRPC_FIELD_PARAMS => Params}
    end,
    jsx:encode(WithParams).

encode_response(Id, Result) ->
    Response = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => Id,
        ?JSONRPC_FIELD_RESULT => Result
    },
    jsx:encode(Response).

encode_error_response(Id, Code, Message) when is_integer(Code), is_binary(Message) ->
    Error = #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message
    },
    Response = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => Id,
        ?JSONRPC_FIELD_ERROR => Error
    },
    jsx:encode(Response).

encode_notification(Method, Params) when is_binary(Method) ->
    Notification = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_METHOD => Method
    },
    WithParams = case Params of
        undefined -> Notification;
        _ -> Notification#{?JSONRPC_FIELD_PARAMS => Params}
    end,
    jsx:encode(WithParams).

encode_batch(Messages) ->
    jsx:encode(Messages).

decode_request(Json) when is_binary(Json) ->
    try
        case jsx:decode(Json, [return_maps]) of
            Map when is_map(Map) ->
                case validate_version(Map) of
                    ok -> parse_json_rpc(Map);
                    Error -> Error
                end;
            _ -> {error, {invalid_request, not_object}}
        end
    catch
        error:badarg -> {error, {parse_error, invalid_json}};
        Class:Reason -> {error, {parse_error, {Class, Reason}}}
    end.

decode_response(Json) when is_binary(Json) ->
    decode_request(Json).

parse_json_rpc(Data) ->
    case maps:get(?JSONRPC_FIELD_JSONRPC, Data, undefined) of
        ?JSONRPC_VERSION -> parse_by_type(Data);
        _ -> {error, {invalid_request, {wrong_version, maps:get(?JSONRPC_FIELD_JSONRPC, Data)}}}
    end.

parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Request
    Params = maps:get(?JSONRPC_FIELD_PARAMS, Data, undefined),
    {ok, #json_rpc_request{id = Id, method = Method, params = Params}};
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_RESULT := Result}) ->
    %% Success response
    {ok, #json_rpc_response{id = Id, result = Result, error = undefined}};
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_ERROR := Error}) ->
    %% Error response
    {ok, #json_rpc_response{id = Id, result = undefined, error = Error}};
parse_by_type(#{?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Notification
    Params = maps:get(?JSONRPC_FIELD_PARAMS, Data, undefined),
    {ok, #json_rpc_notification{method = Method, params = Params}};
parse_by_type(_) ->
    {error, {invalid_request, unknown_message_type}}.

validate_version(Data) when is_map(Data) ->
    case maps:get(?JSONRPC_FIELD_JSONRPC, Data, undefined) of
        ?JSONRPC_VERSION -> ok;
        undefined -> {error, {invalid_request, missing_jsonrpc}};
        Version -> {error, {invalid_request, {wrong_version, Version}}}
    end.

verify_request(Data, Id, Method, Params) ->
    ?assertEqual(?JSONRPC_VERSION, maps:get(?JSONRPC_FIELD_JSONRPC, Data)),
    ?assertEqual(Id, maps:get(?JSONRPC_FIELD_ID, Data)),
    ?assertEqual(Method, maps:get(?JSONRPC_FIELD_METHOD, Data)),
    case Params of
        undefined -> ?assertNot(maps:is_key(?JSONRPC_FIELD_PARAMS, Data));
        _ -> ?assertEqual(Params, maps:get(?JSONRPC_FIELD_PARAMS, Data))
    end.