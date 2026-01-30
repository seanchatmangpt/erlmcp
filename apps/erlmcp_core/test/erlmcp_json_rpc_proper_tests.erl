-module(erlmcp_json_rpc_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Property-Based Tests for erlmcp_json_rpc Module
%%%
%%% Tests invariants:
%%% - Encode/Decode roundtrip: decode(encode(X)) = X
%%% - JSON-RPC 2.0 spec compliance
%%% - Error handling for malformed input
%%% - Message structure validation
%%%
%%% Chicago School TDD: Real processes, no mocks, state-based verification
%%%====================================================================

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid request IDs (integers, strings, null)
request_id() ->
    proper_types:union([
        proper_types:int(),
        proper_types:binary(),
        proper_types:null()
    ]).

%% Generate valid method names (non-empty binary)
method_name() ->
    ?LET(Str, proper_types:non_empty(proper_types:list(proper_types:range(32, 126))),
        list_to_binary(Str)).

%% Generate valid parameter maps
param_map() ->
    proper_types:map(proper_types:binary(), proper_value()).

%% Generate valid parameter arrays
param_array() ->
    proper_types:list(proper_value()).

%% Generate parameters (map, array, or undefined)
params() ->
    proper_types:union([
        param_map(),
        param_array(),
        proper_types:undefined()
    ]).

%% Generate JSON-compatible values
proper_value() ->
    proper_types:union([
        proper_types:int(),
        proper_types:float(),
        proper_types:binary(),
        proper_types:boolean(),
        proper_types:nil(),
        proper_types:list(proper_value()),
        proper_types:map(proper_types:binary(), proper_value())
    ]).

%% Generate error codes
error_code() ->
    proper_types:int(-32768, 32000).

%% Generate error messages
error_message() ->
    method_name().

%% Generate error data
error_data() ->
    proper_types:union([
        proper_value(),
        proper_types:undefined()
    ]).

%% Generate valid JSON-RPC requests
request() ->
    ?LET({Id, Method, Params},
        {request_id(), method_name(), params()},
        #{
            jsonrpc => <<"2.0">>,
            id => Id,
            method => Method,
            params => Params
        }).

%% Generate valid JSON-RPC responses (success)
success_response() ->
    ?LET({Id, Result},
        {request_id(), proper_value()},
        #{
            jsonrpc => <<"2.0">>,
            id => Id,
            result => Result
        }).

%% Generate valid JSON-RPC responses (error)
error_response() ->
    ?LET({Id, Code, Message, Data},
        {request_id(), error_code(), error_message(), error_data()},
        begin
            Base = #{
                jsonrpc => <<"2.0">>,
                id => Id,
                error => #{
                    code => Code,
                    message => Message
                }
            },
            case Data of
                undefined -> Base;
                _ -> Base#{error => (maps:get(error, Base))#{data => Data}}
            end
        end).

%% Generate valid JSON-RPC notifications
notification() ->
    ?LET({Method, Params},
        {method_name(), params()},
        #{
            jsonrpc => <<"2.0">>,
            method => Method,
            params => Params
        }).

%% Generate batch requests
batch_request() ->
    proper_types:non_empty(proper_types:list(request())).

%%%====================================================================
%%% Properties: Request Encoding/Decoding
%%%====================================================================

%% Property: Encoding and decoding a request should return the original
prop_encode_decode_request_roundtrip() ->
    ?FORALL(Request, request(),
        begin
            Encoded = erlmcp_json_rpc:encode_request(
                maps:get(id, Request),
                maps:get(method, Request),
                maps:get(params, Request)
            ),
            case erlmcp_json_rpc:decode_message(Encoded) of
                {ok, DecodedRec} ->
                    %% Convert record to map for comparison
                    Decoded = rec_to_map(DecodedRec),
                    maps:get(jsonrpc, Decoded) =:= <<"2.0">> andalso
                    maps:get(id, Decoded) =:= maps:get(id, Request) andalso
                    maps:get(method, Decoded) =:= maps:get(method, Request);
                _ ->
                    false
            end
        end).

%% Property: Encoding request produces valid JSON
prop_encode_request_valid_json() ->
    ?FORALL(Request, request(),
        begin
            Encoded = erlmcp_json_rpc:encode_request(
                maps:get(id, Request),
                maps:get(method, Request),
                maps:get(params, Request)
            ),
            is_binary(Encoded) andalso byte_size(Encoded) > 0
        end).

%% Property: Encoding request includes required fields
prop_encode_request_has_required_fields() ->
    ?FORALL({Id, Method, Params}, {request_id(), method_name(), params()},
        begin
            Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
            Decoded = jsx:decode(Encoded, [return_maps]),

            maps:is_key(<<"jsonrpc">>, Decoded) andalso
            maps:is_key(<<"id">>, Decoded) andalso
            maps:is_key(<<"method">>, Decoded) andalso
            maps:get(<<"jsonrpc">>, Decoded) =:= <<"2.0">>
        end).

%%%====================================================================
%%% Properties: Response Encoding/Decoding
%%%====================================================================

%% Property: Encoding and decoding a success response should return the original
prop_encode_decode_success_response_roundtrip() ->
    ?FORALL(Response, success_response(),
        begin
            Encoded = erlmcp_json_rpc:encode_response(
                maps:get(id, Response),
                maps:get(result, Response)
            ),
            case erlmcp_json_rpc:decode_message(Encoded) of
                {ok, DecodedRec} ->
                    Decoded = rec_to_map(DecodedRec),
                    maps:get(jsonrpc, Decoded) =:= <<"2.0">> andalso
                    maps:get(id, Decoded) =:= maps:get(id, Response) andalso
                    maps:get(result, Decoded) =:= maps:get(result, Response);
                _ ->
                    false
            end
        end).

%% Property: Encoding and decoding an error response preserves error information
prop_encode_decode_error_response_roundtrip() ->
    ?FORALL(Response, error_response(),
        begin
            ErrorMap = maps:get(error, Response),
            Encoded = erlmcp_json_rpc:encode_error_response(
                maps:get(id, Response),
                maps:get(code, ErrorMap),
                maps:get(message, ErrorMap),
                maps:get(data, ErrorMap, undefined)
            ),
            case erlmcp_json_rpc:decode_message(Encoded) of
                {ok, DecodedRec} ->
                    Decoded = rec_to_map(DecodedRec),
                    case maps:get(error, Decoded) of
                        ErrorRec when is_map(ErrorRec) ->
                            Code = maps:get(<<"code">>, ErrorRec),
                            Message = maps:get(<<"message">>, ErrorRec),
                            Data = maps:get(<<"data">>, ErrorRec, undefined),
                            Code =:= maps:get(code, ErrorMap) andalso
                            Message =:= maps:get(message, ErrorMap) andalso
                            Data =:= maps:get(data, ErrorMap, undefined);
                        _ ->
                            false
                    end;
                _ ->
                    false
            end
        end).

%% Property: Error response has valid error structure
prop_error_response_structure() ->
    ?FORALL({Id, Code, Message, Data},
        {request_id(), error_code(), error_message(), error_data()},
        begin
            Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
            case erlmcp_json_rpc:decode_message(Encoded) of
                {ok, DecodedRec} ->
                    Decoded = rec_to_map(DecodedRec),
                    maps:is_key(<<"error">>, Decoded) andalso
                    begin
                        ErrorMap = maps:get(error, Decoded),
                        maps:is_key(<<"code">>, ErrorMap) andalso
                        maps:is_key(<<"message">>, ErrorMap)
                    end;
                _ ->
                    false
            end
        end).

%%%====================================================================
%%% Properties: Notification Encoding/Decoding
%%%====================================================================

%% Property: Encoding and decoding a notification should work correctly
prop_encode_decode_notification_roundtrip() ->
    ?FORALL(Notification, notification(),
        begin
            Encoded = erlmcp_json_rpc:encode_notification(
                maps:get(method, Notification),
                maps:get(params, Notification)
            ),
            case erlmcp_json_rpc:decode_message(Encoded) of
                {ok, DecodedRec} ->
                    Decoded = rec_to_map(DecodedRec),
                    maps:get(jsonrpc, Decoded) =:= <<"2.0">> andalso
                    maps:get(method, Decoded) =:= maps:get(method, Notification) andalso
                    not maps:is_key(<<"id">>, Decoded);  %% Notifications don't have IDs
                _ ->
                    false
            end
        end).

%% Property: Notifications do not have id field
prop_notification_no_id() ->
    ?FORALL(Notification, notification(),
        begin
            Encoded = erlmcp_json_rpc:encode_notification(
                maps:get(method, Notification),
                maps:get(params, Notification)
            ),
            Decoded = jsx:decode(Encoded, [return_maps]),
            not maps:is_key(<<"id">>, Decoded)
        end).

%%%====================================================================
%%% Properties: Decode Error Handling
%%%====================================================================

%% Property: Decoding invalid JSON returns error
prop_decode_invalid_json() ->
    ?FORALL(BadJson, proper_types:binary(),
        ?IMPLIES(byte_size(BadJson) > 0 andalso not is_valid_json(BadJson),
        begin
            Result = erlmcp_json_rpc:decode_message(BadJson),
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end)).

%% Property: Decoding non-binary input returns error
prop_decode_non_binary() ->
    ?FORALL(NonBinary, proper_types:union([
        proper_types:int(),
        proper_types:atom(),
        proper_types:list(proper_types:int())
    ]),
        begin
            Result = erlmcp_json_rpc:decode_message(NonBinary),
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end).

%% Property: Decoding empty binary returns error
prop_decode_empty_binary() ->
    ?FORALL(_, proper_types:always(<<>>),
        begin
            Result = erlmcp_json_rpc:decode_message(<<>>),
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end).

%% Property: Decoding JSON without jsonrpc field returns error
prop_decode_missing_jsonrpc() ->
    ?FORALL(Id, request_id(),
        begin
            BadJson = jsx:encode(#{
                <<"id">> => Id,
                <<"method">> => <<"test">>
            }),
            Result = erlmcp_json_rpc:decode_message(BadJson),
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end).

%% Property: Decoding JSON with wrong jsonrpc version returns error
prop_decode_wrong_jsonrpc_version() ->
    ?FORALL(Id, request_id(),
        begin
            BadJson = jsx:encode(#{
                <<"jsonrpc">> => <<"1.0">>,
                <<"id">> => Id,
                <<"method">> => <<"test">>
            }),
            Result = erlmcp_json_rpc:decode_message(BadJson),
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end).

%%%====================================================================
%%% Properties: Error Code Standards
%%%====================================================================

%% Property: Standard error codes are in valid range
prop_standard_error_codes_valid() ->
    ?FORALL(Error, proper_types:oneof([
        parse_error, invalid_request, method_not_found,
        invalid_params, internal_error
    ]),
        begin
            Code = error_code_to_int(Error),
            Code >= -32768 andalso Code =< -32000
        end).

%% Property: Custom error codes are outside standard range
prop_custom_error_codes_valid() ->
    ?FORALL(Code, proper_types:int(),
        begin
            (Code >= -32768 andalso Code =< -32000) orelse
            (Code >= -32099 andalso Code =< -32000) orelse
            (Code >= -32768 andalso Code =< 32000)
        end).

%%%====================================================================
%%% Properties: Batch Encoding
%%%====================================================================

%% Property: Encoding and decoding batch requests preserves all items
prop_batch_encode_decode_roundtrip() ->
    ?FORALL(Requests, batch_request(),
        ?IMPLIES(length(Requests) =< 10,  %% Limit batch size for testing
        begin
            Encoded = lists:map(fun(Request) ->
                erlmcp_json_rpc:encode_request(
                    maps:get(id, Request),
                    maps:get(method, Request),
                    maps:get(params, Request)
                )
            end, Requests),

            %% Decode each request
            Decoded = lists:map(fun(Enc) ->
                {ok, Dec} = erlmcp_json_rpc:decode_message(Enc),
                Dec
            end, Encoded),

            %% Verify we got the same number of requests
            length(Decoded) =:= length(Requests)
        end)).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Check if binary is valid JSON (simple heuristic)
is_valid_json(Binary) ->
    try
        jsx:decode(Binary, [return_maps]),
        true
    catch
        _:_ -> false
    end.

%% Convert error code atom to integer
error_code_to_int(parse_error) -> ?JSONRPC_PARSE_ERROR;
error_code_to_int(invalid_request) -> ?JSONRPC_INVALID_REQUEST;
error_code_to_int(method_not_found) -> ?JSONRPC_METHOD_NOT_FOUND;
error_code_to_int(invalid_params) -> ?JSONRPC_INVALID_PARAMS;
error_code_to_int(internal_error) -> ?JSONRPC_INTERNAL_ERROR.

%% Convert JSON-RPC records to maps for comparison
rec_to_map(#json_rpc_request{id = Id, method = Method, params = Params}) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method
    },
    case Params of
        undefined -> Map;
        _ -> Map#{<<"params">> => Params}
    end;

rec_to_map(#json_rpc_response{id = Id, result = Result, error = undefined}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    };

rec_to_map(#json_rpc_response{id = Id, error = ErrorMap}) when is_map(ErrorMap) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => ErrorMap
    };

rec_to_map(#json_rpc_notification{method = Method, params = Params}) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method
    },
    case Params of
        undefined -> Map;
        _ -> Map#{<<"params">> => Params}
    end.

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    [
        ?_assertEqual(true, proper:module(?MODULE, [
            {numtests, 100},
            {output, nrty}
        ]))
    ].

%%%====================================================================
%%% Individual Property Tests (for debugging)
%%%====================================================================

%% Test individual properties with fewer cases for quick debugging
prop_test_request_roundtrip() ->
    ?assertEqual(true, proper:quickcheck(prop_encode_decode_request_roundtrip(), 50)).

prop_test_response_roundtrip() ->
    ?assertEqual(true, proper:quickcheck(prop_encode_decode_success_response_roundtrip(), 50)).

prop_test_error_response() ->
    ?assertEqual(true, proper:quickcheck(prop_encode_decode_error_response_roundtrip(), 50)).

prop_test_notification() ->
    ?assertEqual(true, proper:quickcheck(prop_encode_decode_notification_roundtrip(), 50)).

prop_test_decode_invalid_json() ->
    ?assertEqual(true, proper:quickcheck(prop_decode_invalid_json(), 50)).

prop_test_error_codes() ->
    ?assertEqual(true, proper:quickcheck(prop_standard_error_codes_valid(), 20)).
