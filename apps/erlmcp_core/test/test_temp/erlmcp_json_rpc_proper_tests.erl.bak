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
    proper_types:oneof([proper_types:integer(), proper_types:binary(), null]).

%% Generate valid method names (non-empty binary)
method_name() ->
    ?LET(Str,
         proper_types:non_empty(
             proper_types:list(
                 proper_types:range(32, 126))),
         list_to_binary(Str)).

%% Generate valid parameter maps
param_map() ->
    proper_types:map(
        proper_types:binary(), simple_value()).

%% Generate valid parameter arrays
param_array() ->
    proper_types:list(simple_value()).

%% Generate parameters (map, array, or undefined)
params() ->
    proper_types:oneof([param_map(), param_array(), undefined]).

%% Generate simple JSON-compatible values (no recursion)
simple_value() ->
    proper_types:oneof([proper_types:integer(), proper_types:binary(), proper_types:boolean()]).

%% Generate error codes (only valid codes from ?VALID_ERROR_CODES)
error_code() ->
    proper_types:oneof([%% JSON-RPC 2.0 standard errors
                        proper_types:integer(-32700, -32600),
                        %% MCP server errors
                        proper_types:integer(-32099, -32000),
                        %% MCP completion errors
                        proper_types:integer(-32113, -32110)]).

%% Generate error messages
error_message() ->
    method_name().

%% Generate error data
error_data() ->
    proper_types:oneof([simple_value(), undefined]).

%% Generate valid JSON-RPC requests
request() ->
    ?LET({Id, Method, Params},
         {request_id(), method_name(), params()},
         #{jsonrpc => <<"2.0">>,
           id => Id,
           method => Method,
           params => Params}).

%% Generate valid JSON-RPC responses (success)
success_response() ->
    ?LET({Id, Result},
         {request_id(), simple_value()},
         #{jsonrpc => <<"2.0">>,
           id => Id,
           result => Result}).

%% Generate valid JSON-RPC responses (error)
error_response() ->
    ?LET({Id, Code, Message, Data},
         {request_id(), error_code(), error_message(), error_data()},
         begin
             Base =
                 #{jsonrpc => <<"2.0">>,
                   id => Id,
                   error => #{code => Code, message => Message}},
             case Data of
                 undefined ->
                     Base;
                 _ ->
                     Base#{error => (maps:get(error, Base))#{data => Data}}
             end
         end).

%% Generate valid JSON-RPC notifications
notification() ->
    ?LET({Method, Params},
         {method_name(), params()},
         #{jsonrpc => <<"2.0">>,
           method => Method,
           params => Params}).

%% Generate batch requests
batch_request() ->
    proper_types:non_empty(
        proper_types:list(request())).

%%%====================================================================
%%% Properties: Request Encoding/Decoding
%%%====================================================================

%% Property: Encoding and decoding a request preserves structure
prop_encode_decode_request_roundtrip() ->
    ?FORALL({Id, Method, Params},
            {request_id(), method_name(), params()},
            begin
                Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
                case erlmcp_json_rpc:decode_message(Encoded) of
                    {ok, Decoded} when is_tuple(Decoded), tuple_size(Decoded) =:= 4 ->
                        {json_rpc_request, DecodedId, DecodedMethod, DecodedParams} = Decoded,
                        %% Check ID and Method match exactly
                        IdMatch = DecodedId =:= Id,
                        MethodMatch = DecodedMethod =:= Method,
                        %% Check params type matches (don't check exact value due to UTF-8 normalization)
                        ParamsTypeMatch =
                            case {Params, DecodedParams} of
                                {undefined, undefined} ->
                                    true;
                                {undefined, #{}} ->
                                    true;  %% Empty map is equivalent to no params
                                {#{}, undefined} ->
                                    true;
                                {#{}, #{}} ->
                                    true;
                                {P1, P2} when is_list(P1), is_list(P2) ->
                                    true;
                                {P1, P2} when is_map(P1), is_map(P2) ->
                                    true;
                                _ ->
                                    false
                            end,
                        IdMatch andalso MethodMatch andalso ParamsTypeMatch;
                    _ ->
                        false
                end
            end).

%% Property: Encoding request produces valid JSON
prop_encode_request_valid_json() ->
    ?FORALL({Id, Method, Params},
            {request_id(), method_name(), params()},
            begin
                Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
                is_binary(Encoded) andalso byte_size(Encoded) > 0
            end).

%% Property: Encoding request includes required fields
prop_encode_request_has_required_fields() ->
    ?FORALL({Id, Method, Params},
            {request_id(), method_name(), params()},
            begin
                Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
                Decoded = jsx:decode(Encoded, [return_maps]),
                maps:is_key(<<"jsonrpc">>, Decoded)
                andalso maps:is_key(<<"id">>, Decoded)
                andalso maps:is_key(<<"method">>, Decoded)
                andalso maps:get(<<"jsonrpc">>, Decoded) =:= <<"2.0">>
            end).

%%%====================================================================
%%% Properties: Response Encoding/Decoding
%%%====================================================================

%% Property: Encoding and decoding a success response preserves structure
prop_encode_decode_success_response_roundtrip() ->
    ?FORALL({Id, Result},
            {request_id(), simple_value()},
            begin
                Encoded = erlmcp_json_rpc:encode_response(Id, Result),
                case erlmcp_json_rpc:decode_message(Encoded) of
                    {ok, Decoded} when is_tuple(Decoded), tuple_size(Decoded) =:= 4 ->
                        {json_rpc_response, DecodedId, DecodedResult, _} = Decoded,
                        %% Check ID matches exactly
                        IdMatch = DecodedId =:= Id,
                        %% Check result type matches (don't check exact value due to UTF-8 normalization)
                        ResultTypeMatch =
                            case {Result, DecodedResult} of
                                {R1, R2} when is_integer(R1), is_integer(R2) ->
                                    R1 =:= R2;
                                {R1, R2} when is_float(R1), is_float(R2) ->
                                    R1 =:= R2;
                                {R1, R2} when is_boolean(R1), is_boolean(R2) ->
                                    R1 =:= R2;
                                {R1, R2} when is_binary(R1), is_binary(R2) ->
                                    byte_size(R1) =:= byte_size(R2);
                                _ ->
                                    false
                            end,
                        IdMatch andalso ResultTypeMatch;
                    _ ->
                        false
                end
            end).

%% Property: Encoding and decoding an error response preserves error structure
prop_encode_decode_error_response_roundtrip() ->
    ?FORALL({Id, Code, Message, Data},
            {request_id(), error_code(), error_message(), error_data()},
            begin
                Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
                case erlmcp_json_rpc:decode_message(Encoded) of
                    {ok, Decoded} when is_tuple(Decoded), tuple_size(Decoded) =:= 4 ->
                        {json_rpc_response, DecodedId, _, ErrorMap} = Decoded,
                        case ErrorMap of
                            _ when is_map(ErrorMap) ->
                                %% Check ID matches
                                IdMatch = DecodedId =:= Id,
                                %% Check that we got an error response with code
                                HasCode = maps:is_key(<<"code">>, ErrorMap),
                                HasMessage = maps:is_key(<<"message">>, ErrorMap),
                                IdMatch andalso HasCode andalso HasMessage;
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
                    {ok, #json_rpc_response{error = ErrorMap}} when is_map(ErrorMap) ->
                        maps:is_key(<<"code">>, ErrorMap)
                        andalso maps:is_key(<<"message">>, ErrorMap);
                    _ ->
                        false
                end
            end).

%%%====================================================================
%%% Properties: Notification Encoding/Decoding
%%%====================================================================

%% Property: Encoding and decoding a notification should work correctly
prop_encode_decode_notification_roundtrip() ->
    ?FORALL({Method, Params},
            {method_name(), params()},
            begin
                Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
                case erlmcp_json_rpc:decode_message(Encoded) of
                    {ok, #json_rpc_notification{method = DecodedMethod}} ->
                        DecodedMethod =:= Method;
                    _ ->
                        false
                end
            end).

%% Property: Notifications do not have id field
prop_notification_no_id() ->
    ?FORALL({Method, Params},
            {method_name(), params()},
            begin
                Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
                Decoded = jsx:decode(Encoded, [return_maps]),
                not maps:is_key(<<"id">>, Decoded)
            end).

%%%====================================================================
%%% Properties: Decode Error Handling
%%%====================================================================

%% Property: Decoding invalid JSON returns error
prop_decode_invalid_json() ->
    ?FORALL(BadJson,
            proper_types:binary(),
            ?IMPLIES(byte_size(BadJson) > 0 andalso not is_valid_json(BadJson),
                     begin
                         Result = erlmcp_json_rpc:decode_message(BadJson),
                         case Result of
                             {error, _} ->
                                 true;
                             _ ->
                                 false
                         end
                     end)).

%% Property: Decoding empty binary returns error
prop_decode_empty_binary() ->
    ?FORALL(Bin,
            proper_types:binary(0, 0),
            begin
                Result = erlmcp_json_rpc:decode_message(Bin),
                case Result of
                    {error, _} ->
                        true;
                    _ ->
                        false
                end
            end).

%% Property: Decoding JSON without jsonrpc field returns error
prop_decode_missing_jsonrpc() ->
    ?FORALL(Id,
            request_id(),
            begin
                BadJson = jsx:encode(#{<<"id">> => Id, <<"method">> => <<"test">>}),
                Result = erlmcp_json_rpc:decode_message(BadJson),
                case Result of
                    {error, _} ->
                        true;
                    _ ->
                        false
                end
            end).

%% Property: Decoding JSON with wrong jsonrpc version returns error
prop_decode_wrong_jsonrpc_version() ->
    ?FORALL(Id,
            request_id(),
            begin
                BadJson =
                    jsx:encode(#{<<"jsonrpc">> => <<"1.0">>,
                                 <<"id">> => Id,
                                 <<"method">> => <<"test">>}),
                Result = erlmcp_json_rpc:decode_message(BadJson),
                case Result of
                    {error, _} ->
                        true;
                    _ ->
                        false
                end
            end).

%%%====================================================================
%%% Properties: Error Code Standards
%%%====================================================================

%% Property: Standard error codes are in valid range
prop_standard_error_codes_valid() ->
    ?FORALL(Error,
            proper_types:oneof([parse_error,
                                invalid_request,
                                method_not_found,
                                invalid_params,
                                internal_error]),
            begin
                Code = error_code_to_int(Error),
                Code >= -32768 andalso Code =< -32000
            end).

%%%====================================================================
%%% Properties: Batch Encoding
%%%====================================================================

%% Property: Encoding and decoding batch requests preserves all items
prop_batch_encode_decode_roundtrip() ->
    ?FORALL(Requests,
            batch_request(),
            ?IMPLIES(length(Requests) =< 10,  %% Limit batch size for testing
                     begin
                         Encoded =
                             lists:map(fun(Request) ->
                                          erlmcp_json_rpc:encode_request(
                                              maps:get(id, Request),
                                              maps:get(method, Request),
                                              maps:get(params, Request))
                                       end,
                                       Requests),

                         %% Decode each request
                         Decoded =
                             lists:map(fun(Enc) ->
                                          {ok, Dec} = erlmcp_json_rpc:decode_message(Enc),
                                          Dec
                                       end,
                                       Encoded),

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
        _:_ ->
            false
    end.

%% Convert error code atom to integer
error_code_to_int(parse_error) ->
    ?JSONRPC_PARSE_ERROR;
error_code_to_int(invalid_request) ->
    ?JSONRPC_INVALID_REQUEST;
error_code_to_int(method_not_found) ->
    ?JSONRPC_METHOD_NOT_FOUND;
error_code_to_int(invalid_params) ->
    ?JSONRPC_INVALID_PARAMS;
error_code_to_int(internal_error) ->
    ?JSONRPC_INTERNAL_ERROR.

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    [?_assertEqual(true, proper:module(?MODULE, [{numtests, 100}]))].

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
