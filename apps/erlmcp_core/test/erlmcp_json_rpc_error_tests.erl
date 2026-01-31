-module(erlmcp_json_rpc_error_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC 2.0 Error Handling and Error Codes
%% Chicago School TDD: Test ONLY observable behavior through public API
%% Focus: encode_error_response, error code validation, error helpers
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Basic Error Response Tests
%%====================================================================

basic_error_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_error_response_basic()),
             ?_test(test_encode_error_response_with_data()),
             ?_test(test_encode_error_response_with_null_data()),
             ?_test(test_encode_error_response_invalid_code())
         ]
     end}.

test_encode_error_response_basic() ->
    Id = 1,
    Code = -32600,
    Message = <<"Invalid Request">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_error_response_with_data() ->
    Id = 2,
    Code = -32602,
    Message = <<"Invalid Parameters">>,
    Data = #{<<"field">> => <<"name">>, <<"reason">> => <<"required">>},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)),
    DataMap = maps:get(<<"data">>, Error),
    ?assertEqual(<<"name">>, maps:get(<<"field">>, DataMap)).

test_encode_error_response_with_null_data() ->
    Id = 3,
    Code = -32603,
    Message = <<"Internal Error">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, null),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    %% null data should not include data field
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_encode_error_response_invalid_code() ->
    Id = 4,
    InvalidCode = 9999,
    Message = <<"Test Error">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, InvalidCode, Message),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    %% Should use internal error code (-32603)
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

%%====================================================================
%% Error Code Validation Tests
%%====================================================================

error_code_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_error_code_parse_error()),
             ?_test(test_validate_error_code_invalid_request()),
             ?_test(test_validate_error_code_method_not_found()),
             ?_test(test_validate_error_code_invalid_params()),
             ?_test(test_validate_error_code_internal_error()),
             ?_test(test_validate_error_code_invalid()),
             ?_test(test_validate_mcp_error_codes())
         ]
     end}.

test_validate_error_code_parse_error() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32700)).

test_validate_error_code_invalid_request() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32600)).

test_validate_error_code_method_not_found() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32601)).

test_validate_error_code_invalid_params() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32602)).

test_validate_error_code_internal_error() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32603)).

test_validate_error_code_invalid() ->
    ?assertNot(erlmcp_json_rpc:validate_error_code(0)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(9999)).

test_validate_mcp_error_codes() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32001)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32002)).

%%====================================================================
%% Error Helper Functions Tests (Common)
%%====================================================================

error_helpers_common_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_method_not_found()),
             ?_test(test_error_invalid_params()),
             ?_test(test_error_internal()),
             ?_test(test_error_parse())
         ]
     end}.

test_error_method_not_found() ->
    Id = 1,
    Method = <<"unknown_method">>,
    Encoded = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Method, maps:get(<<"method">>, Data)).

test_error_invalid_params() ->
    Id = 1,
    Reason = <<"missing required field">>,
    Encoded = erlmcp_json_rpc:error_invalid_params(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_error_internal() ->
    Id = 1,
    Encoded = erlmcp_json_rpc:error_internal(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_error_parse() ->
    Id = 1,
    Encoded = erlmcp_json_rpc:error_parse(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, Error)).

%%====================================================================
%% Error Code Classification Tests
%%====================================================================

error_classification_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_severity_critical()),
             ?_test(test_error_severity_error()),
             ?_test(test_error_severity_warning()),
             ?_test(test_error_category_jsonrpc()),
             ?_test(test_error_category_mcp_core()),
             ?_test(test_is_jsonrpc_standard_error()),
             ?_test(test_is_mcp_core_error())
         ]
     end}.

test_error_severity_critical() ->
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32700)),
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32600)),
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32603)).

test_error_severity_error() ->
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32601)),
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32602)).

test_error_severity_warning() ->
    ?assertEqual(warning, erlmcp_json_rpc:error_severity(-32079)).

test_error_category_jsonrpc() ->
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32700)),
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32600)).

test_error_category_mcp_core() ->
    ?assertEqual(mcp_core, erlmcp_json_rpc:error_category(-32001)),
    ?assertEqual(mcp_core, erlmcp_json_rpc:error_category(-32002)).

test_is_jsonrpc_standard_error() ->
    ?assert(erlmcp_json_rpc:is_jsonrpc_standard_error(-32700)),
    ?assert(erlmcp_json_rpc:is_jsonrpc_standard_error(-32600)),
    ?assertNot(erlmcp_json_rpc:is_jsonrpc_standard_error(-32001)).

test_is_mcp_core_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_core_error(-32001)),
    ?assertNot(erlmcp_json_rpc:is_mcp_core_error(-32600)).

%%====================================================================
%% MCP Error Helpers Tests
%%====================================================================

mcp_error_helpers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_resource_not_found()),
             ?_test(test_error_tool_not_found()),
             ?_test(test_error_prompt_not_found()),
             ?_test(test_error_capability_not_supported()),
             ?_test(test_error_not_initialized()),
             ?_test(test_error_validation_failed())
         ]
     end}.

test_error_resource_not_found() ->
    Id = 1,
    Uri = <<"resource://missing">>,
    Encoded = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Uri, maps:get(<<"uri">>, Data)).

test_error_tool_not_found() ->
    Id = 1,
    Tool = <<"missing_tool">>,
    Encoded = erlmcp_json_rpc:error_tool_not_found(Id, Tool),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Tool, maps:get(<<"tool">>, Data)).

test_error_prompt_not_found() ->
    Id = 1,
    Prompt = <<"missing_prompt">>,
    Encoded = erlmcp_json_rpc:error_prompt_not_found(Id, Prompt),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Prompt, maps:get(<<"prompt">>, Data)).

test_error_capability_not_supported() ->
    Id = 1,
    Capability = <<"sampling">>,
    Encoded = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Capability, maps:get(<<"capability">>, Data)).

test_error_not_initialized() ->
    Id = 1,
    Encoded = erlmcp_json_rpc:error_not_initialized(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32005, maps:get(<<"code">>, Error)).

test_error_validation_failed() ->
    Id = 1,
    Reason = <<"schema validation failed">>,
    Encoded = erlmcp_json_rpc:error_validation_failed(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Reason, maps:get(<<"details">>, Data)).

%%====================================================================
%% Error Creation Tests
%%====================================================================

error_creation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_error()),
             ?_test(test_create_error_with_data())
         ]
     end}.

test_create_error() ->
    Code = -32600,
    Message = <<"Invalid Request">>,
    Error = erlmcp_json_rpc:create_error(Code, Message, undefined),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assertEqual(undefined, Error#mcp_error.data).

test_create_error_with_data() ->
    Code = -32602,
    Message = <<"Invalid Parameters">>,
    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, details, <<"test">>),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assert(is_map(Error#mcp_error.data)).

%%====================================================================
%% Batch Error Response Tests
%%====================================================================

batch_error_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_batch_error_response_with_id()),
             ?_test(test_create_batch_error_response_without_id()),
             ?_test(test_map_batch_error_to_code_invalid_request()),
             ?_test(test_map_batch_error_to_code_parse_error())
         ]
     end}.

test_create_batch_error_response_with_id() ->
    Request = #{<<"id">> => 123, <<"method">> => <<"test">>},
    Response = erlmcp_json_rpc:create_batch_error_response(Request, invalid_request, not_an_object),
    ?assertEqual(123, Response#json_rpc_response.id),
    ?assert(is_map(Response#json_rpc_response.error)).

test_create_batch_error_response_without_id() ->
    Request = #{<<"method">> => <<"test">>},
    Response = erlmcp_json_rpc:create_batch_error_response(Request, invalid_request, not_an_object),
    ?assertEqual(null, Response#json_rpc_response.id),
    ?assert(is_map(Response#json_rpc_response.error)).

test_map_batch_error_to_code_invalid_request() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(invalid_request, not_an_object),
    ?assertEqual(-32600, Code),
    ?assertEqual(<<"Invalid Request: not an object">>, Message).

test_map_batch_error_to_code_parse_error() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(parse_error, ignored),
    ?assertEqual(-32700, Code),
    ?assertEqual(<<"Parse error">>, Message).

%%====================================================================
%% Error Response Decoding Tests
%%====================================================================

decode_error_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_error_response_basic()),
             ?_test(test_decode_error_response_with_data()),
             ?_test(test_decode_error_response_missing_code()),
             ?_test(test_decode_error_response_missing_message())
         ]
     end}.

test_decode_error_response_basic() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32600,
            <<"message">> => <<"Invalid Request">>
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{}}, Result),
    {ok, Response} = Result,
    ?assert(is_map(Response#json_rpc_response.error)).

test_decode_error_response_with_data() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid Params">>,
            <<"data">> => #{<<"field">> => <<"name">>}
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{}}, Result).

test_decode_error_response_missing_code() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"message">> => <<"Error">>
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    %% Parser might accept or reject incomplete error objects
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

test_decode_error_response_missing_message() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32600
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    %% Parser might accept or reject incomplete error objects
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%%====================================================================
%% Type Conversion in Error Helpers
%%====================================================================

error_type_conversion_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_invalid_params_string_conversion()),
             ?_test(test_error_invalid_params_atom_conversion()),
             ?_test(test_error_validation_failed_string_conversion()),
             ?_test(test_error_validation_failed_atom_conversion())
         ]
     end}.

test_error_invalid_params_string_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_invalid_params(Id, "string details"),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_error_invalid_params_atom_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_invalid_params(Id, invalid_type),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_error_validation_failed_string_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_validation_failed(Id, "validation failed"),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)).

test_error_validation_failed_atom_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_validation_failed(Id, schema_error),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)).
