%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive tests for JSON-RPC error response structure
%%% according to MCP specification.
%%%
%%% Tests cover:
%%% 1. Parse error (-32700) with parsing context
%%% 2. Invalid request (-32600) with field information
%%% 3. Invalid params (-32602) with field information
%%% 4. Method not found (-32601) with method name
%%% 5. Internal error (-32603) with details
%%% 6. Version mismatch with supported versions
%%% 7. Feature not negotiated with feature name
%%% 8. Error responses with and without ID field
%%% 9. Error data structure validation
%%% 10. Error code and message consistency
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_error_responses_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup_test() ->
    ok.

teardown_test(_) ->
    ok.

%%====================================================================
%% Test Cases - Basic Error Response Structure
%%====================================================================

error_response_has_code_test() ->
    Id = 1,
    Code = ?JSONRPC_PARSE_ERROR,
    Message = <<"Parse error">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    % Decode and verify
    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    ?assert(is_record(Response, json_rpc_response)),
    ?assertEqual(1, Response#json_rpc_response.id),
    ?assertNotEqual(undefined, Response#json_rpc_response.error),
    Error = Response#json_rpc_response.error,
    ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error)).

error_response_has_message_test() ->
    Id = 2,
    Code = ?JSONRPC_INVALID_REQUEST,
    Message = <<"Invalid Request">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    ?assertEqual(Message, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error)).

error_response_without_data_test() ->
    Id = 3,
    Code = ?JSONRPC_METHOD_NOT_FOUND,
    Message = <<"Method not found">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    % Should NOT have data field if not provided
    ?assertNot(maps:is_key(?JSONRPC_ERROR_FIELD_DATA, Error)).

%%====================================================================
%% Test Cases - Error with Data Field
%%====================================================================

parse_error_with_context_test() ->
    Id = 4,
    Code = ?JSONRPC_PARSE_ERROR,
    Message = <<"Parse error">>,
    Data = #{<<"parsing_error">> => <<"Unexpected character at position 42">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error)),
    ?assertEqual(Message, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error)),
    ?assert(maps:is_key(?JSONRPC_ERROR_FIELD_DATA, Error)),
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"Unexpected character at position 42">>,
                 maps:get(<<"parsing_error">>, DataMap)).

invalid_request_with_field_info_test() ->
    Id = 5,
    Code = ?JSONRPC_INVALID_REQUEST,
    Message = <<"Invalid Request">>,
    Data = #{
        <<"field">> => <<"method">>,
        <<"reason">> => <<"method field is required">>
    },
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"method">>, maps:get(<<"field">>, DataMap)),
    ?assertEqual(<<"method field is required">>, maps:get(<<"reason">>, DataMap)).

invalid_params_with_field_info_test() ->
    Id = 6,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Data = #{
        <<"field">> => <<"uri">>,
        <<"reason">> => <<"uri must be a string">>
    },
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"uri">>, maps:get(<<"field">>, DataMap)),
    ?assertEqual(<<"uri must be a string">>, maps:get(<<"reason">>, DataMap)).

method_not_found_with_method_name_test() ->
    Id = 7,
    Code = ?JSONRPC_METHOD_NOT_FOUND,
    Message = <<"Method not found">>,
    Data = #{<<"method">> => <<"invalid_method">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"invalid_method">>, maps:get(<<"method">>, DataMap)).

internal_error_with_details_test() ->
    Id = 8,
    Code = ?JSONRPC_INTERNAL_ERROR,
    Message = <<"Internal error">>,
    Data = #{<<"details">> => <<"Database connection failed">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"Database connection failed">>, maps:get(<<"details">>, DataMap)).

%%====================================================================
%% Test Cases - MCP Custom Errors
%%====================================================================

version_mismatch_with_supported_versions_test() ->
    Id = 9,
    Code = ?JSONRPC_INVALID_REQUEST,
    Message = <<"Unsupported protocol version">>,
    Data = #{
        <<"supported">> => [<<"2025-06-18">>, <<"2024-11-05">>],
        <<"requested">> => <<"2024-01-01">>
    },
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual([<<"2025-06-18">>, <<"2024-11-05">>], maps:get(<<"supported">>, DataMap)),
    ?assertEqual(<<"2024-01-01">>, maps:get(<<"requested">>, DataMap)).

feature_not_negotiated_test() ->
    Id = 10,
    Code = ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED,
    Message = <<"Feature not negotiated">>,
    Data = #{<<"feature">> => <<"resources">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"resources">>, maps:get(<<"feature">>, DataMap)).

resource_not_found_with_uri_test() ->
    Id = 11,
    Code = ?MCP_ERROR_RESOURCE_NOT_FOUND,
    Message = <<"Resource not found">>,
    Data = #{<<"uri">> => <<"file:///path/to/missing/resource">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"file:///path/to/missing/resource">>, maps:get(<<"uri">>, DataMap)).

tool_not_found_with_name_test() ->
    Id = 12,
    Code = ?MCP_ERROR_TOOL_NOT_FOUND,
    Message = <<"Tool not found">>,
    Data = #{<<"tool_name">> => <<"unknown_tool">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"unknown_tool">>, maps:get(<<"tool_name">>, DataMap)).

%%====================================================================
%% Test Cases - Error Codes Consistency
%%====================================================================

standard_error_codes_test() ->
    TestCases = [
        {?JSONRPC_PARSE_ERROR, <<"Parse error">>},
        {?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>},
        {?JSONRPC_METHOD_NOT_FOUND, <<"Method not found">>},
        {?JSONRPC_INVALID_PARAMS, <<"Invalid params">>},
        {?JSONRPC_INTERNAL_ERROR, <<"Internal error">>}
    ],

    lists:foreach(fun({Code, Message}) ->
        Json = erlmcp_json_rpc:encode_error_response(1, Code, Message),
        {ok, Response} = erlmcp_json_rpc:decode_message(Json),
        Error = Response#json_rpc_response.error,
        ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error))
    end, TestCases).

mcp_error_codes_test() ->
    TestCases = [
        {?MCP_ERROR_RESOURCE_NOT_FOUND, <<"Resource not found">>},
        {?MCP_ERROR_TOOL_NOT_FOUND, <<"Tool not found">>},
        {?MCP_ERROR_PROMPT_NOT_FOUND, <<"Prompt not found">>},
        {?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, <<"Capability not supported">>}
    ],

    lists:foreach(fun({Code, Message}) ->
        Json = erlmcp_json_rpc:encode_error_response(1, Code, Message),
        {ok, Response} = erlmcp_json_rpc:decode_message(Json),
        Error = Response#json_rpc_response.error,
        ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error))
    end, TestCases).

%%====================================================================
%% Test Cases - Error Response with Different ID Types
%%====================================================================

error_response_with_integer_id_test() ->
    Id = 123,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(123, Response#json_rpc_response.id).

error_response_with_binary_id_test() ->
    Id = <<"abc-123-def">>,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(<<"abc-123-def">>, Response#json_rpc_response.id).

error_response_with_null_id_test() ->
    % Some errors may have null ID for unparseable requests
    Id = null,
    Code = ?JSONRPC_PARSE_ERROR,
    Message = <<"Parse error">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(null, Response#json_rpc_response.id).

%%====================================================================
%% Test Cases - Create Error Helper Functions
%%====================================================================

create_error_with_basic_data_test() ->
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Data = #{<<"field">> => <<"uri">>},

    Error = erlmcp_json_rpc:create_error(Code, Message, Data),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assertEqual(Data, Error#mcp_error.data).

create_error_with_undefined_data_test() ->
    Code = ?JSONRPC_INTERNAL_ERROR,
    Message = <<"Internal error">>,

    Error = erlmcp_json_rpc:create_error(Code, Message, undefined),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assertEqual(undefined, Error#mcp_error.data).

create_error_with_atom_key_test() ->
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,

    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, field, <<"uri">>),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    Data = Error#mcp_error.data,
    ?assertEqual(<<"uri">>, maps:get(<<"field">>, Data)).

%%====================================================================
%% Test Cases - Complex Data Structures
%%====================================================================

error_with_nested_data_test() ->
    Id = 20,
    Code = ?JSONRPC_INVALID_REQUEST,
    Message = <<"Invalid Request">>,
    Data = #{
        <<"validation">> => #{
            <<"field">> => <<"params">>,
            <<"schema_errors">> => [
                #{<<"path">> => <<"$.uri">>, <<"error">> => <<"required">>},
                #{<<"path">> => <<"$.type">>, <<"error">> => <<"invalid type">>}
            ]
        }
    },
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    Validation = maps:get(<<"validation">>, DataMap),
    ?assertEqual(<<"params">>, maps:get(<<"field">>, Validation)),
    ?assertEqual(2, length(maps:get(<<"schema_errors">>, Validation))).

error_with_array_data_test() ->
    Id = 21,
    Code = ?JSONRPC_INVALID_REQUEST,
    Message = <<"Multiple validation errors">>,
    Data = #{
        <<"errors">> => [
            <<"Field 1 is invalid">>,
            <<"Field 2 is required">>,
            <<"Field 3 has wrong type">>
        ]
    },
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    Errors = maps:get(<<"errors">>, DataMap),
    ?assertEqual(3, length(Errors)).

%%====================================================================
%% Test Cases - JSON Encoding/Decoding
%%====================================================================

error_response_json_format_test() ->
    Id = 22,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Data = #{<<"field">> => <<"uri">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    % Verify it's valid binary JSON
    ?assert(is_binary(Json)),

    % Decode the JSON directly
    DecodedMap = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedMap)),
    ?assertEqual(22, maps:get(<<"id">>, DecodedMap)),
    ?assert(maps:is_key(<<"error">>, DecodedMap)),

    ErrorMap = maps:get(<<"error">>, DecodedMap),
    ?assertEqual(Code, maps:get(<<"code">>, ErrorMap)),
    ?assertEqual(Message, maps:get(<<"message">>, ErrorMap)),
    ?assert(maps:is_key(<<"data">>, ErrorMap)).

error_response_roundtrip_test() ->
    % Test that an error response can be encoded and decoded without loss
    Id = 23,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Data = #{
        <<"field">> => <<"uri">>,
        <<"reason">> => <<"must be a string">>,
        <<"value">> => 123
    },

    % Encode
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    % Decode
    {ok, Response} = erlmcp_json_rpc:decode_message(Json),

    % Verify round-trip
    Error = Response#json_rpc_response.error,
    ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error)),
    ?assertEqual(Message, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error)),

    DecodedData = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(<<"uri">>, maps:get(<<"field">>, DecodedData)),
    ?assertEqual(<<"must be a string">>, maps:get(<<"reason">>, DecodedData)),
    ?assertEqual(123, maps:get(<<"value">>, DecodedData)).

%%====================================================================
%% Test Cases - Error Response Combinations
%%====================================================================

all_standard_errors_with_data_test() ->
    % Test each standard error code with appropriate data
    TestCases = [
        {?JSONRPC_PARSE_ERROR, <<"Parse error">>, #{<<"parsing_error">> => <<"invalid JSON">>}},
        {?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>, #{<<"field">> => <<"id">>}},
        {?JSONRPC_METHOD_NOT_FOUND, <<"Method not found">>, #{<<"method">> => <<"unknown">>}},
        {?JSONRPC_INVALID_PARAMS, <<"Invalid params">>, #{<<"field">> => <<"params">>}},
        {?JSONRPC_INTERNAL_ERROR, <<"Internal error">>, #{<<"details">> => <<"unexpected error">>}}
    ],

    lists:foreach(fun({Code, Message, Data}) ->
        Json = erlmcp_json_rpc:encode_error_response(1, Code, Message, Data),
        {ok, Response} = erlmcp_json_rpc:decode_message(Json),
        Error = Response#json_rpc_response.error,
        ?assertEqual(Code, maps:get(?JSONRPC_ERROR_FIELD_CODE, Error)),
        ?assertEqual(Message, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error)),
        ?assert(maps:is_key(?JSONRPC_ERROR_FIELD_DATA, Error))
    end, TestCases).

%%====================================================================
%% Test Cases - Edge Cases
%%====================================================================

error_with_empty_data_test() ->
    Id = 30,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Data = #{},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(0, maps:size(DataMap)).

error_with_unicode_message_test() ->
    Id = 31,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params: 日本語"/utf8>>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    ?assertEqual(Message, maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, Error)).

error_with_large_data_test() ->
    Id = 32,
    Code = ?JSONRPC_INTERNAL_ERROR,
    Message = <<"Internal error">>,
    LargeString = binary:copy(<<"x">>, 10000),
    Data = #{<<"large">> => LargeString},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    Error = Response#json_rpc_response.error,
    DataMap = maps:get(?JSONRPC_ERROR_FIELD_DATA, Error),
    ?assertEqual(LargeString, maps:get(<<"large">>, DataMap)).

%%====================================================================
%% Test Cases - Actual Server Integration
%%====================================================================

error_response_has_jsonrpc_version_test() ->
    Id = 40,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    % Verify the raw JSON has jsonrpc field
    DecodedMap = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedMap)).

error_has_no_result_field_test() ->
    Id = 41,
    Code = ?JSONRPC_INTERNAL_ERROR,
    Message = <<"Internal error">>,
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    DecodedMap = jsx:decode(Json, [return_maps]),
    % Error response should NOT have result field
    ?assertNot(maps:is_key(<<"result">>, DecodedMap)),
    % But should have error field
    ?assert(maps:is_key(<<"error">>, DecodedMap)).

error_and_result_mutually_exclusive_test() ->
    % Verify that a response has either error or result, never both
    Id = 42,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid params">>,
    Data = #{<<"field">> => <<"test">>},
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    DecodedMap = jsx:decode(Json, [return_maps]),
    HasError = maps:is_key(<<"error">>, DecodedMap),
    HasResult = maps:is_key(<<"result">>, DecodedMap),

    % XOR - should have exactly one
    ?assert(HasError xor HasResult).
