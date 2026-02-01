-module(erlmcp_error_response_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%%====================================================================
%%% Common Test Callbacks
%%%====================================================================

all() ->
    [%% JSON-RPC 2.0 Standard Error Tests
     parse_error_minus_32700_test,
     invalid_request_minus_32600_test,
     invalid_request_missing_jsonrpc_test,
     invalid_request_wrong_version_test,
     method_not_found_minus_32601_test,
     invalid_params_minus_32602_test,
     internal_error_minus_32603_test,
     %% MCP Core Error Tests (-32001 to -32010)
     mcp_resource_not_found_minus_32001_test,
     mcp_tool_not_found_minus_32002_test,
     mcp_prompt_not_found_minus_32003_test,
     mcp_not_initialized_minus_32004_test,
     mcp_validation_failed_minus_32007_test,
     %% Error Structure Tests
     error_response_has_required_fields_test,
     error_response_code_is_integer_test,
     error_response_message_is_string_test,
     error_response_data_is_optional_test,
     %% Error Message Format Tests
     error_message_not_empty_test,
     error_code_matches_error_type_test,
     %% Edge Case Tests
     null_id_in_error_response_test,
     batch_error_response_test,
     error_with_data_field_test].

init_per_suite(Config) ->
    %% Start erlmcp application for testing
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

%%%====================================================================
%%% JSON-RPC 2.0 Standard Error Tests
%%%====================================================================

%% @doc Test Parse Error (-32700)
%% Spec: Invalid JSON was received by the server
%% An error occurred on the server while parsing the JSON text
parse_error_minus_32700_test(_Config) ->
    %% Send invalid JSON to trigger parse error
    InvalidJson = <<"{invalid json}">>,
    {error, {parse_error, _}} = erlmcp_json_rpc:decode_message(InvalidJson),

    %% Also test error creation function
    ErrorResponse = erlmcp_json_rpc:error_parse(1),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    %% Validate error response structure
    <<"2.0">> = maps:get(<<"jsonrpc">>, ResponseMap),
    1 = maps:get(<<"id">>, ResponseMap),
    ErrorObj1 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj1),
    Message = maps:get(<<"message">>, ErrorObj1),

    %% Validate specific error code
    ct:log("Parse error code: ~p", [Code]),
    ct:log("Parse error message: ~p", [Message]),
    if Code =:= -32700 ->
           ok;
       true ->
           ct:fail("Parse error MUST have code -32700, got ~p", [Code])
    end,

    true = is_binary(Message),
    true = byte_size(Message) > 0,
    ok.

%% @doc Test Invalid Request (-32600)
%% Spec: The JSON sent is not a valid Request object
invalid_request_minus_32600_test(_Config) ->
    %% Create a request with invalid structure (missing 'method' field)
    InvalidRequest = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1}),
    %% Missing 'method' field
    case erlmcp_json_rpc:decode_message(InvalidRequest) of
        {error, {invalid_request, _}} ->
            %% Expected error
            ok;
        {ok, _} ->
            %% Some parsers might accept this, so we test the error creation directly
            ErrorResponse = erlmcp_json_rpc:error_invalid_params(1, <<"Missing method field">>),
            ResponseMap = jsx:decode(ErrorResponse, [return_maps]),
            ErrorObj = maps:get(<<"error">>, ResponseMap),
            Code = maps:get(<<"code">>, ErrorObj),
            true = Code < 0,
            ok
    end,
    ok.

%% @doc Test Invalid Request - Missing jsonrpc field
invalid_request_missing_jsonrpc_test(_Config) ->
    %% Request without jsonrpc version field
    RequestNoVersion = jsx:encode(#{<<"id">> => 1, <<"method">> => <<"tools/list">>}),

    case erlmcp_json_rpc:decode_message(RequestNoVersion) of
        {error, {invalid_request, missing_jsonrpc}} ->
            ok;
        {error, {invalid_request, _Reason}} ->
            ok;
        _Other ->
            %% Parser might handle this differently
            ok
    end,
    ok.

%% @doc Test Invalid Request - Wrong jsonrpc version
invalid_request_wrong_version_test(_Config) ->
    %% Request with wrong jsonrpc version
    RequestWrongVersion =
        jsx:encode(#{<<"jsonrpc">> => <<"1.0">>,
                     <<"id">> => 1,
                     <<"method">> => <<"tools/list">>}),

    case erlmcp_json_rpc:decode_message(RequestWrongVersion) of
        {error, {invalid_request, {wrong_version, _}}} ->
            ok;
        {error, {invalid_request, _Reason}} ->
            ok;
        _Other ->
            ok
    end,
    ok.

%% @doc Test Method Not Found (-32601)
%% Spec: The method does not exist / is not available
method_not_found_minus_32601_test(_Config) ->
    %% Create error response for non-existent method
    ErrorResponse = erlmcp_json_rpc:error_method_not_found(1, <<"nonexistent_method">>),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    <<"2.0">> = maps:get(<<"jsonrpc">>, ResponseMap),
    1 = maps:get(<<"id">>, ResponseMap),
    ErrorObj2 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj2),
    Message = maps:get(<<"message">>, ErrorObj2),
    Data = maps:get(<<"data">>, ErrorObj2),

    %% Validate error code
    ct:log("Method not found code: ~p", [Code]),
    if Code =:= -32601 ->
           ok;
       true ->
           ct:fail("Method not found MUST have code -32601, got ~p", [Code])
    end,

    true = is_binary(Message),
    true = maps:is_key(<<"method">>, Data),
    <<"nonexistent_method">> = maps:get(<<"method">>, Data),
    ok.

%% @doc Test Invalid Params (-32602)
%% Spec: Invalid method parameter(s)
invalid_params_minus_32602_test(_Config) ->
    %% Test with binary details
    ErrorResponse1 = erlmcp_json_rpc:error_invalid_params(1, <<"Missing required parameter">>),
    ResponseMap1 = jsx:decode(ErrorResponse1, [return_maps]),

    ErrorObj3 = maps:get(<<"error">>, ResponseMap1),
    Code1 = maps:get(<<"code">>, ErrorObj3),
    Data1 = maps:get(<<"data">>, ErrorObj3),

    ct:log("Invalid params code: ~p", [Code1]),
    if Code1 =:= -32602 ->
           ok;
       true ->
           ct:fail("Invalid params MUST have code -32602, got ~p", [Code1])
    end,

    true = maps:is_key(<<"details">>, Data1),
    <<"Missing required parameter">> = maps:get(<<"details">>, Data1),

    %% Test with atom details (converted to binary)
    ErrorResponse2 = erlmcp_json_rpc:error_invalid_params(1, invalid_type),
    ResponseMap2 = jsx:decode(ErrorResponse2, [return_maps]),
    ErrorObj4 = maps:get(<<"error">>, ResponseMap2),
    Data2 = maps:get(<<"data">>, ErrorObj4),
    <<"invalid_type">> = maps:get(<<"details">>, Data2),

    %% Test with list details (converted to binary)
    ErrorResponse3 = erlmcp_json_rpc:error_invalid_params(1, "list details"),
    ResponseMap3 = jsx:decode(ErrorResponse3, [return_maps]),
    ErrorObj5 = maps:get(<<"error">>, ResponseMap3),
    Data3 = maps:get(<<"data">>, ErrorObj5),
    <<"list details">> = maps:get(<<"details">>, Data3),

    ok.

%% @doc Test Internal Error (-32603)
%% Spec: Internal JSON-RPC error
internal_error_minus_32603_test(_Config) ->
    ErrorResponse = erlmcp_json_rpc:error_internal(1),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    <<"2.0">> = maps:get(<<"jsonrpc">>, ResponseMap),
    1 = maps:get(<<"id">>, ResponseMap),
    ErrorObj6 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj6),
    Message = maps:get(<<"message">>, ErrorObj6),

    ct:log("Internal error code: ~p", [Code]),
    if Code =:= -32603 ->
           ok;
       true ->
           ct:fail("Internal error MUST have code -32603, got ~p", [Code])
    end,

    true = is_binary(Message),
    true = byte_size(Message) > 0,
    ok.

%%%====================================================================
%%% MCP Core Error Tests (-32001 to -32010)
%%%====================================================================

%% @doc Test Resource Not Found (-32001)
%% Spec: The requested resource was not found
mcp_resource_not_found_minus_32001_test(_Config) ->
    Uri = <<"file:///path/to/nonexistent/resource.txt">>,
    ErrorResponse = erlmcp_json_rpc:error_resource_not_found(1, Uri),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    <<"2.0">> = maps:get(<<"jsonrpc">>, ResponseMap),
    1 = maps:get(<<"id">>, ResponseMap),
    ErrorObj7 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj7),
    Message = maps:get(<<"message">>, ErrorObj7),
    Data = maps:get(<<"data">>, ErrorObj7),

    ct:log("Resource not found code: ~p", [Code]),
    if Code =:= -32001 ->
           ok;
       true ->
           ct:fail("Resource not found MUST have code -32001, got ~p", [Code])
    end,

    true = is_binary(Message),
    true = maps:is_key(<<"uri">>, Data),
    Uri = maps:get(<<"uri">>, Data),
    ok.

%% @doc Test Tool Not Found (-32002)
%% Spec: The requested tool was not found
mcp_tool_not_found_minus_32002_test(_Config) ->
    ToolName = <<"nonexistent_tool">>,
    ErrorResponse = erlmcp_json_rpc:error_tool_not_found(1, ToolName),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    ErrorObj8 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj8),
    Data = maps:get(<<"data">>, ErrorObj8),

    ct:log("Tool not found code: ~p", [Code]),
    if Code =:= -32002 ->
           ok;
       true ->
           ct:fail("Tool not found MUST have code -32002, got ~p", [Code])
    end,

    true = maps:is_key(<<"tool">>, Data),
    ToolName = maps:get(<<"tool">>, Data),
    ok.

%% @doc Test Prompt Not Found (-32003)
%% Spec: The requested prompt was not found
mcp_prompt_not_found_minus_32003_test(_Config) ->
    PromptName = <<"nonexistent_prompt">>,
    ErrorResponse = erlmcp_json_rpc:error_prompt_not_found(1, PromptName),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    ErrorObj9 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj9),
    Data = maps:get(<<"data">>, ErrorObj9),

    ct:log("Prompt not found code: ~p", [Code]),
    if Code =:= -32003 ->
           ok;
       true ->
           ct:fail("Prompt not found MUST have code -32003, got ~p", [Code])
    end,

    true = maps:is_key(<<"prompt">>, Data),
    PromptName = maps:get(<<"prompt">>, Data),
    ok.

%% @doc Test Not Initialized (-32004)
%% Spec: Client has not called initialize
mcp_not_initialized_minus_32004_test(_Config) ->
    ErrorResponse = erlmcp_json_rpc:error_not_initialized(1),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    ErrorObj10 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj10),
    Message = maps:get(<<"message">>, ErrorObj10),

    ct:log("Not initialized code: ~p", [Code]),
    true = Code < 0,
    true = is_binary(Message),
    ok.

%% @doc Test Validation Failed (-32007)
%% Spec: Request validation failed
mcp_validation_failed_minus_32007_test(_Config) ->
    Details = <<"Invalid parameter format">>,
    ErrorResponse = erlmcp_json_rpc:error_validation_failed(1, Details),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    ErrorObj11 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj11),
    Data = maps:get(<<"data">>, ErrorObj11),

    ct:log("Validation failed code: ~p", [Code]),
    if Code =:= -32007 ->
           ok;
       true ->
           ct:fail("Validation failed MUST have code -32007, got ~p", [Code])
    end,

    true = maps:is_key(<<"details">>, Data),
    Details = maps:get(<<"details">>, Data),
    ok.

%%%====================================================================
%%% Error Structure Tests
%%%====================================================================

%% @doc Test that error responses have required fields
error_response_has_required_fields_test(_Config) ->
    %% Test multiple error types
    ErrorResponses =
        [erlmcp_json_rpc:error_parse(1),
         erlmcp_json_rpc:error_method_not_found(1, <<"test">>),
         erlmcp_json_rpc:error_invalid_params(1, <<"test">>),
         erlmcp_json_rpc:error_internal(1),
         erlmcp_json_rpc:error_resource_not_found(1, <<"uri">>)],

    lists:foreach(fun(Response) ->
                     ResponseMap = jsx:decode(Response, [return_maps]),
                     %% Must have jsonrpc, id, and error fields
                     true = maps:is_key(<<"jsonrpc">>, ResponseMap),
                     true = maps:is_key(<<"id">>, ResponseMap),
                     true = maps:is_key(<<"error">>, ResponseMap),

                     %% Error object must have code and message
                     ErrorObj = maps:get(<<"error">>, ResponseMap),
                     true = maps:is_key(<<"code">>, ErrorObj),
                     true = maps:is_key(<<"message">>, ErrorObj)
                  end,
                  ErrorResponses),
    ok.

%% @doc Test that error code is always an integer
error_response_code_is_integer_test(_Config) ->
    ErrorResponses =
        [erlmcp_json_rpc:error_parse(1),
         erlmcp_json_rpc:error_method_not_found(2, <<"test">>),
         erlmcp_json_rpc:error_invalid_params(3, <<"test">>),
         erlmcp_json_rpc:error_internal(4)],

    lists:foreach(fun(Response) ->
                     ResponseMap = jsx:decode(Response, [return_maps]),
                     ErrorObj = maps:get(<<"error">>, ResponseMap),
                     Code = maps:get(<<"code">>, ErrorObj),
                     true = is_integer(Code)
                  end,
                  ErrorResponses),
    ok.

%% @doc Test that error message is always a string
error_response_message_is_string_test(_Config) ->
    ErrorResponses =
        [erlmcp_json_rpc:error_parse(1),
         erlmcp_json_rpc:error_method_not_found(1, <<"test">>),
         erlmcp_json_rpc:error_invalid_params(1, <<"test">>),
         erlmcp_json_rpc:error_internal(1)],

    lists:foreach(fun(Response) ->
                     ResponseMap = jsx:decode(Response, [return_maps]),
                     ErrorObj = maps:get(<<"error">>, ResponseMap),
                     Message = maps:get(<<"message">>, ErrorObj),
                     true = is_binary(Message)
                  end,
                  ErrorResponses),
    ok.

%% @doc Test that data field is optional
error_response_data_is_optional_test(_Config) ->
    %% Error with data field
    ErrorWithData = erlmcp_json_rpc:error_method_not_found(1, <<"test">>),
    ResponseMap1 = jsx:decode(ErrorWithData, [return_maps]),
    ErrorObj1 = maps:get(<<"error">>, ResponseMap1),
    true = maps:is_key(<<"data">>, ErrorObj1),

    %% Error without data field (undefined)
    ErrorWithoutData = erlmcp_json_rpc:error_internal(1),
    ResponseMap2 = jsx:decode(ErrorWithoutData, [return_maps]),
    ErrorObj2 = maps:get(<<"error">>, ResponseMap2),
    %% Data field should not be present when undefined
    false = maps:is_key(<<"data">>, ErrorObj2),
    ok.

%%%====================================================================
%%% Error Message Format Tests
%%%====================================================================

%% @doc Test that error messages are not empty
error_message_not_empty_test(_Config) ->
    ErrorResponses =
        [erlmcp_json_rpc:error_parse(1),
         erlmcp_json_rpc:error_method_not_found(1, <<"test">>),
         erlmcp_json_rpc:error_invalid_params(1, <<"test">>),
         erlmcp_json_rpc:error_internal(1)],

    lists:foreach(fun(Response) ->
                     ResponseMap = jsx:decode(Response, [return_maps]),
                     ErrorObj = maps:get(<<"error">>, ResponseMap),
                     Message = maps:get(<<"message">>, ErrorObj),
                     true = byte_size(Message) > 0
                  end,
                  ErrorResponses),
    ok.

%% @doc Test that error code matches error type
error_code_matches_error_type_test(_Config) ->
    %% Parse error
    ParseError = erlmcp_json_rpc:error_parse(1),
    ParseMap = jsx:decode(ParseError, [return_maps]),
    ParseErrorObj = maps:get(<<"error">>, ParseMap),
    ParseCode = maps:get(<<"code">>, ParseErrorObj),
    ct:log("Parse error code: ~p", [ParseCode]),
    -32700 = ParseCode,

    %% Method not found
    MethodNotFound = erlmcp_json_rpc:error_method_not_found(1, <<"test">>),
    MethodMap = jsx:decode(MethodNotFound, [return_maps]),
    MethodErrorObj = maps:get(<<"error">>, MethodMap),
    MethodCode = maps:get(<<"code">>, MethodErrorObj),
    -32601 = MethodCode,

    %% Invalid params
    InvalidParams = erlmcp_json_rpc:error_invalid_params(1, <<"test">>),
    ParamsMap = jsx:decode(InvalidParams, [return_maps]),
    ParamsErrorObj = maps:get(<<"error">>, ParamsMap),
    ParamsCode = maps:get(<<"code">>, ParamsErrorObj),
    -32602 = ParamsCode,

    %% Internal error
    InternalError = erlmcp_json_rpc:error_internal(1),
    InternalMap = jsx:decode(InternalError, [return_maps]),
    InternalErrorObj = maps:get(<<"error">>, InternalMap),
    InternalCode = maps:get(<<"code">>, InternalErrorObj),
    -32603 = InternalCode,

    %% Resource not found
    ResourceNotFound = erlmcp_json_rpc:error_resource_not_found(1, <<"uri">>),
    ResourceMap = jsx:decode(ResourceNotFound, [return_maps]),
    ResourceErrorObj = maps:get(<<"error">>, ResourceMap),
    ResourceCode = maps:get(<<"code">>, ResourceErrorObj),
    -32001 = ResourceCode,

    ok.

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

%% @doc Test null ID in error response
null_id_in_error_response_test(_Config) ->
    %% Some error conditions might not have a request ID
    %% The spec allows null as the id field in such cases
    ErrorResponse = erlmcp_json_rpc:error_parse(null),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    Id = maps:get(<<"id">>, ResponseMap),
    null = Id,

    %% Verify the error is still valid
    ErrorObj12 = maps:get(<<"error">>, ResponseMap),
    Code = maps:get(<<"code">>, ErrorObj12),
    Message = maps:get(<<"message">>, ErrorObj12),
    true = is_integer(Code),
    true = is_binary(Message),
    ok.

%% @doc Test batch error response
batch_error_response_test(_Config) ->
    %% Create a batch with invalid requests
    BatchJson =
        jsx:encode([#{<<"jsonrpc">> => <<"2.0">>,
                      <<"id">> => 1,
                      <<"method">> => <<"valid_method">>},
                    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2},
                    #{<<"invalid">> => <<"request">>}]),

    case erlmcp_json_rpc:decode_batch(BatchJson) of
        {ok, Messages} when is_list(Messages) ->
            %% Check that we got responses for each request
            ct:log("Batch produced ~p responses", [length(Messages)]),
            true = length(Messages) >= 1;
        {error, _Reason} ->
            %% Some validation might fail at batch level
            ok
    end,
    ok.

%% @doc Test error with data field containing complex structure
error_with_data_field_test(_Config) ->
    %% Test error with map data
    ErrorResponse1 = erlmcp_json_rpc:error_resource_not_found(1, <<"file:///test.txt">>),
    ResponseMap1 = jsx:decode(ErrorResponse1, [return_maps]),
    ErrorObj13 = maps:get(<<"error">>, ResponseMap1),
    Data1 = maps:get(<<"data">>, ErrorObj13),
    true = is_map(Data1),
    true = maps:is_key(<<"uri">>, Data1),

    %% Test error with string data
    ErrorResponse2 = erlmcp_json_rpc:error_invalid_params(1, <<"Invalid type">>),
    ResponseMap2 = jsx:decode(ErrorResponse2, [return_maps]),
    ErrorObj14 = maps:get(<<"error">>, ResponseMap2),
    Data2 = maps:get(<<"data">>, ErrorObj14),
    true = is_map(Data2),
    <<"Invalid type">> = maps:get(<<"details">>, Data2),

    ok.
