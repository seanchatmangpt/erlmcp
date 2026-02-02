%% @doc Comprehensive coverage tests for erlmcp_json_rpc
%% Tests all public API functions to achieve 80%+ coverage
%% Chicago School TDD: State-based verification, no mocks
-module(erlmcp_json_rpc_coverage_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Encoding Tests
%%%====================================================================

encode_request_test() ->
    Id = 1,
    Method = <<"test/method">>,
    Params = #{<<"arg1">> => <<"value1">>},

    Json = erlmcp_json_rpc:encode_request(Id, Method, Params),

    % Verify JSON structure
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(Params, maps:get(<<"params">>, Decoded)).

encode_response_test() ->
    Id = 1,
    Result = #{<<"data">> => <<"test">>},

    Json = erlmcp_json_rpc:encode_response(Id, Result),

    % Verify JSON structure
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)),
    ?assertEqual(undefined, maps:get(<<"error">>, Decoded, undefined)).

encode_error_response_test() ->
    Id = 1,
    Code = -32600,
    Message = <<"Invalid request">>,

    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message),

    % Verify JSON structure
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)).

encode_error_response_with_data_test() ->
    Id = 1,
    Code = -32602,
    Message = <<"Invalid params">>,
    Data = #{<<"details">> => <<"Missing required field">>},

    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),

    % Verify JSON structure
    Decoded = jsx:decode(Json, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

encode_notification_test() ->
    Method = <<"test/notification">>,
    Params = #{<<"value">> => 42},

    Json = erlmcp_json_rpc:encode_notification(Method, Params),

    % Verify JSON structure
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(Params, maps:get(<<"params">>, Decoded)),
    ?assertEqual(undefined, maps:get(<<"id">>, Decoded, undefined)).

%%%====================================================================
%%% Decoding Tests
%%%====================================================================

decode_request_test() ->
    Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                       <<"id">> => 1,
                       <<"method">> => <<"test/method">>,
                       <<"params">> => #{}}),

    {ok, Request} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{}, Request),
    ?assertEqual(1, Request#json_rpc_request.id),
    ?assertEqual(<<"test/method">>, Request#json_rpc_request.method).

decode_response_test() ->
    Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                       <<"id">> => 1,
                       <<"result">> => #{<<"data">> => <<"test">>}}),

    {ok, Response} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{}, Response),
    ?assertEqual(1, Response#json_rpc_response.id),
    ?assertEqual(#{<<"data">> => <<"test">>}, Response#json_rpc_response.result).

decode_notification_test() ->
    Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                       <<"method">> => <<"test/notification">>,
                       <<"params">> => #{}}),

    {ok, Notification} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{}, Notification),
    ?assertEqual(<<"test/notification">>, Notification#json_rpc_notification.method).

decode_invalid_json_test() ->
    InvalidJson = <<"not a valid json">>,

    {error, {parse_error, _}} = erlmcp_json_rpc:decode_message(InvalidJson).

decode_missing_jsonrpc_field_test() ->
    Json = jsx:encode(#{<<"id">> => 1,
                       <<"method">> => <<"test">>,

                       <<"params">> => #{}}),

    {error, _} = erlmcp_json_rpc:decode_message(Json).

decode_batch_request_test() ->
    Batch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>, <<"params">> => #{}},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"m2">>, <<"params">> => #{}}
    ],
    Json = jsx:encode(Batch),

    {error, {batch_request, 2}} = erlmcp_json_rpc:decode_message(Json),
    % Should use decode_batch instead
    {ok, Requests} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(2, length(Requests)).

%%%====================================================================
%%% Batch Operations Tests
%%%====================================================================

is_batch_request_test() ->
    BatchJson = jsx:encode([#{<<"jsonrpc">> => <<"2.0">>}]),
    SingleJson = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>}),

    ?assertEqual(true, erlmcp_json_rpc:is_batch_request(BatchJson)),
    ?assertEqual(false, erlmcp_json_rpc:is_batch_request(SingleJson)).

encode_batch_test() ->
    Messages = [
        #json_rpc_request{id = 1, method = <<"m1">>, params = #{}},
        #json_rpc_request{id = 2, method = <<"m2">>, params = #{}}
    ],

    Json = erlmcp_json_rpc:encode_batch(Messages),
    Decoded = jsx:decode(Json, [return_maps]),

    ?assertEqual(true, is_list(Decoded)),
    ?assertEqual(2, length(Decoded)).

decode_batch_valid_test() ->
    Batch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>, <<"params">> => #{}},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"m2">>, <<"params">> => #{}}
    ],
    Json = jsx:encode(Batch),

    {ok, Requests} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(2, length(Requests)),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"m1">>},
                  #json_rpc_request{id = 2, method = <<"m2">>}], Requests).

decode_batch_empty_test() ->
    Json = jsx:encode([]),

    {error, {invalid_request, empty_batch}} = erlmcp_json_rpc:decode_batch(Json).

decode_batch_invalid_version_test() ->
    Batch = [
        #{<<"jsonrpc">> => <<"1.0">>, <<"id">> => 1, <<"method">> => <<"m1">>, <<"params">> => #{}}
    ],
    Json = jsx:encode(Batch),

    {error, {invalid_request, _}} = erlmcp_json_rpc:decode_batch(Json).

%%%====================================================================
%%% Error Helper Functions Tests
%%%====================================================================

error_method_not_found_test() ->
    Id = 1,
    Method = <<"unknown/method">>,

    Json = erlmcp_json_rpc:error_method_not_found(Id, Method),
    Decoded = jsx:decode(Json, [return_maps]),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"method">> => Method}, maps:get(<<"data">>, Error)).

error_invalid_params_test() ->
    Id = 1,
    Details = <<"Missing required field: name">>,

    Json = erlmcp_json_rpc:error_invalid_params(Id, Details),
    Decoded = jsx:decode(Json, [return_maps]),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid params">>, maps:get(<<"message">>, Error)).

error_resource_not_found_test() ->
    Id = 1,
    Uri = <<"file://missing.txt">>,

    Json = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    Decoded = jsx:decode(Json, [return_maps]),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32001, maps:get(<<"code">>, Error)),
    ?assertEqual(#{<<"uri">> => Uri}, maps:get(<<"data">>, Error)).

error_tool_not_found_test() ->
    Id = 1,
    ToolName = <<"unknown_tool">>,

    Json = erlmcp_json_rpc:error_tool_not_found(Id, ToolName),
    Decoded = jsx:decode(Json, [return_maps]),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32002, maps:get(<<"code">>, Error)),
    ?assertEqual(#{<<"tool">> => ToolName}, maps:get(<<"data">>, Error)).

error_internal_test() ->
    Id = 1,
    Json = erlmcp_json_rpc:error_internal(Id),
    Decoded = jsx:decode(Json, [return_maps]),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Internal error">>, maps:get(<<"message">>, Error)).

%%%====================================================================
%%% Error Code Validation Tests
%%%====================================================================

validate_error_code_test() ->
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32700)),
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32600)),
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32601)),
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32602)),
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32603)),
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32001)),
    ?assertEqual(true, erlmcp_json_rpc:validate_error_code(-32002)),
    ?assertEqual(false, erlmcp_json_rpc:validate_error_code(9999)),
    ?assertEqual(false, erlmcp_json_rpc:validate_error_code(0)).

%%%====================================================================
%%% Error Classification Tests
%%%====================================================================

error_severity_test() ->
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32700)),
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32600)),
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32603)),
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32601)),
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32602)),
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32001)),
    ?assertEqual(warning, erlmcp_json_rpc:error_severity(-32081)),
    ?assertEqual(info, erlmcp_json_rpc:error_severity(9999)).

error_category_test() ->
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32700)),
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32600)),
    ?assertEqual(mcp_core, erlmcp_json_rpc:error_category(-32001)),
    ?assertEqual(content, erlmcp_json_rpc:error_category(-32011)),
    ?assertEqual(resource, erlmcp_json_rpc:error_category(-32021)),
    ?assertEqual(tool, erlmcp_json_rpc:error_category(-32031)),
    ?assertEqual(prompt, erlmcp_json_rpc:error_category(-32041)),
    ?assertEqual(auth, erlmcp_json_rpc:error_category(-32051)),
    ?assertEqual(protocol, erlmcp_json_rpc:error_category(-32061)),
    ?assertEqual(pagination, erlmcp_json_rpc:error_category(-32071)),
    ?assertEqual(task, erlmcp_json_rpc:error_category(-32081)),
    ?assertEqual(progress, erlmcp_json_rpc:error_category(-32091)),
    ?assertEqual(completion, erlmcp_json_rpc:error_category(-32110)),
    ?assertEqual(unknown, erlmcp_json_rpc:error_category(9999)).

is_jsonrpc_standard_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_jsonrpc_standard_error(-32700)),
    ?assertEqual(true, erlmcp_json_rpc:is_jsonrpc_standard_error(-32600)),
    ?assertEqual(false, erlmcp_json_rpc:is_jsonrpc_standard_error(-32001)),
    ?assertEqual(false, erlmcp_json_rpc:is_jsonrpc_standard_error(0)).

is_mcp_core_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_core_error(-32001)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_core_error(-32010)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_core_error(-32000)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_core_error(-32011)).

is_mcp_content_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_content_error(-32011)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_content_error(-32020)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_content_error(-32021)).

is_mcp_resource_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_resource_error(-32021)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_resource_error(-32030)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_resource_error(-32031)).

is_mcp_tool_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_tool_error(-32031)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_tool_error(-32040)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_tool_error(-32041)).

is_mcp_prompt_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_prompt_error(-32041)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_prompt_error(-32050)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_prompt_error(-32051)).

is_mcp_auth_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_auth_error(-32051)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_auth_error(-32060)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_auth_error(-32061)).

is_mcp_protocol_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_protocol_error(-32061)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_protocol_error(-32070)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_protocol_error(-32071)).

is_mcp_pagination_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_pagination_error(-32071)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_pagination_error(-32080)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_pagination_error(-32081)).

is_mcp_task_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_task_error(-32081)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_task_error(-32090)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_task_error(-32091)).

is_mcp_progress_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_progress_error(-32091)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_progress_error(-32100)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_progress_error(-32110)).

is_mcp_completion_error_test() ->
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_completion_error(-32110)),
    ?assertEqual(true, erlmcp_json_rpc:is_mcp_completion_error(-32113)),
    ?assertEqual(false, erlmcp_json_rpc:is_mcp_completion_error(-32081)).

%%%====================================================================
%%% Error Object Creation Tests
%%%====================================================================

create_error_test() ->
    Code = -32602,
    Message = <<"Invalid params">>,
    Data = #{<<"details">> => <<"test">>},

    Error = erlmcp_json_rpc:create_error(Code, Message, Data),
    ?assertMatch(#mcp_error{}, Error),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assertEqual(Data, Error#mcp_error.data).

create_error_with_data_test() ->
    Code = -32001,
    Message = <<"Resource not found">>,
    DataKey = uri,
    DataValue = <<"file://test">>,

    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, DataKey, DataValue),
    ?assertMatch(#mcp_error{}, Error),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(#{<<"uri">> => DataValue}, Error#mcp_error.data).

%%%====================================================================
%%% Experimental Error Codes Tests
%%%====================================================================

error_elicitation_failed_test() ->
    Id = 1,
    Reason = <<"User cancelled">>,
    Json = erlmcp_json_rpc:error_elicitation_failed(Id, Reason),
    Decoded = jsx:decode(Json, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1090, maps:get(<<"code">>, Error)),
    ?assertEqual(#{<<"reason">> => Reason}, maps:get(<<"data">>, Error)).

error_elicitation_timeout_test() ->
    Id = 1,
    TimeoutMs = 5000,
    Json = erlmcp_json_rpc:error_elicitation_timeout(Id, TimeoutMs),
    Decoded = jsx:decode(Json, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1091, maps:get(<<"code">>, Error)),
    ?assertEqual(#{<<"timeoutMs">> => TimeoutMs}, maps:get(<<"data">>, Error)).

error_experimental_task_not_found_test() ->
    Id = 1,
    TaskId = <<"task-123">>,
    Json = erlmcp_json_rpc:error_experimental_task_not_found(Id, TaskId),
    Decoded = jsx:decode(Json, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1095, maps:get(<<"code">>, Error)),
    ?assertEqual(#{<<"taskId">> => TaskId}, maps:get(<<"data">>, Error)).

error_task_dependency_failed_test() ->
    Id = 1,
    TaskId = <<"task-1">>,
    DependencyId = <<"task-2">>,
    Json = erlmcp_json_rpc:error_task_dependency_failed(Id, TaskId, DependencyId),
    Decoded = jsx:decode(Json, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1096, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)),
    ?assertEqual(DependencyId, maps:get(<<"dependencyId">>, Data)).

%%%====================================================================
%%% Batch Error Response Tests
%%%====================================================================

create_batch_error_response_test() ->
    Request = #{<<"id">> => 1, <<"method">> => <<"test">>},
    Reason = invalid_request,
    Details = not_an_object,

    ErrorResponse = erlmcp_json_rpc:create_batch_error_response(Request, Reason, Details),
    ?assertMatch(#json_rpc_response{id = 1, error = _}, ErrorResponse).

create_batch_error_response_no_id_test() ->
    Request = #{<<"method">> => <<"test">>},
    Reason = invalid_request,
    Details = missing_jsonrpc,

    ErrorResponse = erlmcp_json_rpc:create_batch_error_response(Request, Reason, Details),
    ?assertMatch(#json_rpc_response{id = null, error = _}, ErrorResponse).
