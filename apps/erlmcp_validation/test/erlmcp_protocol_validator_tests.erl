-module(erlmcp_protocol_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_spec_parser.hrl").
-include("erlmcp_protocol_validator.hrl").

%%====================================================================
%% Test Suite for erlmcp_protocol_validator Module
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_protocol_validator:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% JSON-RPC 2.0 Validation Tests
%%====================================================================

json_rpc_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(test_validate_jsonrpc_version_field()),
         ?_test(test_validate_request_structure()),
         ?_test(test_validate_response_structure()),
         ?_test(test_validate_notification_structure()),
         ?_test(test_validate_batch_requests()),
         ?_test(test_validate_id_correlation()),
         ?_test(test_validate_null_id_handling())
     ] end}.

test_validate_jsonrpc_version_field() ->
    %% Valid: jsonrpc = "2.0"
    ValidMsg = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>, <<"id">> => 1},
    {ok, true, _} = erlmcp_protocol_validator:validate_message(ValidMsg),

    %% Invalid: missing jsonrpc field
    InvalidMsg1 = #{<<"method">> => <<"test">>, <<"id">> => 1},
    {ok, false, _} = erlmcp_protocol_validator:validate_message(InvalidMsg1),

    %% Invalid: wrong version
    InvalidMsg2 = #{<<"jsonrpc">> => <<"1.0">>, <<"method">> => <<"test">>, <<"id">> => 1},
    {ok, false, _} = erlmcp_protocol_validator:validate_message(InvalidMsg2),

    ok.

test_validate_request_structure() ->
    %% Valid request with all fields
    ValidMsg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialize">>,
        <<"id">> => 1,
        <<"params">> => #{}
    },
    {ok, true, Result} = erlmcp_protocol_validator:validate_message(ValidMsg),
    ?assertEqual(request, maps:get(message, Result)),

    %% Invalid: missing method
    InvalidMsg = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
    {ok, false, _} = erlmcp_protocol_validator:validate_message(InvalidMsg),

    ok.

test_validate_response_structure() ->
    %% Valid response with result
    ValidResult = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"protocolVersion">> => <<"2025-11-25">>}
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_message(ValidResult),

    %% Valid response with error
    ValidError = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_message(ValidError),

    %% Invalid: missing both result and error
    InvalidMsg = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
    {ok, false, _} = erlmcp_protocol_validator:validate_message(InvalidMsg),

    ok.

test_validate_notification_structure() ->
    %% Valid notification (no id field)
    ValidNotif = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/progress">>,
        <<"params">> => #{<<"progressToken">> => <<"token1">>, <<"progress">> => 50}
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_message(ValidNotif),

    ok.

test_validate_batch_requests() ->
    %% Valid batch request
    ValidBatch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>, <<"id">> => 1},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>, <<"id">> => 2}
    ],
    {ok, true, _} = erlmcp_protocol_validator:validate_message(ValidBatch),

    %% Empty batch is invalid
    EmptyBatch = [],
    {ok, false, _} = erlmcp_protocol_validator:validate_message(EmptyBatch),

    ok.

test_validate_id_correlation() ->
    RequestId = 42,
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>, <<"id">> => RequestId},
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => RequestId, <<"result">> => #{}},

    {ok, true, ReqResult} = erlmcp_protocol_validator:validate_message(Request),
    {ok, true, RespResult} = erlmcp_protocol_validator:validate_message(Response),

    ?assertEqual(RequestId, maps:get(id, ReqResult)),
    ?assertEqual(RequestId, maps:get(id, RespResult)),

    ok.

test_validate_null_id_handling() ->
    %% Notification without id
    Notif1 = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"initialized">>},
    {ok, true, _} = erlmcp_protocol_validator:validate_message(Notif1),

    %% Notification with null id
    Notif2 = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"initialized">>, <<"id">> => null},
    {ok, true, _} = erlmcp_protocol_validator:validate_message(Notif2),

    ok.

%%====================================================================
%% Error Code Validation Tests
%%====================================================================

error_code_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(test_validate_jsonrpc_error_codes()),
         ?_test(test_validate_mcp_error_codes()),
         ?_test(test_validate_refusal_codes()),
         ?_test(test_validate_error_structure())
     ] end}.

test_validate_jsonrpc_error_codes() ->
    JSONRPCCodes = [-32700, -32600, -32601, -32602, -32603],

    lists:foreach(fun(Code) ->
        Error = #{<<"code">> => Code, <<"message">> => <<"test error">>},
        {ok, true, _} = erlmcp_protocol_validator:validate_error(Error)
    end, JSONRPCCodes),

    ok.

test_validate_mcp_error_codes() ->
    MCPCodes = [-32001, -32002, -32003, -32004, -32005, -32006, -32007, -32008, -32009, -32010],

    lists:foreach(fun(Code) ->
        Error = #{<<"code">> => Code, <<"message">> => <<"test error">>},
        {ok, true, _} = erlmcp_protocol_validator:validate_error(Error)
    end, MCPCodes),

    ok.

test_validate_refusal_codes() ->
    RefusalCodes = [1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1089],

    lists:foreach(fun(Code) ->
        Error = #{<<"code">> => Code, <<"message">> => <<"test error">>},
        {ok, true, _} = erlmcp_protocol_validator:validate_error(Error)
    end, RefusalCodes),

    ok.

test_validate_error_structure() ->
    %% Valid error with required fields
    ValidError = #{
        <<"code">> => -32600,
        <<"message">> => <<"Invalid Request">>
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_error(ValidError),

    %% Valid error with optional data field
    ValidErrorWithData = #{
        <<"code">> => -32008,
        <<"message">> => <<"Resource not found">>,
        <<"data">> => #{<<"uri">> => <<"file:///test">>}
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_error(ValidErrorWithData),

    %% Invalid: missing code
    InvalidError1 = #{<<"message">> => <<"Error">>},
    {ok, false, _} = erlmcp_protocol_validator:validate_error(InvalidError1),

    %% Invalid: missing message
    InvalidError2 = #{<<"code">> => -32600},
    {ok, false, _} = erlmcp_protocol_validator:validate_error(InvalidError2),

    ok.

%%====================================================================
%% Method Validation Tests
%%====================================================================

method_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(test_validate_tools_list_method()),
         ?_test(test_validate_tools_call_method()),
         ?_test(test_validate_resources_list_method()),
         ?_test(test_validate_resources_read_method()),
         ?_test(test_validate_prompts_list_method()),
         ?_test(test_validate_prompts_get_method()),
         ?_test(test_validate_ping_method()),
         ?_test(test_validate_progress_notification())
     ] end}.

test_validate_tools_list_method() ->
    Method = <<"tools/list">>,
    Params = #{},

    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, Params),

    ok.

test_validate_tools_call_method() ->
    Method = <<"tools/call">>,

    %% Valid call
    ValidParams = #{<<"name">> => <<"test_tool">>, <<"arguments">> => #{}},
    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, ValidParams),

    %% Missing required parameter
    InvalidParams = #{<<"name">> => <<"test_tool">>},
    {ok, false, _} = erlmcp_protocol_validator:validate_method(Method, InvalidParams),

    ok.

test_validate_resources_list_method() ->
    Method = <<"resources/list">>,
    Params = #{},

    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, Params),

    ok.

test_validate_resources_read_method() ->
    Method = <<"resources/read">>,

    %% Valid call
    ValidParams = #{<<"uri">> => <<"file:///test.txt">>},
    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, ValidParams),

    %% Missing required parameter
    InvalidParams = #{},
    {ok, false, _} = erlmcp_protocol_validator:validate_method(Method, InvalidParams),

    ok.

test_validate_prompts_list_method() ->
    Method = <<"prompts/list">>,
    Params = #{},

    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, Params),

    ok.

test_validate_prompts_get_method() ->
    Method = <<"prompts/get">>,

    %% Valid call
    ValidParams = #{<<"name">> => <<"test_prompt">>},
    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, ValidParams),

    %% Missing required parameter
    InvalidParams = #{},
    {ok, false, _} = erlmcp_protocol_validator:validate_method(Method, InvalidParams),

    ok.

test_validate_ping_method() ->
    Method = <<"ping">>,
    Params = #{},

    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, Params),

    ok.

test_validate_progress_notification() ->
    Method = <<"notifications/progress">>,

    %% Valid with required params
    ValidParams = #{
        <<"progressToken">> => <<"token123">>,
        <<"progress">> => 50.0
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, ValidParams),

    %% Valid with optional params
    ValidParamsWithOptional = #{
        <<"progressToken">> => <<"token123">>,
        <<"progress">> => 50.0,
        <<"total">> => 100.0,
        <<"message">> => <<"Processing">>
    },
    {ok, true, _} = erlmcp_protocol_validator:validate_method(Method, ValidParamsWithOptional),

    %% Missing required params
    InvalidParams = #{<<"progress">> => 50.0},
    {ok, false, _} = erlmcp_protocol_validator:validate_method(Method, InvalidParams),

    ok.

%%====================================================================
%% Report Generation Tests
%%====================================================================

report_generation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(test_get_validation_results()),
         ?_test(test_reset_validation_state())
     ] end}.

test_get_validation_results() ->
    {ok, Results} = erlmcp_protocol_validator:get_validation_results(),

    ?assert(is_list(Results)),

    ok.

test_reset_validation_state() ->
    %% Get current results
    {ok, Results1} = erlmcp_protocol_validator:get_validation_results(),

    %% Reset state
    ok = erlmcp_protocol_validator:reset_validation_state(),

    %% Get new results (should be empty)
    {ok, Results2} = erlmcp_protocol_validator:get_validation_results(),

    ?assertEqual(0, length(Results2)),

    ok.

%%====================================================================
%% Transport Configuration Tests
%%====================================================================

transport_configuration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(test_set_transport()),
         ?_test(test_get_transport())
     ] end}.

test_set_transport() ->
    ok = erlmcp_protocol_validator:set_transport(stdio),
    ok = erlmcp_protocol_validator:set_transport(sse),
    ok = erlmcp_protocol_validator:set_transport(websocket),
    ok = erlmcp_protocol_validator:set_transport(http),

    ok.

test_get_transport() ->
    erlmcp_protocol_validator:set_transport(stdio),
    {ok, stdio} = erlmcp_protocol_validator:get_transport(),

    erlmcp_protocol_validator:set_transport(sse),
    {ok, sse} = erlmcp_protocol_validator:get_transport(),

    ok.
