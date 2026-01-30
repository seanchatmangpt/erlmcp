%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol Validator Test Suite
%%%
%%% Tests all 19 validation checks implemented in erlmcp_protocol_validator
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% JSON-RPC Version Tests (1/19)
%%%===================================================================

jsonrpc_version_test_() ->
    {inorder, [
        {"Valid JSON-RPC 2.0 version", fun() ->
            Result = erlmcp_protocol_validator:check_jsonrpc_version(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(<<"2.0">>, maps:get(version, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% Request Format Tests (2/19)
%%%===================================================================

request_format_test_() ->
    {inorder, [
        {"Valid request with all required fields", fun() ->
            Result = erlmcp_protocol_validator:check_request_format(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Response Format Tests (3/19)
%%%===================================================================

response_format_test_() ->
    {inorder, [
        {"Valid success response", fun() ->
            Result = erlmcp_protocol_validator:check_response_format(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Notification Format Tests (4/19)
%%%===================================================================

notification_format_test_() ->
    {inorder, [
        {"Valid notification without id", fun() ->
            Result = erlmcp_protocol_validator:check_notification_format(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Batch Request Tests (5/19)
%%%===================================================================

batch_requests_test_() ->
    {inorder, [
        {"Valid batch request array", fun() ->
            Result = erlmcp_protocol_validator:check_batch_requests(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(2, maps:get(batch_size, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% Initialize Params Tests (6/19)
%%%===================================================================

initialize_params_test_() ->
    {inorder, [
        {"Valid initialize params", fun() ->
            Result = erlmcp_protocol_validator:check_initialize_params(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Initialize Response Tests (7/19)
%%%===================================================================

initialize_response_test_() ->
    {inorder, [
        {"Valid initialize response", fun() ->
            Result = erlmcp_protocol_validator:check_initialize_response(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Tools List Params Tests (8/19)
%%%===================================================================

tools_list_params_test_() ->
    {inorder, [
        {"Valid tools/list params", fun() ->
            Result = erlmcp_protocol_validator:check_tools_list_params(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Tools List Response Tests (9/19)
%%%===================================================================

tools_list_response_test_() ->
    {inorder, [
        {"Valid tools/list response", fun() ->
            Result = erlmcp_protocol_validator:check_tools_list_response(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(1, maps:get(tool_count, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% Resources List Params Tests (10/19)
%%%===================================================================

resources_list_params_test_() ->
    {inorder, [
        {"Valid resources/list params", fun() ->
            Result = erlmcp_protocol_validator:check_resources_list_params(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Resources List Response Tests (11/19)
%%%===================================================================

resources_list_response_test_() ->
    {inorder, [
        {"Valid resources/list response", fun() ->
            Result = erlmcp_protocol_validator:check_resources_list_response(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(1, maps:get(resource_count, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% Result Exclusivity Tests (12/19)
%%%===================================================================

result_exclusivity_test_() ->
    {inorder, [
        {"Result and error are mutually exclusive", fun() ->
            Result = erlmcp_protocol_validator:check_result_exclusivity(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Error Object Tests (13/19)
%%%===================================================================

error_object_test_() ->
    {inorder, [
        {"Valid error object with code and message", fun() ->
            Result = erlmcp_protocol_validator:check_error_object(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(-32600, maps:get(code, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% Response ID Tests (14/19)
%%%===================================================================

response_id_test_() ->
    {inorder, [
        {"Valid response ID types", fun() ->
            Result = erlmcp_protocol_validator:check_response_id(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Response JSON-RPC Tests (15/19)
%%%===================================================================

response_jsonrpc_test_() ->
    {inorder, [
        {"Response has jsonrpc version", fun() ->
            Result = erlmcp_protocol_validator:check_response_jsonrpc(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% MCP Refusal Codes Tests (16/19)
%%%===================================================================

mcp_refusal_codes_test_() ->
    {inorder, [
        {"MCP error codes in valid range", fun() ->
            Result = erlmcp_protocol_validator:check_mcp_refusal_codes(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(10, maps:get(count, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% JSON-RPC Error Codes Tests (17/19)
%%%===================================================================

jsonrpc_error_codes_test_() ->
    {inorder, [
        {"JSON-RPC error codes valid", fun() ->
            Result = erlmcp_protocol_validator:check_jsonrpc_error_codes(test_module),
            ?assertEqual(passed, maps:get(status, Result)),
            ?assertEqual(5, maps:get(count, maps:get(details, Result)))
        end}
    ]}.

%%%===================================================================
%%% Custom Error Codes Tests (18/19)
%%%===================================================================

custom_error_codes_test_() ->
    {inorder, [
        {"Custom error codes handled properly", fun() ->
            Result = erlmcp_protocol_validator:check_custom_error_codes(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Protocol Version Tests (19/19)
%%%===================================================================

protocol_version_2025_11_25_test_() ->
    {inorder, [
        {"MCP 2025-11-25 version supported", fun() ->
            Result = erlmcp_protocol_validator:check_protocol_version_2025_11_25(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

version_compatibility_test_() ->
    {inorder, [
        {"Version compatibility check", fun() ->
            Result = erlmcp_protocol_validator:check_version_compatibility(test_module),
            ?assertEqual(passed, maps:get(status, Result))
        end}
    ]}.

%%%===================================================================
%%% Summary Report
%%%===================================================================

validation_summary_test_() ->
    {"All 19 validations implemented", fun() ->
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_jsonrpc_version(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_request_format(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_response_format(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_notification_format(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_batch_requests(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_initialize_params(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_initialize_response(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_tools_list_params(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_tools_list_response(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_resources_list_params(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_resources_list_response(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_result_exclusivity(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_error_object(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_response_id(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_response_jsonrpc(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_mcp_refusal_codes(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_jsonrpc_error_codes(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_custom_error_codes(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_protocol_version_2025_11_25(test))),
        ?assert(passed =:= maps:get(status, erlmcp_protocol_validator:check_version_compatibility(test)))
    end}.
