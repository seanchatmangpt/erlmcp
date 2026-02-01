-module(erlmcp_spec_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Suite: erlmcp_spec_parser
%%% Description: Tests for hardcoded MCP 2025-11-25 specification parser
%%% Testing Methodology: Chicago School TDD - Real calls, NO mocks
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% Test Groups
%%%-----------------------------------------------------------------------------

%% Test group: Method Specification Tests
method_spec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get tools/create method spec", fun test_get_tools_create_spec/0},
      {"Get tools/list method spec", fun test_get_tools_list_spec/0},
      {"Get tools/call method spec", fun test_get_tools_call_spec/0},
      {"Get prompts/create method spec", fun test_get_prompts_create_spec/0},
      {"Get prompts/list method spec", fun test_get_prompts_list_spec/0},
      {"Get prompts/get method spec", fun test_get_prompts_get_spec/0},
      {"Get resources/list method spec", fun test_get_resources_list_spec/0},
      {"Get resources/read method spec", fun test_get_resources_read_spec/0},
      {"Get tasks/create method spec", fun test_get_tasks_create_spec/0},
      {"Get tasks/list method spec", fun test_get_tasks_list_spec/0},
      {"Get tasks/get method spec", fun test_get_tasks_get_spec/0},
      {"Get tasks/cancel method spec", fun test_get_tasks_cancel_spec/0},
      {"Get tasks/result method spec", fun test_get_tasks_result_spec/0},
      {"Get unknown method returns undefined", fun test_get_unknown_method_spec/0}]}.

%% Test group: Method Validation Tests
method_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Validate tools/create method", fun test_is_valid_method_tools_create/0},
      {"Validate tools/list method", fun test_is_valid_method_tools_list/0},
      {"Validate invalid method", fun test_is_valid_method_invalid/0},
      {"Validate empty method name", fun test_is_valid_method_empty/0}]}.

%% Test group: Error Code Tests
error_code_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get parse error code", fun test_get_parse_error_code/0},
      {"Get invalid request error code", fun test_get_invalid_request_code/0},
      {"Get method not found error code", fun test_get_method_not_found_code/0},
      {"Get resource not found error code", fun test_get_resource_not_found_code/0},
      {"Get tool not found error code", fun test_get_tool_not_found_code/0},
      {"Get invalid error code returns undefined", fun test_get_invalid_error_code/0}]}.

%% Test group: Error Code Validation Tests
error_code_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Validate JSON-RPC parse error", fun test_is_valid_error_code_parse/0},
      {"Validate JSON-RPC method not found", fun test_is_valid_error_code_method_not_found/0},
      {"Validate MCP resource not found", fun test_is_valid_error_code_resource_not_found/0},
      {"Validate MCP tool not found", fun test_is_valid_error_code_tool_not_found/0},
      {"Validate invalid error code", fun test_is_valid_error_code_invalid/0}]}.

%% Test group: List All Methods Tests
list_methods_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get all methods returns list", fun test_get_all_methods/0},
      {"Get all methods count", fun test_get_all_methods_count/0},
      {"Get all methods contains tools/create", fun test_get_all_methods_contains_tools_create/0}]}.

%% Test group: List All Error Codes Tests
list_error_codes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get all error codes returns list", fun test_get_all_error_codes/0},
      {"Get all error codes count", fun test_get_all_error_codes_count/0},
      {"Get all error codes contains -32700",
       fun test_get_all_error_codes_contains_parse_error/0}]}.

%% Test group: Required Parameters Tests
required_params_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get required params for tools/create", fun test_get_required_params_tools_create/0},
      {"Get required params for tools/list", fun test_get_required_params_tools_list/0},
      {"Get required params for tools/call", fun test_get_required_params_tools_call/0},
      {"Get required params for unknown method returns empty",
       fun test_get_required_params_unknown/0}]}.

%% Test group: State Transitions Tests
state_transitions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get state transitions returns list", fun test_get_state_transitions/0},
      {"Validate pending to processing transition",
       fun test_state_transition_pending_to_processing/0},
      {"Validate processing to completed transition",
       fun test_state_transition_processing_to_completed/0},
      {"Validate processing to failed transition",
       fun test_state_transition_processing_to_failed/0},
      {"Validate pending to cancelled transition",
       fun test_state_transition_pending_to_cancelled/0},
      {"Validate invalid completed transition", fun test_state_transition_completed_invalid/0},
      {"Validate invalid failed transition", fun test_state_transition_failed_invalid/0}]}.

%% Test group: Method Spec Structure Tests
method_spec_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Method spec contains name field", fun test_method_spec_has_name/0},
      {"Method spec contains description field", fun test_method_spec_has_description/0},
      {"Method spec contains params field", fun test_method_spec_has_params/0},
      {"Method spec contains returns field", fun test_method_spec_has_returns/0}]}.

%% Test group: Error Code Structure Tests
error_code_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Error code contains code field", fun test_error_code_has_code/0},
      {"Error code contains message field", fun test_error_code_has_message/0},
      {"Error code contains category field", fun test_error_code_has_category/0}]}.

%% Test group: Edge Cases Tests
edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Get method spec with undefined", fun test_get_method_spec_undefined/0},
      {"Get error code with zero", fun test_get_error_code_zero/0},
      {"Validate method with binary", fun test_is_valid_method_binary/0},
      {"Validate method with atom", fun test_is_valid_method_atom/0},
      {"Get required params for method with no params", fun test_get_required_params_no_params/0}]}.

%%%-----------------------------------------------------------------------------
%%% Setup and Cleanup Functions
%%%-----------------------------------------------------------------------------

setup() ->
    %% No setup needed - using real erlmcp_spec_parser module
    ok.

cleanup(_State) ->
    %% No cleanup needed
    ok.

%%%-----------------------------------------------------------------------------
%%% Test Functions: Method Specifications
%%%-----------------------------------------------------------------------------

test_get_tools_create_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/create">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tools/create">>, maps:get(<<"name">>, Spec)).

test_get_tools_list_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tools/list">>, maps:get(<<"name">>, Spec)).

test_get_tools_call_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/call">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tools/call">>, maps:get(<<"name">>, Spec)).

test_get_prompts_create_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"prompts/create">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"prompts/create">>, maps:get(<<"name">>, Spec)).

test_get_prompts_list_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"prompts/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"prompts/list">>, maps:get(<<"name">>, Spec)).

test_get_prompts_get_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"prompts/get">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"prompts/get">>, maps:get(<<"name">>, Spec)).

test_get_resources_list_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"resources/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"resources/list">>, maps:get(<<"name">>, Spec)).

test_get_resources_read_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"resources/read">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"resources/read">>, maps:get(<<"name">>, Spec)).

test_get_tasks_create_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tasks/create">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tasks/create">>, maps:get(<<"name">>, Spec)).

test_get_tasks_list_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tasks/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tasks/list">>, maps:get(<<"name">>, Spec)).

test_get_tasks_get_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tasks/get">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tasks/get">>, maps:get(<<"name">>, Spec)).

test_get_tasks_cancel_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tasks/cancel">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tasks/cancel">>, maps:get(<<"name">>, Spec)).

test_get_tasks_result_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tasks/result">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(<<"tasks/result">>, maps:get(<<"name">>, Spec)).

test_get_unknown_method_spec() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"unknown/method">>),
    ?assertEqual(undefined, Spec).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Method Validation
%%%-----------------------------------------------------------------------------

test_is_valid_method_tools_create() ->
    ?assert(erlmcp_spec_parser:is_valid_method(<<"tools/create">>)).

test_is_valid_method_tools_list() ->
    ?assert(erlmcp_spec_parser:is_valid_method(<<"tools/list">>)).

test_is_valid_method_invalid() ->
    ?assertNot(erlmcp_spec_parser:is_valid_method(<<"invalid/method">>)).

test_is_valid_method_empty() ->
    ?assertNot(erlmcp_spec_parser:is_valid_method(<<>>)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Error Codes
%%%-----------------------------------------------------------------------------

test_get_parse_error_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32700),
    ?assertNotEqual(undefined, ErrorCode),
    ?assertEqual(-32700, maps:get(<<"code">>, ErrorCode)),
    ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, ErrorCode)).

test_get_invalid_request_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32600),
    ?assertNotEqual(undefined, ErrorCode),
    ?assertEqual(-32600, maps:get(<<"code">>, ErrorCode)),
    ?assertEqual(<<"Invalid Request">>, maps:get(<<"message">>, ErrorCode)).

test_get_method_not_found_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32601),
    ?assertNotEqual(undefined, ErrorCode),
    ?assertEqual(-32601, maps:get(<<"code">>, ErrorCode)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, ErrorCode)).

test_get_resource_not_found_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32001),
    ?assertNotEqual(undefined, ErrorCode),
    ?assertEqual(-32001, maps:get(<<"code">>, ErrorCode)).

test_get_tool_not_found_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32002),
    ?assertNotEqual(undefined, ErrorCode),
    ?assertEqual(-32002, maps:get(<<"code">>, ErrorCode)).

test_get_invalid_error_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-99999),
    ?assertEqual(undefined, ErrorCode).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Error Code Validation
%%%-----------------------------------------------------------------------------

test_is_valid_error_code_parse() ->
    ?assert(erlmcp_spec_parser:is_valid_error_code(-32700)).

test_is_valid_error_code_method_not_found() ->
    ?assert(erlmcp_spec_parser:is_valid_error_code(-32601)).

test_is_valid_error_code_resource_not_found() ->
    ?assert(erlmcp_spec_parser:is_valid_error_code(-32001)).

test_is_valid_error_code_tool_not_found() ->
    ?assert(erlmcp_spec_parser:is_valid_error_code(-32002)).

test_is_valid_error_code_invalid() ->
    ?assertNot(erlmcp_spec_parser:is_valid_error_code(-99999)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: List All Methods
%%%-----------------------------------------------------------------------------

test_get_all_methods() ->
    Methods = erlmcp_spec_parser:get_all_methods(),
    ?assert(is_list(Methods)),
    ?assert(length(Methods) > 0).

test_get_all_methods_count() ->
    Methods = erlmcp_spec_parser:get_all_methods(),
    ?assertEqual(13, length(Methods)).

test_get_all_methods_contains_tools_create() ->
    Methods = erlmcp_spec_parser:get_all_methods(),
    ?assert(lists:member(<<"tools/create">>, Methods)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: List All Error Codes
%%%-----------------------------------------------------------------------------

test_get_all_error_codes() ->
    ErrorCodes = erlmcp_spec_parser:get_all_error_codes(),
    ?assert(is_list(ErrorCodes)),
    ?assert(length(ErrorCodes) > 0).

test_get_all_error_codes_count() ->
    ErrorCodes = erlmcp_spec_parser:get_all_error_codes(),
    %% JSON-RPC standard errors (5) + MCP core errors (10) + other ranges
    ?assert(length(ErrorCodes) >= 100).

test_get_all_error_codes_contains_parse_error() ->
    ErrorCodes = erlmcp_spec_parser:get_all_error_codes(),
    ?assert(lists:member(-32700, ErrorCodes)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Required Parameters
%%%-----------------------------------------------------------------------------

test_get_required_params_tools_create() ->
    Params = erlmcp_spec_parser:get_required_params(<<"tools/create">>),
    ?assert(is_list(Params)),
    ?assert(lists:member(<<"name">>, Params)),
    ?assert(lists:member(<<"description">>, Params)).

test_get_required_params_tools_list() ->
    Params = erlmcp_spec_parser:get_required_params(<<"tools/list">>),
    ?assert(is_list(Params)),
    %% tools/list has no required params
    ?assertEqual([], Params).

test_get_required_params_tools_call() ->
    Params = erlmcp_spec_parser:get_required_params(<<"tools/call">>),
    ?assert(is_list(Params)),
    ?assert(lists:member(<<"name">>, Params)),
    ?assert(lists:member(<<"arguments">>, Params)).

test_get_required_params_unknown() ->
    Params = erlmcp_spec_parser:get_required_params(<<"unknown/method">>),
    ?assertEqual([], Params).

%%%-----------------------------------------------------------------------------
%%% Test Functions: State Transitions
%%%-----------------------------------------------------------------------------

test_get_state_transitions() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assert(is_list(Transitions)),
    ?assert(length(Transitions) > 0).

test_state_transition_pending_to_processing() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assert(lists:member({pending, processing}, Transitions)).

test_state_transition_processing_to_completed() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assert(lists:member({processing, completed}, Transitions)).

test_state_transition_processing_to_failed() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assert(lists:member({processing, failed}, Transitions)).

test_state_transition_pending_to_cancelled() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assert(lists:member({pending, cancelled}, Transitions)).

test_state_transition_completed_invalid() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assertNot(lists:member({completed, processing}, Transitions)).

test_state_transition_failed_invalid() ->
    Transitions = erlmcp_spec_parser:get_state_transitions(),
    ?assertNot(lists:member({failed, pending}, Transitions)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Method Spec Structure
%%%-----------------------------------------------------------------------------

test_method_spec_has_name() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/create">>),
    ?assert(maps:is_key(<<"name">>, Spec)).

test_method_spec_has_description() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/create">>),
    ?assert(maps:is_key(<<"description">>, Spec)).

test_method_spec_has_params() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/create">>),
    ?assert(maps:is_key(<<"params">>, Spec)).

test_method_spec_has_returns() ->
    Spec = erlmcp_spec_parser:get_method_spec(<<"tools/create">>),
    ?assert(maps:is_key(<<"returns">>, Spec)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Error Code Structure
%%%-----------------------------------------------------------------------------

test_error_code_has_code() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32700),
    ?assert(maps:is_key(<<"code">>, ErrorCode)).

test_error_code_has_message() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32700),
    ?assert(maps:is_key(<<"message">>, ErrorCode)).

test_error_code_has_category() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(-32700),
    ?assert(maps:is_key(<<"category">>, ErrorCode)).

%%%-----------------------------------------------------------------------------
%%% Test Functions: Edge Cases
%%%-----------------------------------------------------------------------------

test_get_method_spec_undefined() ->
    Spec = erlmcp_spec_parser:get_method_spec(undefined),
    ?assertEqual(undefined, Spec).

test_get_error_code_zero() ->
    ErrorCode = erlmcp_spec_parser:get_error_code(0),
    ?assertEqual(undefined, ErrorCode).

test_is_valid_method_binary() ->
    ?assert(erlmcp_spec_parser:is_valid_method(<<"tools/create">>)).

test_is_valid_method_atom() ->
    ?assertNot(erlmcp_spec_parser:is_valid_method('tools/create')).

test_get_required_params_no_params() ->
    Params = erlmcp_spec_parser:get_required_params(<<"resources/list">>),
    ?assert(is_list(Params)),
    ?assertEqual([], Params).
