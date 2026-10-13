%%%-------------------------------------------------------------------
%%% @doc erlmcp_errors_tests - Error Handling Tests
%%%
%%% Tests the error formatting and code mapping system:
%%% - Real error responses (no mocks)
%%% - Standard error codes (JSON-RPC and MCP)
%%% - Refusal error detection (1001-1089)
%%% - Error message formatting
%%% - State-based verification (output format)
%%%
%%% Uses Chicago School TDD:
%%% - Verify observable error responses
%%% - Test error code ranges and types
%%% - Validate refusal error detection
%%%
%%% Target: 90%+ coverage (core infrastructure, security-critical)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_errors_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

errors_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_errors_start/1,
      fun test_format_error_simple/1,
      fun test_format_error_with_data/1,
      fun test_is_refusal_error_true/1,
      fun test_is_refusal_error_false/1,
      fun test_is_refusal_error_boundaries/1,
      fun test_refusal_reason_all_codes/1,
      fun test_refusal_reason_unknown/1,
      fun test_error_code_parse_error/1,
      fun test_error_code_invalid_request/1,
      fun test_error_code_method_not_found/1,
      fun test_error_code_invalid_params/1,
      fun test_error_code_internal_error/1,
      fun test_error_code_resource_not_found/1,
      fun test_error_code_tool_not_found/1,
      fun test_error_code_not_initialized/1,
      fun test_error_code_validation_failed/1,
      fun test_error_code_rate_limited/1,
      fun test_error_code_unauthorized/1,
      fun test_error_code_unknown/1,
      fun test_error_message_all_codes/1,
      fun test_error_message_unknown/1,
      fun test_gen_server_handle_call_unknown/1,
      fun test_gen_server_handle_cast/1,
      fun test_gen_server_handle_info/1,
      fun test_gen_server_terminate/1,
      fun test_gen_server_code_change/1]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start error manager
    {ok, Pid} = erlmcp_errors:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop error manager
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_errors);
        false ->
            ok
    end.

%%====================================================================
%% Basic API Tests
%%====================================================================

test_errors_start(Pid) ->
    %% Verify: Server started
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

test_format_error_simple(_Pid) ->
    %% Exercise: Format simple error
    Result = erlmcp_errors:format_error({parse_error, <<"Invalid JSON">>}),

    %% Verify: Returns proper error structure
    ?assert(is_map(Result)),
    ?assert(maps:is_key(<<"code">>, Result)),
    ?assert(maps:is_key(<<"message">>, Result)),
    ?assert(maps:is_key(<<"data">>, Result)),

    ?assertEqual(-32700, maps:get(<<"code">>, Result)),
    ?assertEqual(<<"Invalid JSON">>, maps:get(<<"message">>, Result)),
    ?assertEqual(#{}, maps:get(<<"data">>, Result)).

test_format_error_with_data(_Pid) ->
    %% Exercise: Format error with additional data
    Data = #{<<"field">> => <<"value">>, <<"details">> => [1, 2, 3]},
    Result = erlmcp_errors:format_error({invalid_params, <<"Validation failed">>, Data}),

    %% Verify: Data preserved in error
    ?assertEqual(-32602, maps:get(<<"code">>, Result)),
    ?assertEqual(<<"Validation failed">>, maps:get(<<"message">>, Result)),
    ?assertEqual(Data, maps:get(<<"data">>, Result)).

%%====================================================================
%% Refusal Error Tests
%%====================================================================

test_is_refusal_error_true(_Pid) ->
    %% Exercise: Test valid refusal error codes
    RefusalCodes = lists:seq(1001, 1089),

    lists:foreach(fun(Code) ->
        Result = erlmcp_errors:is_refusal_error(Code),
        ?assert(Result, io_lib:format("Code ~p should be refusal error", [Code]))
    end, RefusalCodes).

test_is_refusal_error_false(_Pid) ->
    %% Exercise: Test non-refusal error codes
    NonRefusalCodes = [
        -32700, % parse_error
        -32600, % invalid_request
        -32601, % method_not_found
        -32602, % invalid_params
        -32603, % internal_error
        -32001, % resource_not_found
        -32002, % tool_not_found
        -32005, % not_initialized
        -32007, % validation_failed
        -32010, % rate_limited
        -32051, % unauthorized
        1000,   % Below refusal range
        1090    % Above refusal range
    ],

    lists:foreach(fun(Code) ->
        Result = erlmcp_errors:is_refusal_error(Code),
        ?assertNot(Result, io_lib:format("Code ~p should NOT be refusal error", [Code]))
    end, NonRefusalCodes).

test_is_refusal_error_boundaries(_Pid) ->
    %% Exercise: Test boundary conditions
    ?assert(erlmcp_errors:is_refusal_error(1001)), % Lower bound
    ?assert(erlmcp_errors:is_refusal_error(1089)), % Upper bound
    ?assertNot(erlmcp_errors:is_refusal_error(1000)), % Below
    ?assertNot(erlmcp_errors:is_refusal_error(1090)). % Above

test_refusal_reason_all_codes(_Pid) ->
    %% Exercise: Test all refusal reason mappings
    RefusalReasons = [
        {1001, <<"Content violates policy">>},
        {1002, <<"Violates safety guidelines">>},
        {1003, <<"Rate limit exceeded">>},
        {1004, <<"Resource constraints">>},
        {1005, <<"Permission denied">>},
        {1006, <<"Invalid input">>},
        {1007, <<"Unsupported operation">>},
        {1008, <<"Temporarily unavailable">>},
        {1009, <<"Dependency failed">>},
        {1010, <<"Custom refusal">>}
    ],

    lists:foreach(fun({Code, ExpectedReason}) ->
        Result = erlmcp_errors:refusal_reason(Code),
        ?assertEqual(ExpectedReason, Result)
    end, RefusalReasons).

test_refusal_reason_unknown(_Pid) ->
    %% Exercise: Test refusal reason for unknown codes in range
    %% Any code 1001-1089 not explicitly mapped returns "Tool refused"
    UnknownCode = 1050,
    Result = erlmcp_errors:refusal_reason(UnknownCode),

    ?assertEqual(<<"Tool refused">>, Result).

%%====================================================================
%% Error Code Tests
%%====================================================================

test_error_code_parse_error(_Pid) ->
    ?assertEqual(-32700, erlmcp_errors:error_code(parse_error)).

test_error_code_invalid_request(_Pid) ->
    ?assertEqual(-32600, erlmcp_errors:error_code(invalid_request)).

test_error_code_method_not_found(_Pid) ->
    ?assertEqual(-32601, erlmcp_errors:error_code(method_not_found)).

test_error_code_invalid_params(_Pid) ->
    ?assertEqual(-32602, erlmcp_errors:error_code(invalid_params)).

test_error_code_internal_error(_Pid) ->
    ?assertEqual(-32603, erlmcp_errors:error_code(internal_error)).

test_error_code_resource_not_found(_Pid) ->
    ?assertEqual(-32001, erlmcp_errors:error_code(resource_not_found)).

test_error_code_tool_not_found(_Pid) ->
    ?assertEqual(-32002, erlmcp_errors:error_code(tool_not_found)).

test_error_code_not_initialized(_Pid) ->
    ?assertEqual(-32005, erlmcp_errors:error_code(not_initialized)).

test_error_code_validation_failed(_Pid) ->
    ?assertEqual(-32007, erlmcp_errors:error_code(validation_failed)).

test_error_code_rate_limited(_Pid) ->
    ?assertEqual(-32010, erlmcp_errors:error_code(rate_limited)).

test_error_code_unauthorized(_Pid) ->
    ?assertEqual(-32051, erlmcp_errors:error_code(unauthorized)).

test_error_code_unknown(_Pid) ->
    ?assertEqual(-32603, erlmcp_errors:error_code(unknown_atom)).

%%====================================================================
%% Error Message Tests
%%====================================================================

test_error_message_all_codes(_Pid) ->
    %% Exercise: Test all error messages
    ErrorMessages = [
        {parse_error, <<"Parse error">>},
        {invalid_request, <<"Invalid request">>},
        {method_not_found, <<"Method not found">>},
        {invalid_params, <<"Invalid params">>},
        {internal_error, <<"Internal error">>},
        {resource_not_found, <<"Resource not found">>},
        {tool_not_found, <<"Tool not found">>},
        {not_initialized, <<"Not initialized">>},
        {validation_failed, <<"Validation failed">>},
        {rate_limited, <<"Rate limited">>},
        {unauthorized, <<"Unauthorized">>}
    ],

    lists:foreach(fun({ErrorCode, ExpectedMessage}) ->
        Result = erlmcp_errors:error_message(ErrorCode),
        ?assertEqual(ExpectedMessage, Result)
    end, ErrorMessages).

test_error_message_unknown(_Pid) ->
    ?assertEqual(<<"Unknown error">>, erlmcp_errors:error_message(unknown_atom)).

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

test_gen_server_handle_call_unknown(_Pid) ->
    %% Exercise: Unknown call
    Result = gen_server:call(erlmcp_errors, unknown_request),

    %% Verify: Returns error
    ?assertEqual({error, unknown_request}, Result).

test_gen_server_handle_cast(_Pid) ->
    %% Exercise: Cast
    Result = gen_server:cast(erlmcp_errors, test_cast),

    %% Verify: Doesn't crash
    ?assertEqual(ok, Result).

test_gen_server_handle_info(_Pid) ->
    %% Exercise: Info message
    Pid ! test_info,
    timer:sleep(100),

    %% Verify: Still alive
    ?assert(is_process_alive(Pid)).

test_gen_server_terminate(_Pid) ->
    %% Exercise: Stop
    ok = gen_server:stop(erlmcp_errors),

    %% Verify: Stopped
    ?assertNot(is_process_alive(Pid)).

test_gen_server_code_change(_Pid) ->
    %% Exercise: Code change
    {ok, State} = sys:get_state(erlmcp_errors),
    Result = erlmcp_errors:code_change("", State, ""),

    %% Verify: Succeeds
    ?assertMatch({ok, _}, Result).
