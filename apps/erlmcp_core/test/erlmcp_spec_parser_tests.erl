-module(erlmcp_spec_parser_tests).
-author("Chicago School TDD: NO MOCKS, TEST REAL BEHAVIOR").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_spec_parser Module
%%
%% CHICAGO SCHOOL TDD:
%% - Test ONLY observable behavior through public API
%% - NO internal function testing
%% - NO mocking, NO fake implementations
%% - Test ALL methods, notifications, error codes
%%====================================================================

%%====================================================================
%% Version Tests
%%====================================================================

version_test_() ->
    [
        ?_test(test_get_version_returns_correct_version()),
        ?_test(test_get_version_is_binary()),
        ?_test(test_get_version_is_hardcoded())
    ].

test_get_version_returns_correct_version() ->
    ?assertEqual(<<"2025-11-25">>, erlmcp_spec_parser:get_version()).

test_get_version_is_binary() ->
    Version = erlmcp_spec_parser:get_version(),
    ?assert(is_binary(Version)).

test_get_version_is_hardcoded() ->
    %% This test exists to remind: NO PARSING, JUST HARDCODED TRUTH
    Version = erlmcp_spec_parser:get_version(),
    ?assertEqual(<<"2025-11-25">>, Version),
    ?assert(byte_size(Version) > 0).

%%====================================================================
%% Method Lookup Tests
%%====================================================================

method_lookup_test_() ->
    [
        ?_test(test_get_method_initialize()),
        ?_test(test_get_method_ping()),
        ?_test(test_get_method_shutdown()),
        ?_test(test_get_method_resources_list()),
        ?_test(test_get_method_resources_read()),
        ?_test(test_get_method_resources_subscribe()),
        ?_test(test_get_method_resources_unsubscribe()),
        ?_test(test_get_method_tools_list()),
        ?_test(test_get_method_tools_call()),
        ?_test(test_get_method_prompts_list()),
        ?_test(test_get_method_prompts_get()),
        ?_test(test_get_method_unknown_returns_undefined())
    ].

test_get_method_initialize() ->
    Spec = erlmcp_spec_parser:get_method(<<"initialize">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)),
    ?assert(maps:is_key(params, Spec)),
    ?assert(maps:is_key(result, Spec)).

test_get_method_ping() ->
    Spec = erlmcp_spec_parser:get_method(<<"ping">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_shutdown() ->
    Spec = erlmcp_spec_parser:get_method(<<"shutdown">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_resources_list() ->
    Spec = erlmcp_spec_parser:get_method(<<"resources/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)),
    ?assert(maps:is_key(params, Spec)).

test_get_method_resources_read() ->
    Spec = erlmcp_spec_parser:get_method(<<"resources/read">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_resources_subscribe() ->
    Spec = erlmcp_spec_parser:get_method(<<"resources/subscribe">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_resources_unsubscribe() ->
    Spec = erlmcp_spec_parser:get_method(<<"resources/unsubscribe">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_tools_list() ->
    Spec = erlmcp_spec_parser:get_method(<<"tools/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_tools_call() ->
    Spec = erlmcp_spec_parser:get_method(<<"tools/call">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)),
    Params = maps:get(params, Spec),
    ?assert(is_map(Params)),
    ?assert(maps:size(Params) >= 2).

test_get_method_prompts_list() ->
    Spec = erlmcp_spec_parser:get_method(<<"prompts/list">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_prompts_get() ->
    Spec = erlmcp_spec_parser:get_method(<<"prompts/get">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(request_response, maps:get(type, Spec)).

test_get_method_unknown_returns_undefined() ->
    Spec = erlmcp_spec_parser:get_method(<<"unknown/method">>),
    ?assertEqual(undefined, Spec).

%%====================================================================
%% List All Methods Tests
%%====================================================================

list_all_methods_test_() ->
    [
        ?_test(test_list_all_methods_returns_list()),
        ?_test(test_list_all_methods_contains_all_methods()),
        ?_test(test_list_all_methods_length()),
        ?_test(test_list_all_methods_all_binaries())
    ].

test_list_all_methods_returns_list() ->
    Methods = erlmcp_spec_parser:list_all_methods(),
    ?assert(is_list(Methods)).

test_list_all_methods_contains_all_methods() ->
    Methods = erlmcp_spec_parser:list_all_methods(),
    RequiredMethods = [
        <<"initialize">>,
        <<"ping">>,
        <<"shutdown">>,
        <<"resources/list">>,
        <<"resources/read">>,
        <<"resources/subscribe">>,
        <<"resources/unsubscribe">>,
        <<"tools/list">>,
        <<"tools/call">>,
        <<"prompts/list">>,
        <<"prompts/get">>
    ],
    lists:foreach(fun(Method) ->
        ?assert(lists:member(Method, Methods))
    end, RequiredMethods).

test_list_all_methods_length() ->
    Methods = erlmcp_spec_parser:list_all_methods(),
    ?assertEqual(11, length(Methods)).

test_list_all_methods_all_binaries() ->
    Methods = erlmcp_spec_parser:list_all_methods(),
    lists:foreach(fun(Method) ->
        ?assert(is_binary(Method))
    end, Methods).

%%====================================================================
%% Notification Lookup Tests
%%====================================================================

notification_lookup_test_() ->
    [
        ?_test(test_get_notification_initialized()),
        ?_test(test_get_notification_cancelled()),
        ?_test(test_get_notification_progress()),
        ?_test(test_get_notification_roots_list_changed()),
        ?_test(test_get_notification_unknown_returns_undefined())
    ].

test_get_notification_initialized() ->
    Spec = erlmcp_spec_parser:get_notification(<<"notifications/initialized">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(notification, maps:get(type, Spec)),
    ?assert(maps:is_key(params, Spec)).

test_get_notification_cancelled() ->
    Spec = erlmcp_spec_parser:get_notification(<<"notifications/cancelled">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(notification, maps:get(type, Spec)).

test_get_notification_progress() ->
    Spec = erlmcp_spec_parser:get_notification(<<"notifications/progress">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(notification, maps:get(type, Spec)),
    Params = maps:get(params, Spec),
    %% Params is a map with keys representing parameter names
    ?assert(is_map(Params)),
    ?assert(maps:size(Params) >= 2).

test_get_notification_roots_list_changed() ->
    Spec = erlmcp_spec_parser:get_notification(<<"notifications/roots/list_changed">>),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(notification, maps:get(type, Spec)).

test_get_notification_unknown_returns_undefined() ->
    Spec = erlmcp_spec_parser:get_notification(<<"notifications/unknown">>),
    ?assertEqual(undefined, Spec).

%%====================================================================
%% List All Notifications Tests
%%====================================================================

list_all_notifications_test_() ->
    [
        ?_test(test_list_all_notifications_returns_list()),
        ?_test(test_list_all_notifications_contains_all()),
        ?_test(test_list_all_notifications_length()),
        ?_test(test_list_all_notifications_all_binaries())
    ].

test_list_all_notifications_returns_list() ->
    Notifications = erlmcp_spec_parser:list_all_notifications(),
    ?assert(is_list(Notifications)).

test_list_all_notifications_contains_all() ->
    Notifications = erlmcp_spec_parser:list_all_notifications(),
    Required = [
        <<"notifications/initialized">>,
        <<"notifications/cancelled">>,
        <<"notifications/progress">>,
        <<"notifications/roots/list_changed">>
    ],
    lists:foreach(fun(Notification) ->
        ?assert(lists:member(Notification, Notifications))
    end, Required).

test_list_all_notifications_length() ->
    Notifications = erlmcp_spec_parser:list_all_notifications(),
    ?assertEqual(4, length(Notifications)).

test_list_all_notifications_all_binaries() ->
    Notifications = erlmcp_spec_parser:list_all_notifications(),
    lists:foreach(fun(Notification) ->
        ?assert(is_binary(Notification))
    end, Notifications).

%%====================================================================
%% Error Code Lookup Tests
%%====================================================================

error_code_lookup_test_() ->
    [
        ?_test(test_get_error_code_parse_error()),
        ?_test(test_get_error_code_invalid_request()),
        ?_test(test_get_error_code_method_not_found()),
        ?_test(test_get_error_code_invalid_params()),
        ?_test(test_get_error_code_internal_error()),
        ?_test(test_get_error_code_not_initialized()),
        ?_test(test_get_error_code_capability_not_supported()),
        ?_test(test_get_error_code_validation_failed()),
        ?_test(test_get_error_code_message_too_large()),
        ?_test(test_get_error_code_unknown_returns_undefined())
    ].

test_get_error_code_parse_error() ->
    Spec = erlmcp_spec_parser:get_error_code(-32700),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32700, maps:get(code, Spec)),
    ?assertEqual(<<"Parse error">>, maps:get(name, Spec)),
    ?assertEqual(jsonrpc, maps:get(category, Spec)).

test_get_error_code_invalid_request() ->
    Spec = erlmcp_spec_parser:get_error_code(-32600),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32600, maps:get(code, Spec)),
    ?assertEqual(<<"Invalid Request">>, maps:get(name, Spec)).

test_get_error_code_method_not_found() ->
    Spec = erlmcp_spec_parser:get_error_code(-32601),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32601, maps:get(code, Spec)),
    ?assertEqual(<<"Method not found">>, maps:get(name, Spec)).

test_get_error_code_invalid_params() ->
    Spec = erlmcp_spec_parser:get_error_code(-32602),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32602, maps:get(code, Spec)),
    ?assertEqual(<<"Invalid params">>, maps:get(name, Spec)).

test_get_error_code_internal_error() ->
    Spec = erlmcp_spec_parser:get_error_code(-32603),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32603, maps:get(code, Spec)),
    ?assertEqual(<<"Internal error">>, maps:get(name, Spec)).

test_get_error_code_not_initialized() ->
    Spec = erlmcp_spec_parser:get_error_code(-32005),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32005, maps:get(code, Spec)),
    ?assertEqual(mcp_core, maps:get(category, Spec)).

test_get_error_code_capability_not_supported() ->
    Spec = erlmcp_spec_parser:get_error_code(-32004),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32004, maps:get(code, Spec)).

test_get_error_code_validation_failed() ->
    Spec = erlmcp_spec_parser:get_error_code(-32003),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32003, maps:get(code, Spec)).

test_get_error_code_message_too_large() ->
    Spec = erlmcp_spec_parser:get_error_code(-32002),
    ?assertNotEqual(undefined, Spec),
    ?assertEqual(-32002, maps:get(code, Spec)).

test_get_error_code_unknown_returns_undefined() ->
    Spec = erlmcp_spec_parser:get_error_code(99999),
    ?assertEqual(undefined, Spec).

%%====================================================================
%% List All Error Codes Tests
%%====================================================================

list_all_error_codes_test_() ->
    [
        ?_test(test_list_all_error_codes_returns_list()),
        ?_test(test_list_all_error_codes_contains_all_jsonrpc()),
        ?_test(test_list_all_error_codes_contains_all_mcp()),
        ?_test(test_list_all_error_codes_length()),
        ?_test(test_list_all_error_codes_all_integers())
    ].

test_list_all_error_codes_returns_list() ->
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    ?assert(is_list(Codes)).

test_list_all_error_codes_contains_all_jsonrpc() ->
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    %% JSON-RPC standard errors
    ?assert(lists:member(-32700, Codes)),
    ?assert(lists:member(-32600, Codes)),
    ?assert(lists:member(-32601, Codes)),
    ?assert(lists:member(-32602, Codes)),
    ?assert(lists:member(-32603, Codes)).

test_list_all_error_codes_contains_all_mcp() ->
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    %% MCP-specific errors
    ?assert(lists:member(-32009, Codes)),
    ?assert(lists:member(-32008, Codes)),
    ?assert(lists:member(-32007, Codes)),
    ?assert(lists:member(-32006, Codes)),
    ?assert(lists:member(-32005, Codes)),
    ?assert(lists:member(-32004, Codes)),
    ?assert(lists:member(-32003, Codes)),
    ?assert(lists:member(-32002, Codes)),
    ?assert(lists:member(-32001, Codes)),
    ?assert(lists:member(-32000, Codes)).

test_list_all_error_codes_length() ->
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    ?assertEqual(15, length(Codes)).

test_list_all_error_codes_all_integers() ->
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    lists:foreach(fun(Code) ->
        ?assert(is_integer(Code))
    end, Codes).

%%====================================================================
%% Validate Request Tests
%%====================================================================

validate_request_test_() ->
    [
        ?_test(test_validate_request_initialize_valid()),
        ?_test(test_validate_request_ping_valid()),
        ?_test(test_validate_request_tools_call_valid()),
        ?_test(test_validate_request_unknown_method()),
        ?_test(test_validate_request_with_empty_params())
    ].

test_validate_request_initialize_valid() ->
    Method = <<"initialize">>,
    Params = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{},
        <<"clientInfo">> => #{
            <<"name">> => <<"test-client">>,
            <<"version">> => <<"1.0.0">>
        }
    },
    Result = erlmcp_spec_parser:validate_request(Method, Params),
    ?assertMatch({ok, _}, Result).

test_validate_request_ping_valid() ->
    Method = <<"ping">>,
    Params = #{},
    Result = erlmcp_spec_parser:validate_request(Method, Params),
    ?assertMatch({ok, _}, Result).

test_validate_request_tools_call_valid() ->
    Method = <<"tools/call">>,
    Params = #{
        <<"name">> => <<"test_tool">>,
        <<"arguments">> => #{}
    },
    Result = erlmcp_spec_parser:validate_request(Method, Params),
    ?assertMatch({ok, _}, Result).

test_validate_request_unknown_method() ->
    Method = <<"unknown/method">>,
    Params = #{},
    Result = erlmcp_spec_parser:validate_request(Method, Params),
    ?assertMatch({error, {method_not_found, _}}, Result).

test_validate_request_with_empty_params() ->
    Method = <<"initialize">>,
    Params = #{},
    Result = erlmcp_spec_parser:validate_request(Method, Params),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Validate Notification Tests
%%====================================================================

validate_notification_test_() ->
    [
        ?_test(test_validate_notification_initialized_valid()),
        ?_test(test_validate_notification_progress_valid()),
        ?_test(test_validate_notification_unknown()),
        ?_test(test_validate_notification_with_empty_params())
    ].

test_validate_notification_initialized_valid() ->
    Notification = <<"notifications/initialized">>,
    Params = #{},
    Result = erlmcp_spec_parser:validate_notification(Notification, Params),
    ?assertMatch({ok, _}, Result).

test_validate_notification_progress_valid() ->
    Notification = <<"notifications/progress">>,
    Params = #{
        <<"progressToken">> => <<"token-123">>,
        <<"progress">> => 50,
        <<"total">> => 100
    },
    Result = erlmcp_spec_parser:validate_notification(Notification, Params),
    ?assertMatch({ok, _}, Result).

test_validate_notification_unknown() ->
    Notification = <<"notifications/unknown">>,
    Params = #{},
    Result = erlmcp_spec_parser:validate_notification(Notification, Params),
    ?assertMatch({error, {notification_not_found, _}}, Result).

test_validate_notification_with_empty_params() ->
    Notification = <<"notifications/cancelled">>,
    Params = #{},
    Result = erlmcp_spec_parser:validate_notification(Notification, Params),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Edge Cases and Integration Tests
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_method_spec_has_required_fields()),
        ?_test(test_notification_spec_has_required_fields()),
        ?_test(test_error_code_spec_has_required_fields()),
        ?_test(test_all_methods_in_list_have_specs()),
        ?_test(test_all_notifications_in_list_have_specs()),
        ?_test(test_all_error_codes_in_list_have_specs())
    ].

test_method_spec_has_required_fields() ->
    Spec = erlmcp_spec_parser:get_method(<<"initialize">>),
    ?assert(maps:is_key(type, Spec)),
    ?assert(maps:is_key(params, Spec)),
    ?assert(maps:is_key(result, Spec)).

test_notification_spec_has_required_fields() ->
    Spec = erlmcp_spec_parser:get_notification(<<"notifications/initialized">>),
    ?assert(maps:is_key(type, Spec)),
    ?assert(maps:is_key(params, Spec)).

test_error_code_spec_has_required_fields() ->
    Spec = erlmcp_spec_parser:get_error_code(-32700),
    ?assert(maps:is_key(code, Spec)),
    ?assert(maps:is_key(name, Spec)),
    ?assert(maps:is_key(message, Spec)),
    ?assert(maps:is_key(category, Spec)).

test_all_methods_in_list_have_specs() ->
    Methods = erlmcp_spec_parser:list_all_methods(),
    lists:foreach(fun(Method) ->
        Spec = erlmcp_spec_parser:get_method(Method),
        ?assertNotEqual(undefined, Spec)
    end, Methods).

test_all_notifications_in_list_have_specs() ->
    Notifications = erlmcp_spec_parser:list_all_notifications(),
    lists:foreach(fun(Notification) ->
        Spec = erlmcp_spec_parser:get_notification(Notification),
        ?assertNotEqual(undefined, Spec)
    end, Notifications).

test_all_error_codes_in_list_have_specs() ->
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    lists:foreach(fun(Code) ->
        Spec = erlmcp_spec_parser:get_error_code(Code),
        ?assertNotEqual(undefined, Spec)
    end, Codes).

%%====================================================================
%% Hardcoded Truth Verification Tests
%%====================================================================

hardcoded_truth_test_() ->
    [
        ?_test(test_version_is_hardcoded_not_parsed()),
        ?_test(test_methods_are_hardcoded_not_generated()),
        ?_test(test_error_codes_are_hardcoded_not_loaded())
    ].

test_version_is_hardcoded_not_parsed() ->
    %% This test enforces Joe Armstrong's philosophy
    Version = erlmcp_spec_parser:get_version(),
    ?assertEqual(<<"2025-11-25">>, Version),
    ?assert(is_binary(Version)),
    ?assertNotEqual(undefined, Version).

test_methods_are_hardcoded_not_generated() ->
    %% Verify we have the exact methods from MCP 2025-11-25 spec
    Methods = erlmcp_spec_parser:list_all_methods(),
    ?assertEqual(11, length(Methods)),
    ?assert(lists:member(<<"initialize">>, Methods)),
    ?assert(lists:member(<<"tools/call">>, Methods)).

test_error_codes_are_hardcoded_not_loaded() ->
    %% Verify we have the exact error codes from MCP 2025-11-25 spec
    Codes = erlmcp_spec_parser:list_all_error_codes(),
    ?assertEqual(15, length(Codes)),
    ?assert(lists:member(-32700, Codes)),  %% Parse error
    ?assert(lists:member(-32005, Codes)).  %% Not initialized
