-module(erlmcp_logging_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_logging Module (Gap #21 - Log Level Enforcement)
%%====================================================================

%%====================================================================
%% Basic Level Validation Tests
%%====================================================================

validate_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_debug_level()),
             ?_test(test_validate_info_level()),
             ?_test(test_validate_warning_level()),
             ?_test(test_validate_error_level()),
             ?_test(test_validate_critical_level()),
             ?_test(test_validate_invalid_level()),
             ?_test(test_validate_unknown_level())
         ]
     end}.

test_validate_debug_level() ->
    Result = erlmcp_logging:validate_log_level(debug),
    ?assertMatch({ok, debug}, Result).

test_validate_info_level() ->
    Result = erlmcp_logging:validate_log_level(info),
    ?assertMatch({ok, info}, Result).

test_validate_warning_level() ->
    Result = erlmcp_logging:validate_log_level(warning),
    ?assertMatch({ok, warning}, Result).

test_validate_error_level() ->
    Result = erlmcp_logging:validate_log_level(error),
    ?assertMatch({ok, error}, Result).

test_validate_critical_level() ->
    Result = erlmcp_logging:validate_log_level(critical),
    ?assertMatch({ok, critical}, Result).

test_validate_invalid_level() ->
    Result = erlmcp_logging:validate_log_level(invalid),
    ?assertMatch({error, invalid_level}, Result).

test_validate_unknown_level() ->
    Result = erlmcp_logging:validate_log_level(unknown),
    ?assertMatch({error, invalid_level}, Result).

%%====================================================================
%% Level Normalization Tests
%%====================================================================

normalize_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_normalize_atom_level()),
             ?_test(test_normalize_binary_level()),
             ?_test(test_normalize_string_level()),
             ?_test(test_normalize_invalid_binary()),
             ?_test(test_normalize_invalid_string())
         ]
     end}.

test_normalize_atom_level() ->
    Result = erlmcp_logging:normalize_log_level(debug),
    ?assertMatch({ok, debug}, Result).

test_normalize_binary_level() ->
    Result = erlmcp_logging:normalize_log_level(<<"warning">>),
    ?assertMatch({ok, warning}, Result).

test_normalize_string_level() ->
    Result = erlmcp_logging:normalize_log_level("error"),
    ?assertMatch({ok, error}, Result).

test_normalize_invalid_binary() ->
    Result = erlmcp_logging:normalize_log_level(<<"invalid">>),
    ?assertMatch({error, invalid_level}, Result).

test_normalize_invalid_string() ->
    Result = erlmcp_logging:normalize_log_level("trace"),
    ?assertMatch({error, invalid_level}, Result).

%%====================================================================
%% Global Log Level Tests
%%====================================================================

global_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_get_default_level()),
             ?_test(test_set_debug_level()),
             ?_test(test_set_info_level()),
             ?_test(test_set_warning_level()),
             ?_test(test_set_error_level()),
             ?_test(test_set_critical_level()),
             ?_test(test_set_invalid_level_error())
         ]
     end}.

test_get_default_level() ->
    application:unset_env(erlmcp, global_log_level),
    Result = erlmcp_logging:get_global_level(),
    ?assertMatch({ok, info}, Result).

test_set_debug_level() ->
    ok = erlmcp_logging:set_global_level(debug),
    {ok, Level} = erlmcp_logging:get_global_level(),
    ?assertEqual(debug, Level).

test_set_info_level() ->
    ok = erlmcp_logging:set_global_level(info),
    {ok, Level} = erlmcp_logging:get_global_level(),
    ?assertEqual(info, Level).

test_set_warning_level() ->
    ok = erlmcp_logging:set_global_level(warning),
    {ok, Level} = erlmcp_logging:get_global_level(),
    ?assertEqual(warning, Level).

test_set_error_level() ->
    ok = erlmcp_logging:set_global_level(error),
    {ok, Level} = erlmcp_logging:get_global_level(),
    ?assertEqual(error, Level).

test_set_critical_level() ->
    ok = erlmcp_logging:set_global_level(critical),
    {ok, Level} = erlmcp_logging:get_global_level(),
    ?assertEqual(critical, Level).

test_set_invalid_level_error() ->
    Result = erlmcp_logging:set_global_level(invalid),
    ?assertMatch({error, invalid_level}, Result).

%%====================================================================
%% Session-Specific Level Tests
%%====================================================================

session_level_test_() ->
    {setup,
     fun setup_with_ets/0,
     fun cleanup_with_ets/1,
     fun(_) ->
         [
             ?_test(test_session_default_to_global()),
             ?_test(test_set_session_debug_level()),
             ?_test(test_set_session_info_level()),
             ?_test(test_get_session_level()),
             ?_test(test_remove_session_level()),
             ?_test(test_multiple_sessions()),
             ?_test(test_session_invalid_level())
         ]
     end}.

test_session_default_to_global() ->
    application:unset_env(erlmcp, global_log_level),
    SessionId = <<"session_1">>,
    {ok, Level} = erlmcp_logging:get_session_level(SessionId),
    ?assertEqual(info, Level).

test_set_session_debug_level() ->
    SessionId = <<"session_debug">>,
    ok = erlmcp_logging:set_session_level(SessionId, debug),
    {ok, Level} = erlmcp_logging:get_session_level(SessionId),
    ?assertEqual(debug, Level).

test_set_session_info_level() ->
    SessionId = <<"session_info">>,
    ok = erlmcp_logging:set_session_level(SessionId, info),
    {ok, Level} = erlmcp_logging:get_session_level(SessionId),
    ?assertEqual(info, Level).

test_get_session_level() ->
    SessionId = <<"session_get">>,
    erlmcp_logging:set_session_level(SessionId, warning),
    {ok, Level} = erlmcp_logging:get_session_level(SessionId),
    ?assertEqual(warning, Level).

test_remove_session_level() ->
    SessionId = <<"session_remove">>,
    erlmcp_logging:set_session_level(SessionId, error),
    erlmcp_logging:remove_session_level(SessionId),
    application:unset_env(erlmcp, global_log_level),
    {ok, Level} = erlmcp_logging:get_session_level(SessionId),
    ?assertEqual(info, Level).

test_multiple_sessions() ->
    Session1 = <<"session_multi_1">>,
    Session2 = <<"session_multi_2">>,
    Session3 = <<"session_multi_3">>,

    erlmcp_logging:set_session_level(Session1, debug),
    erlmcp_logging:set_session_level(Session2, info),
    erlmcp_logging:set_session_level(Session3, error),

    {ok, Level1} = erlmcp_logging:get_session_level(Session1),
    {ok, Level2} = erlmcp_logging:get_session_level(Session2),
    {ok, Level3} = erlmcp_logging:get_session_level(Session3),

    ?assertEqual(debug, Level1),
    ?assertEqual(info, Level2),
    ?assertEqual(error, Level3).

test_session_invalid_level() ->
    SessionId = <<"session_invalid">>,
    Result = erlmcp_logging:set_session_level(SessionId, invalid),
    ?assertMatch({error, invalid_level}, Result).

%%====================================================================
%% Constants and Validation Tests
%%====================================================================

constants_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_valid_levels_constant()),
             ?_test(test_default_level_constant()),
             ?_test(test_logging_method_constant())
         ]
     end}.

test_valid_levels_constant() ->
    ValidLevels = ?MCP_VALID_LOG_LEVELS,
    ?assert(lists:member(debug, ValidLevels)),
    ?assert(lists:member(info, ValidLevels)),
    ?assert(lists:member(warning, ValidLevels)),
    ?assert(lists:member(error, ValidLevels)),
    ?assert(lists:member(critical, ValidLevels)),
    ?assertEqual(5, length(ValidLevels)).

test_default_level_constant() ->
    DefaultLevel = ?MCP_DEFAULT_LOG_LEVEL,
    ?assertEqual(info, DefaultLevel).

test_logging_method_constant() ->
    Method = ?MCP_METHOD_LOGGING_SET_LEVEL,
    ?assertEqual(<<"logging/setLevel">>, Method).

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

edge_case_test_() ->
    {setup,
     fun setup_with_ets/0,
     fun cleanup_with_ets/1,
     fun(_) ->
         [
             ?_test(test_empty_binary_level()),
             ?_test(test_null_level()),
             ?_test(test_numeric_level()),
             ?_test(test_mixed_case_binary()),
             ?_test(test_whitespace_in_level())
         ]
     end}.

test_empty_binary_level() ->
    Result = erlmcp_logging:validate_log_level(<<>>),
    ?assertMatch({error, invalid_level}, Result).

test_null_level() ->
    Result = erlmcp_logging:validate_log_level(null),
    ?assertMatch({error, invalid_level}, Result).

test_numeric_level() ->
    Result = erlmcp_logging:validate_log_level(123),
    ?assertMatch({error, invalid_level}, Result).

test_mixed_case_binary() ->
    Result = erlmcp_logging:validate_log_level(<<"DEBUG">>),
    ?assertMatch({error, invalid_level}, Result).

test_whitespace_in_level() ->
    Result = erlmcp_logging:validate_log_level(<<"debug ">>),
    ?assertMatch({error, invalid_level}, Result).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:unset_env(erlmcp, global_log_level),
    ok.

setup_with_ets() ->
    setup(),
    try
        ets:new(erlmcp_session_log_levels, [
            named_table,
            public,
            set,
            {keypos, 1}
        ])
    catch
        error:badarg ->
            ets:delete_all_objects(erlmcp_session_log_levels)
    end,
    ok.

cleanup_with_ets(_) ->
    cleanup(ok),
    catch ets:delete(erlmcp_session_log_levels),
    ok.
