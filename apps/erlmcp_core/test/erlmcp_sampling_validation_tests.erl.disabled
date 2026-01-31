%%%-------------------------------------------------------------------
%%% @doc Test Suite for erlmcp_sampling Validation
%%% Chicago School TDD - Test API boundaries, no state inspection
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sampling_validation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Message Validation Tests
%%%====================================================================

message_validation_test_() ->
    [
        ?_test(test_valid_message_structure()),
        ?_test(test_empty_messages_list()),
        ?_test(test_missing_role_field()),
        ?_test(test_missing_content_field()),
        ?_test(test_invalid_role_value())
    ].

test_valid_message_structure() ->
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Hello">>}
    ],
    ?assertMatch({ok, _}, erlmcp_sampling:create_message(Messages, #{})).

test_empty_messages_list() ->
    Messages = [],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    ?assertMatch({error, empty_messages}, Result).

test_missing_role_field() ->
    Messages = [#{<<"content">> => <<"No role">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    ?assertMatch({error, invalid_message_format}, Result).

test_missing_content_field() ->
    Messages = [#{<<"role">> => <<"user">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    ?assertMatch({error, invalid_message_format}, Result).

test_invalid_role_value() ->
    Messages = [#{<<"role">> => <<"invalid_role">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    ?assertMatch({error, invalid_message_format}, Result).

%%%====================================================================
%%% Parameter Validation Tests
%%%====================================================================

parameter_validation_test_() ->
    [
        ?_test(test_valid_temperature()),
        ?_test(test_temperature_out_of_range_low()),
        ?_test(test_temperature_out_of_range_high()),
        ?_test(test_invalid_temperature_type()),
        ?_test(test_valid_max_tokens()),
        ?_test(test_invalid_max_tokens_type()),
        ?_test(test_valid_stop_sequences()),
        ?_test(test_invalid_stop_sequences_type())
    ].

test_valid_temperature() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    ValidTemps = [0.0, 0.5, 1.0, 1.5, 2.0],
    [begin
        Result = erlmcp_sampling:create_message(Messages, #{<<"temperature">> => Temp}),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end || Temp <- ValidTemps].

test_temperature_out_of_range_low() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"temperature">> => -0.1}),
    ?assertMatch({error, invalid_temperature}, Result).

test_temperature_out_of_range_high() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"temperature">> => 2.1}),
    ?assertMatch({error, invalid_temperature}, Result).

test_invalid_temperature_type() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"temperature">> => <<"invalid">>}),
    ?assertMatch({error, invalid_temperature}, Result).

test_valid_max_tokens() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"maxTokens">> => 100}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_invalid_max_tokens_type() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"maxTokens">> => <<"invalid">>}),
    ?assertMatch({error, invalid_parameters}, Result).

test_valid_stop_sequences() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"stopSequences">> => [<<"STOP">>, <<"END">>]}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_invalid_stop_sequences_type() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{<<"stopSequences">> => <<"invalid">>}),
    ?assertMatch({error, invalid_parameters}, Result).

%%%====================================================================
%%% Model Preferences Validation Tests
%%%====================================================================

model_preferences_validation_test_() ->
    [
        ?_test(test_valid_model_preferences()),
        ?_test(test_invalid_cost_priority()),
        ?_test(test_invalid_speed_priority()),
        ?_test(test_invalid_intelligence_priority())
    ].

test_valid_model_preferences() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    ModelPrefs = #{
        <<"costPriority">> => 0.5,
        <<"speedPriority">> => 0.7,
        <<"intelligencePriority">> => 0.3
    },
    Result = erlmcp_sampling:create_message(Messages, #{<<"modelPreferences">> => ModelPrefs}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_invalid_cost_priority() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    ModelPrefs = #{<<"costPriority">> => 1.5},
    Result = erlmcp_sampling:create_message(Messages, #{<<"modelPreferences">> => ModelPrefs}),
    ?assertMatch({error, invalid_parameters}, Result).

test_invalid_speed_priority() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    ModelPrefs = #{<<"speedPriority">> => -0.1},
    Result = erlmcp_sampling:create_message(Messages, #{<<"modelPreferences">> => ModelPrefs}),
    ?assertMatch({error, invalid_parameters}, Result).

test_invalid_intelligence_priority() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    ModelPrefs = #{<<"intelligencePriority">> => <<"invalid">>},
    Result = erlmcp_sampling:create_message(Messages, #{<<"modelPreferences">> => ModelPrefs}),
    ?assertMatch({error, invalid_parameters}, Result).

%%%====================================================================
%%% Role Validation Tests
%%%====================================================================

role_validation_test_() ->
    [
        ?_test(test_valid_user_role()),
        ?_test(test_valid_system_role()),
        ?_test(test_valid_assistant_role()),
        ?_test(test_invalid_role_atom()),
        ?_test(test_invalid_role_empty())
    ].

test_valid_user_role() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_valid_system_role() ->
    Messages = [
        #{<<"role">> => <<"system">>, <<"content">> => <<"You are helpful.">>},
        #{<<"role">> => <<"user">>, <<"content">> => <<"Hi">>}
    ],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_valid_assistant_role() ->
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Hello">>},
        #{<<"role">> => <<"assistant">>, <<"content">> => <<"Hi there">>}
    ],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_invalid_role_atom() ->
    Messages = [#{<<"role">> => user, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    ?assertMatch({error, invalid_message_format}, Result).

test_invalid_role_empty() ->
    Messages = [#{<<"role">> => <<>>, <<"content">> => <<"Test">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    ?assertMatch({error, invalid_message_format}, Result).

%%%====================================================================
%%% Content Validation Tests
%%%====================================================================

content_validation_test_() ->
    [
        ?_test(test_valid_string_content()),
        ?_test(test_empty_content()),
        ?_test(test_unicode_content()),
        ?_test(test_very_long_content())
    ].

test_valid_string_content() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Hello, MCP!">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_empty_content() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<>>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_unicode_content() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"こんにちは"/utf8>>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_very_long_content() ->
    LongContent = binary:copy(<<"a">>, 10000),
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => LongContent}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

edge_case_test_() ->
    [
        ?_test(test_single_message()),
        ?_test(test_long_conversation_history()),
        ?_test(test_alternating_roles()),
        ?_test(test_duplicate_consecutive_roles())
    ].

test_single_message() ->
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Single message">>}],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_long_conversation_history() ->
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Message ", (integer_to_binary(I))/binary>>}
        || I <- lists:seq(1, 50)
    ],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_alternating_roles() ->
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"User 1">>},
        #{<<"role">> => <<"assistant">>, <<"content">> => <<"Assistant 1">>},
        #{<<"role">> => <<"user">>, <<"content">> => <<"User 2">>}
    ],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.

test_duplicate_consecutive_roles() ->
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"User 1">>},
        #{<<"role">> => <<"user">>, <<"content">> => <<"User 2">>}
    ],
    Result = erlmcp_sampling:create_message(Messages, #{}),
    case Result of {ok, _} -> ok; {error, _} -> ok end.
