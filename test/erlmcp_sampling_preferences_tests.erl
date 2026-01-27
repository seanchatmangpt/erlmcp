-module(erlmcp_sampling_preferences_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

%% Test extracting model preferences from empty params
extract_empty_params_test() ->
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(#{}),
    ?assertEqual(undefined, Prefs#mcp_model_preferences.temperature),
    ?assertEqual(4096, Prefs#mcp_model_preferences.max_tokens),
    ?assertEqual(undefined, Prefs#mcp_model_preferences.stop_sequences).

%% Test extracting model preferences with temperature
extract_with_temperature_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_TEMPERATURE => 0.7
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(0.7, Prefs#mcp_model_preferences.temperature),
    ?assertEqual(undefined, Prefs#mcp_model_preferences.stop_sequences).

%% Test extracting model preferences with max_tokens
extract_with_max_tokens_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_MAX_TOKENS => 2048
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(2048, Prefs#mcp_model_preferences.max_tokens).

%% Test extracting model preferences with stop sequences
extract_with_stop_sequences_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_STOP_SEQUENCES => [<<"\\n">>, <<"END">>]
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual([<<"\\n">>, <<"END">>], Prefs#mcp_model_preferences.stop_sequences).

%% Test extracting all preference fields
extract_all_preferences_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_COST_PRIORITY => 0.3,
        ?MCP_PARAM_SPEED_PRIORITY => 0.6,
        ?MCP_PARAM_INTELLIGENCE_PRIORITY => 0.9,
        ?MCP_PARAM_TEMPERATURE => 1.0,
        ?MCP_PARAM_MAX_TOKENS => 8192,
        ?MCP_PARAM_STOP_SEQUENCES => [<<"END">>]
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(0.3, Prefs#mcp_model_preferences.cost_priority),
    ?assertEqual(0.6, Prefs#mcp_model_preferences.speed_priority),
    ?assertEqual(0.9, Prefs#mcp_model_preferences.intelligence_priority),
    ?assertEqual(1.0, Prefs#mcp_model_preferences.temperature),
    ?assertEqual(8192, Prefs#mcp_model_preferences.max_tokens),
    ?assertEqual([<<"END">>], Prefs#mcp_model_preferences.stop_sequences).

%% Test invalid temperature (too low)
validate_invalid_temperature_low_test() ->
    Prefs = #mcp_model_preferences{temperature = -0.5},
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test invalid temperature (too high)
validate_invalid_temperature_high_test() ->
    Prefs = #mcp_model_preferences{temperature = 2.5},
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test valid temperature boundaries (0.0)
validate_temperature_zero_test() ->
    Prefs = #mcp_model_preferences{temperature = 0.0},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test valid temperature boundaries (2.0)
validate_temperature_max_test() ->
    Prefs = #mcp_model_preferences{temperature = 2.0},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test valid temperature boundaries (1.0 - middle)
validate_temperature_middle_test() ->
    Prefs = #mcp_model_preferences{temperature = 1.0},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test invalid max_tokens (zero)
validate_invalid_max_tokens_zero_test() ->
    Prefs = #mcp_model_preferences{max_tokens = 0},
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test invalid max_tokens (negative)
validate_invalid_max_tokens_negative_test() ->
    Prefs = #mcp_model_preferences{max_tokens = -100},
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test valid max_tokens (1)
validate_max_tokens_one_test() ->
    Prefs = #mcp_model_preferences{max_tokens = 1},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test valid max_tokens (large)
validate_max_tokens_large_test() ->
    Prefs = #mcp_model_preferences{max_tokens = 1000000},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test invalid stop_sequences (non-string)
validate_invalid_stop_sequences_non_string_test() ->
    Prefs = #mcp_model_preferences{stop_sequences = [<<"valid">>, 123, <<"also_valid">>]},
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test valid stop_sequences (empty list)
validate_stop_sequences_empty_test() ->
    Prefs = #mcp_model_preferences{stop_sequences = []},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test valid stop_sequences (single string)
validate_stop_sequences_single_test() ->
    Prefs = #mcp_model_preferences{stop_sequences = [<<"END">>]},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test valid stop_sequences (multiple strings)
validate_stop_sequences_multiple_test() ->
    Prefs = #mcp_model_preferences{stop_sequences = [<<"\\n">>, <<"\\n\\n">>, <<"EOF">>]},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test undefined preferences (all defaults)
validate_all_undefined_test() ->
    Prefs = #mcp_model_preferences{},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%% Test invalid modelPreferences not a map
extract_invalid_prefs_not_map_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => [123, 456]},
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test invalid params (not a map)
extract_invalid_params_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:extract_model_preferences([]),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test extraction with invalid temperature value type
extract_invalid_temperature_type_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_TEMPERATURE => <<"1.0">>
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    % Should still work but validation will fail
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test extraction with invalid max_tokens value type
extract_invalid_max_tokens_type_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_MAX_TOKENS => <<"2048">>
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test extraction with invalid stop_sequences value type
extract_invalid_stop_sequences_type_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_STOP_SEQUENCES => <<"END">>
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    {error, {Code, _Msg, _Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test partial preferences (only temperature)
extract_partial_temperature_only_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_TEMPERATURE => 0.5
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ok = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(0.5, Prefs#mcp_model_preferences.temperature),
    ?assertEqual(4096, Prefs#mcp_model_preferences.max_tokens).

%% Test partial preferences (temperature and max_tokens)
extract_partial_temp_and_tokens_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_TEMPERATURE => 1.2,
        ?MCP_PARAM_MAX_TOKENS => 1024
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ok = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assertEqual(1.2, Prefs#mcp_model_preferences.temperature),
    ?assertEqual(1024, Prefs#mcp_model_preferences.max_tokens).

%% Test priority clamping (over 1.0)
extract_priority_clamp_high_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_COST_PRIORITY => 1.5
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(1.0, Prefs#mcp_model_preferences.cost_priority).

%% Test priority clamping (below 0.0)
extract_priority_clamp_low_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_SPEED_PRIORITY => -0.5
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(0.0, Prefs#mcp_model_preferences.speed_priority).

%% Test priority normalization (valid range)
extract_priority_valid_range_test() ->
    Params = #{?MCP_PARAM_MODEL_PREFERENCES => #{
        ?MCP_PARAM_INTELLIGENCE_PRIORITY => 0.75
    }},
    {ok, Prefs} = erlmcp_sampling:extract_model_preferences(Params),
    ?assertEqual(0.75, Prefs#mcp_model_preferences.intelligence_priority).

%% Test error response includes data field
validate_error_has_data_field_test() ->
    Prefs = #mcp_model_preferences{temperature = 3.0},
    {error, {_Code, _Msg, Data}} = erlmcp_sampling:validate_model_preferences(Prefs),
    ?assert(is_map(Data)),
    ?assert(maps:is_key(<<"provided">>, Data) orelse maps:is_key(<<"reason">>, Data)).

%% Test temperature float handling
validate_temperature_float_handling_test() ->
    Prefs = #mcp_model_preferences{temperature = 0.123456},
    ok = erlmcp_sampling:validate_model_preferences(Prefs).

%%====================================================================
%% Test Suite Definition
%%====================================================================

sampling_preferences_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {<<"Extract Preferences">>, [
            ?_test(extract_empty_params_test()),
            ?_test(extract_with_temperature_test()),
            ?_test(extract_with_max_tokens_test()),
            ?_test(extract_with_stop_sequences_test()),
            ?_test(extract_all_preferences_test()),
            ?_test(extract_invalid_prefs_not_map_test()),
            ?_test(extract_invalid_params_test()),
            ?_test(extract_invalid_temperature_type_test()),
            ?_test(extract_invalid_max_tokens_type_test()),
            ?_test(extract_invalid_stop_sequences_type_test()),
            ?_test(extract_partial_temperature_only_test()),
            ?_test(extract_partial_temp_and_tokens_test()),
            ?_test(extract_priority_clamp_high_test()),
            ?_test(extract_priority_clamp_low_test()),
            ?_test(extract_priority_valid_range_test())
        ]},
        {<<"Validate Preferences">>, [
            ?_test(validate_invalid_temperature_low_test()),
            ?_test(validate_invalid_temperature_high_test()),
            ?_test(validate_temperature_zero_test()),
            ?_test(validate_temperature_max_test()),
            ?_test(validate_temperature_middle_test()),
            ?_test(validate_temperature_float_handling_test()),
            ?_test(validate_invalid_max_tokens_zero_test()),
            ?_test(validate_invalid_max_tokens_negative_test()),
            ?_test(validate_max_tokens_one_test()),
            ?_test(validate_max_tokens_large_test()),
            ?_test(validate_invalid_stop_sequences_non_string_test()),
            ?_test(validate_stop_sequences_empty_test()),
            ?_test(validate_stop_sequences_single_test()),
            ?_test(validate_stop_sequences_multiple_test()),
            ?_test(validate_all_undefined_test()),
            ?_test(validate_error_has_data_field_test())
        ]}
     ]
    }.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    ok.

teardown(_) ->
    ok.
