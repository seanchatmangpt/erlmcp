-module(erlmcp_sampling_strategy_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

%% Test valid deterministic strategy
validate_strategy_deterministic_test() ->
    ok = erlmcp_sampling_strategy:validate_strategy(?MCP_SAMPLING_STRATEGY_DETERMINISTIC).

%% Test valid uniform strategy
validate_strategy_uniform_test() ->
    ok = erlmcp_sampling_strategy:validate_strategy(?MCP_SAMPLING_STRATEGY_UNIFORM).

%% Test undefined strategy (optional - should be ok)
validate_strategy_undefined_test() ->
    ok = erlmcp_sampling_strategy:validate_strategy(undefined).

%% Test invalid strategy name
validate_strategy_invalid_name_test() ->
    {error, {Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy(<<"invalid_strategy">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code),
    ?assert(is_map(Data)),
    ?assertEqual(<<"invalid_strategy">>, maps:get(<<"provided">>, Data)).

%% Test case sensitivity - lowercase should not match
validate_strategy_case_sensitivity_lowercase_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"deterministic">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test case sensitivity - uppercase should not match
validate_strategy_case_sensitivity_uppercase_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"DETERMINISTIC">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test invalid type - integer
validate_strategy_invalid_type_integer_test() ->
    {error, {Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy(123),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code),
    ?assert(is_map(Data)).

%% Test invalid type - atom
validate_strategy_invalid_type_atom_test() ->
    {error, {Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy(deterministic),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code),
    ?assert(is_map(Data)).

%% Test invalid type - list
validate_strategy_invalid_type_list_test() ->
    {error, {Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy([deterministic]),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code),
    ?assert(is_map(Data)).

%% Test invalid type - map
validate_strategy_invalid_type_map_test() ->
    {error, {Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy(#{strategy => deterministic}),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code),
    ?assert(is_map(Data)).

%% Test error response includes valid strategies list
validate_strategy_error_includes_valid_list_test() ->
    {error, {_Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy(<<"bad_strategy">>),
    ?assert(maps:is_key(<<"valid_strategies">>, Data)),
    ValidStrategies = maps:get(<<"valid_strategies">>, Data),
    ?assert(is_list(ValidStrategies)),
    ?assert(lists:member(?MCP_SAMPLING_STRATEGY_DETERMINISTIC, ValidStrategies)),
    ?assert(lists:member(?MCP_SAMPLING_STRATEGY_UNIFORM, ValidStrategies)).

%% Test error response includes provided value
validate_strategy_error_includes_provided_test() ->
    ProvidedStrategy = <<"wrong_strategy">>,
    {error, {_Code, _Msg, Data}} = erlmcp_sampling_strategy:validate_strategy(ProvidedStrategy),
    ?assertEqual(ProvidedStrategy, maps:get(<<"provided">>, Data)).

%% Test error message is correct
validate_strategy_error_message_test() ->
    {error, {_Code, Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"invalid">>),
    ?assertEqual(?MCP_MSG_INVALID_SAMPLING_STRATEGY, Msg).

%% Test is_valid_strategy with deterministic
is_valid_strategy_deterministic_test() ->
    ?assert(erlmcp_sampling_strategy:is_valid_strategy(?MCP_SAMPLING_STRATEGY_DETERMINISTIC)).

%% Test is_valid_strategy with uniform
is_valid_strategy_uniform_test() ->
    ?assert(erlmcp_sampling_strategy:is_valid_strategy(?MCP_SAMPLING_STRATEGY_UNIFORM)).

%% Test is_valid_strategy with invalid
is_valid_strategy_invalid_test() ->
    ?assertNot(erlmcp_sampling_strategy:is_valid_strategy(<<"invalid">>)).

%% Test is_valid_strategy with undefined
is_valid_strategy_undefined_test() ->
    ?assertNot(erlmcp_sampling_strategy:is_valid_strategy(undefined)).

%% Test is_valid_strategy with atom (not binary)
is_valid_strategy_atom_test() ->
    ?assertNot(erlmcp_sampling_strategy:is_valid_strategy(deterministic)).

%% Test is_valid_strategy with integer
is_valid_strategy_integer_test() ->
    ?assertNot(erlmcp_sampling_strategy:is_valid_strategy(123)).

%% Test get_valid_strategies returns non-empty list
get_valid_strategies_not_empty_test() ->
    Strategies = erlmcp_sampling_strategy:get_valid_strategies(),
    ?assert(is_list(Strategies)),
    ?assert(length(Strategies) > 0).

%% Test get_valid_strategies includes deterministic
get_valid_strategies_includes_deterministic_test() ->
    Strategies = erlmcp_sampling_strategy:get_valid_strategies(),
    ?assert(lists:member(?MCP_SAMPLING_STRATEGY_DETERMINISTIC, Strategies)).

%% Test get_valid_strategies includes uniform
get_valid_strategies_includes_uniform_test() ->
    Strategies = erlmcp_sampling_strategy:get_valid_strategies(),
    ?assert(lists:member(?MCP_SAMPLING_STRATEGY_UNIFORM, Strategies)).

%% Test get_valid_strategies returns exactly 2 strategies
get_valid_strategies_count_test() ->
    Strategies = erlmcp_sampling_strategy:get_valid_strategies(),
    ?assertEqual(2, length(Strategies)).

%% Test empty binary strategy
validate_strategy_empty_binary_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test strategy with whitespace
validate_strategy_with_whitespace_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"deterministic ">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test strategy with leading whitespace
validate_strategy_leading_whitespace_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<" deterministic">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test similar but different strategy names
validate_strategy_similar_name_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"determinist">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%% Test error code is always -32602 for invalid params
validate_strategy_error_code_consistency_deterministic_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(?MCP_SAMPLING_STRATEGY_DETERMINISTIC),
    % This should pass, not error
    ok = erlmcp_sampling_strategy:validate_strategy(?MCP_SAMPLING_STRATEGY_DETERMINISTIC).

%% Test error code is always -32602 for invalid params - invalid case
validate_strategy_error_code_consistency_invalid_test() ->
    {error, {Code, _Msg, _Data}} = erlmcp_sampling_strategy:validate_strategy(<<"bad">>),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code).

%%====================================================================
%% Test Suite Definition
%%====================================================================

sampling_strategy_tests_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {<<"Validation - Valid Strategies">>, [
            ?_test(validate_strategy_deterministic_test()),
            ?_test(validate_strategy_uniform_test()),
            ?_test(validate_strategy_undefined_test())
        ]},
        {<<"Validation - Invalid Strategies">>, [
            ?_test(validate_strategy_invalid_name_test()),
            ?_test(validate_strategy_case_sensitivity_lowercase_test()),
            ?_test(validate_strategy_case_sensitivity_uppercase_test()),
            ?_test(validate_strategy_empty_binary_test()),
            ?_test(validate_strategy_with_whitespace_test()),
            ?_test(validate_strategy_leading_whitespace_test()),
            ?_test(validate_strategy_similar_name_test())
        ]},
        {<<"Validation - Invalid Types">>, [
            ?_test(validate_strategy_invalid_type_integer_test()),
            ?_test(validate_strategy_invalid_type_atom_test()),
            ?_test(validate_strategy_invalid_type_list_test()),
            ?_test(validate_strategy_invalid_type_map_test())
        ]},
        {<<"Error Response Format">>, [
            ?_test(validate_strategy_error_includes_valid_list_test()),
            ?_test(validate_strategy_error_includes_provided_test()),
            ?_test(validate_strategy_error_message_test()),
            ?_test(validate_strategy_error_code_consistency_invalid_test())
        ]},
        {<<"is_valid_strategy Helper">>, [
            ?_test(is_valid_strategy_deterministic_test()),
            ?_test(is_valid_strategy_uniform_test()),
            ?_test(is_valid_strategy_invalid_test()),
            ?_test(is_valid_strategy_undefined_test()),
            ?_test(is_valid_strategy_atom_test()),
            ?_test(is_valid_strategy_integer_test())
        ]},
        {<<"get_valid_strategies Helper">>, [
            ?_test(get_valid_strategies_not_empty_test()),
            ?_test(get_valid_strategies_includes_deterministic_test()),
            ?_test(get_valid_strategies_includes_uniform_test()),
            ?_test(get_valid_strategies_count_test())
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
