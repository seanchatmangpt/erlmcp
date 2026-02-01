%%%====================================================================
%%% @doc Test Suite for erlmcp_prompt_template Validation
%%% Chicago School TDD - Test API boundaries, no state inspection
%%% @end
%%%====================================================================
-module(erlmcp_prompt_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Template Validation Tests
%%====================================================================

template_validation_test_() ->
    [?_test(test_template_validation_valid()),
     ?_test(test_template_validation_invalid_syntax()),
     ?_test(test_template_validation_too_large())].

test_template_validation_valid() ->
    Template = <<"Hello, {{name}}!">>,
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)).

test_template_validation_invalid_syntax() ->
    Template = <<"{{#section}}content">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(Template)).

test_template_validation_too_large() ->
    %% Create a template larger than MAX_TEMPLATE_SIZE (10240 bytes)
    LargeTemplate = binary:copy(<<"a">>, 10241),
    ?assertMatch({error, {template_too_large, _, _}},
                 erlmcp_prompt_template:validate(LargeTemplate)).

%%====================================================================
%% Template Syntax Detection Tests
%%====================================================================

template_syntax_detection_test_() ->
    [?_test(test_has_template_syntax_true()), ?_test(test_has_template_syntax_false())].

test_has_template_syntax_true() ->
    Template = <<"Hello, {{name}}!">>,
    ?assertEqual(true, erlmcp_prompt_template:has_template_syntax(Template)).

test_has_template_syntax_false() ->
    Template = <<"Hello, World!">>,
    ?assertEqual(false, erlmcp_prompt_template:has_template_syntax(Template)).

%%====================================================================
%% Security Tests
%%====================================================================

security_validation_test_() ->
    [?_test(test_security_invalid_variable_name()),
     ?_test(test_security_variable_name_too_long()),
     ?_test(test_security_variable_value_too_large()),
     ?_test(test_security_nesting_too_deep()),
     ?_test(test_security_output_too_large())].

test_security_invalid_variable_name() ->
    Template = <<"Hello, {{evil-name}}!">>,
    Variables = #{<<"evil-name">> => <<"Hacker">>},
    ?assertMatch({error, {invalid_variable_name, _}},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

test_security_variable_name_too_long() ->
    LongName =
        <<"very_long_variable_name_that_exceeds_the_maximum_allowed_length_of_sixty_four_characters">>,
    Template = <<"Hello, {{", LongName/binary, "}}!">>,
    Variables = #{LongName => <<"value">>},
    ?assertMatch({error, {variable_name_too_long, _, _, _}},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

test_security_variable_value_too_large() ->
    Template = <<"Value: {{data}}">>,
    LargeValue = binary:copy(<<"x">>, 10241),  %% MAX_VARIABLE_VALUE_LEN is 10240
    Variables = #{<<"data">> => LargeValue},
    ?assertMatch({error, {variable_value_too_large, _, _, _}},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

test_security_nesting_too_deep() ->
    %% Create a template with more than 5 levels of nesting
    Template = <<"{{#a}}{{#b}}{{#c}}{{#d}}{{#e}}{{#f}}deep{{/f}}{{/e}}{{/d}}{{/c}}{{/b}}{{/a}}">>,
    ?assertMatch({error, {nesting_too_deep, _, _}}, erlmcp_prompt_template:validate(Template)).

test_security_output_too_large() ->
    %% Create a template that would generate output larger than MAX_OUTPUT_SIZE (100KB)
    Template = <<"{{#items}}x{{/items}}">>,
    %% Create list large enough to exceed output limit
    LargeList = lists:duplicate(200000, #{<<"x">> => <<"data">>}),
    Variables = #{<<"items">> => LargeList},
    ?assertMatch({error, {output_too_large, _, _}},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

%%====================================================================
%% Render Safe Tests
%%====================================================================

render_safe_test_() ->
    [?_test(test_render_safe_valid()), ?_test(test_render_safe_invalid())].

test_render_safe_valid() ->
    Template = <<"Hello, {{name}}!">>,
    Variables = #{<<"name">> => <<"Safe">>},
    ?assertMatch({ok, <<"Hello, Safe!">>}, erlmcp_prompt_template:render_safe(Template, Variables)).

test_render_safe_invalid() ->
    Template = <<"Hello, {{evil-name}}!">>,
    Variables = #{<<"evil-name">> => <<"Hacker">>},
    ?assertMatch({error, {invalid_variable_name, _}},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

%%====================================================================
%% Variable Name Validation Tests
%%====================================================================

variable_name_validation_test_() ->
    [?_test(test_invalid_variable_name_starting_with_number()),
     ?_test(test_section_with_non_map_items())].

test_invalid_variable_name_starting_with_number() ->
    Template = <<"{{123var}}">>,
    Variables = #{<<"123var">> => <<"invalid">>},
    ?assertMatch({error, {invalid_variable_name, _}},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

test_section_with_non_map_items() ->
    Template = <<"{{#items}}{{name}}{{/items}}">>,
    Variables = #{<<"items">> => [not_a_map]},
    ?assertMatch({error, list_items_must_be_maps},
                 erlmcp_prompt_template:render_safe(Template, Variables)).

%%====================================================================
%% Size Limit Tests
%%====================================================================

size_limit_test_() ->
    [?_test(test_template_exactly_at_size_limit()),
     ?_test(test_variable_name_exactly_at_max_length()),
     ?_test(test_variable_value_exactly_at_max_size())].

test_template_exactly_at_size_limit() ->
    Template = binary:copy(<<"a">>, 10240),  %% Exactly MAX_TEMPLATE_SIZE
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)).

test_variable_name_exactly_at_max_length() ->
    Template = <<"{{", (binary:copy(<<"x">>, 64))/binary, "}}">>,
    Variables = #{binary:copy(<<"x">>, 64) => <<"value">>},
    ?assertMatch({ok, _}, erlmcp_prompt_template:render_safe(Template, Variables)).

test_variable_value_exactly_at_max_size() ->
    Template = <<"{{data}}">>,
    Value = binary:copy(<<"x">>, 10240),  %% Exactly MAX_VARIABLE_VALUE_LEN
    Variables = #{<<"data">> => Value},
    ?assertMatch({ok, _}, erlmcp_prompt_template:render_safe(Template, Variables)).

%%====================================================================
%% Template Structure Tests
%%====================================================================

template_structure_test_() ->
    [?_test(test_unclosed_section_tag()),
     ?_test(test_mismatched_closing_tag()),
     ?_test(test_nested_sections_within_limit())].

test_unclosed_section_tag() ->
    Template = <<"{{#section}}content">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(Template)).

test_mismatched_closing_tag() ->
    Template = <<"{{#a}}content{{/b}}">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(Template)).

test_nested_sections_within_limit() ->
    Template = <<"{{#a}}{{#b}}{{#c}}{{#d}}{{#e}}content{{/e}}{{/d}}{{/c}}{{/b}}{{/a}}">>,
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)).

%%====================================================================
%% Error Return Tests
%%====================================================================

error_return_test_() ->
    [?_test(test_render_safe_invalid_variable_name_error()),
     ?_test(test_render_safe_variable_name_too_long_error()),
     ?_test(test_render_safe_variable_value_too_large_error()),
     ?_test(test_render_safe_nesting_too_deep_error()),
     ?_test(test_render_safe_output_too_large_error())].

test_render_safe_invalid_variable_name_error() ->
    Template = <<"{{invalid-name}}">>,
    Variables = #{<<"invalid-name">> => <<"value">>},
    Result = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertMatch({error, {invalid_variable_name, _}}, Result).

test_render_safe_variable_name_too_long_error() ->
    LongName = <<"this_variable_name_is_exactly_sixty_five_characters_long">>,
    Template = <<"{{", LongName/binary, "}}">>,
    Variables = #{LongName => <<"value">>},
    Result = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertMatch({error, {variable_name_too_long, _, _, _}}, Result).

test_render_safe_variable_value_too_large_error() ->
    Template = <<"{{data}}">>,
    LargeValue = binary:copy(<<"x">>, 10241),
    Variables = #{<<"data">> => LargeValue},
    Result = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertMatch({error, {variable_value_too_large, _, _, _}}, Result).

test_render_safe_nesting_too_deep_error() ->
    Template = <<"{{#a}}{{#b}}{{#c}}{{#d}}{{#e}}{{#f}}deep{{/f}}{{/e}}{{/d}}{{/c}}{{/b}}{{/a}}">>,
    Variables = #{},
    Result = erlmcp_prompt_template:validate(Template),
    ?assertMatch({error, {nesting_too_deep, _, _}}, Result).

test_render_safe_output_too_large_error() ->
    Template = <<"{{#items}}x{{/items}}">>,
    LargeList = lists:duplicate(200000, #{}),
    Variables = #{<<"items">> => LargeList},
    Result = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertMatch({error, {output_too_large, _, _}}, Result).
