%%%====================================================================
%%% @doc Comprehensive Test Suite for erlmcp_prompt_template
%%%
%%% Tests:
%%% - Simple variable rendering
%%% - Multiple variables
%%% - Template validation (valid/invalid)
%%% - Section rendering ({{#}})
%%% - Inverted sections ({{^}})
%%% - Missing variable handling
%%% - Numeric value rendering
%%% - Security tests (injection attempts)
%%%
%%% Target: 80%+ coverage (Chicago School TDD)
%%% @end
%%%====================================================================
-module(erlmcp_prompt_template_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

prompt_template_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_simple_variable_rendering/1,
         fun test_multiple_variables/1,
         fun test_missing_variable_handling/1,
         fun test_numeric_value_rendering/1,
         fun test_boolean_value_rendering/1,
         fun test_section_rendering_truthy/1,
         fun test_section_rendering_falsy/1,
         fun test_section_rendering_list/1,
         fun test_inverted_section_truthy/1,
         fun test_inverted_section_falsy/1,
         fun test_inverted_section_empty_list/1,
         fun test_template_validation_valid/1,
         fun test_template_validation_invalid_syntax/1,
         fun test_template_validation_too_large/1,
         fun test_has_template_syntax_true/1,
         fun test_has_template_syntax_false/1,
         fun test_render_safe_valid/1,
         fun test_render_safe_invalid/1,
         fun test_security_invalid_variable_name/1,
         fun test_security_variable_name_too_long/1,
         fun test_security_variable_value_too_large/1,
         fun test_security_nesting_too_deep/1,
         fun test_security_output_too_large/1,
         fun test_whitespace_handling/1,
         fun test_empty_template/1,
         fun test_no_variables_template/1,
         fun test_compile_and_render_separate/1
     ]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    ok.

cleanup(_Pid) ->
    ok.

%%====================================================================
%% Basic Rendering Tests
%%====================================================================

test_simple_variable_rendering(_Pid) ->
    Template = <<"Hello, {{name}}!">>,
    Variables = #{<<"name">> => <<"World">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, World!">>, Result).

test_multiple_variables(_Pid) ->
    Template = <<"{{greeting}}, {{name}}! Today is {{day}}.">>,
    Variables = #{
        <<"greeting">> => <<"Hello">>,
        <<"name">> => <<"Alice">>,
        <<"day">> => <<"Monday">>
    },
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, Alice! Today is Monday.">>, Result).

test_missing_variable_handling(_Pid) ->
    Template = <<"Hello, {{name}}! You are {{age}} years old.">>,
    Variables = #{<<"name">> => <<"Bob">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    %% Missing variables render as empty string
    ?assertEqual(<<"Hello, Bob! You are  years old.">>, Result).

test_numeric_value_rendering(_Pid) ->
    Template = <<"Count: {{count}}, Price: ${{price}}">>,
    Variables = #{
        <<"count">> => 42,
        <<"price">> => 19.99
    },
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Count: 42, Price: $19.99">>, Result).

test_boolean_value_rendering(_Pid) ->
    Template = <<"Show: {{flag}}">>,
    Variables = #{<<"flag">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Show: true">>, Result).

%%====================================================================
%% Section Rendering Tests
%%====================================================================

test_section_rendering_truthy(_Pid) ->
    Template = <<"{{#show}}Display this{{/show}}">>,
    Variables = #{<<"show">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Display this">>, Result).

test_section_rendering_falsy(_Pid) ->
    Template = <<"{{#show}}Display this{{/show}}">>,
    Variables = #{<<"show">> => false},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

test_section_rendering_list(_Pid) ->
    Template = <<"{{#items}}- {{name}}\n{{/items}}">>,
    Variables = #{<<"items">> => [
        #{<<"name">> => <<"Item 1">>},
        #{<<"name">> => <<"Item 2">>},
        #{<<"name">> => <<"Item 3">>}
    ]},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"- Item 1\n- Item 2\n- Item 3\n">>, Result).

%%====================================================================
%% Inverted Section Tests
%%====================================================================

test_inverted_section_truthy(_Pid) ->
    Template = <<"{{^hide}}Hidden is false{{/hide}}">>,
    Variables = #{<<"hide">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    %% Inverted section does NOT render when value is truthy
    ?assertEqual(<<>>, Result).

test_inverted_section_falsy(_Pid) ->
    Template = <<"{{^hide}}Hidden is false{{/hide}}">>,
    Variables = #{<<"hide">> => false},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    %% Inverted section DOES render when value is falsy
    ?assertEqual(<<"Hidden is false">>, Result).

test_inverted_section_empty_list(_Pid) ->
    Template = <<"{{^items}}No items available{{/items}}">>,
    Variables = #{<<"items">> => []},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    %% Inverted section renders for empty list
    ?assertEqual(<<"No items available">>, Result).

%%====================================================================
%% Template Validation Tests
%%====================================================================

test_template_validation_valid(_Pid) ->
    Template = <<"Hello, {{name}}!">>,
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)).

test_template_validation_invalid_syntax(_Pid) ->
    Template = <<"Hello, {{valid {{name}}!">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(Template)).

test_template_validation_too_large(_Pid) ->
    %% Create a template larger than MAX_TEMPLATE_SIZE (10240 bytes)
    LargeTemplate = binary:copy(<<"a">>, 10241),
    ?assertMatch({error, {template_too_large, _, _}}, erlmcp_prompt_template:validate(LargeTemplate)).

%%====================================================================
%% Template Syntax Detection Tests
%%====================================================================

test_has_template_syntax_true(_Pid) ->
    Template = <<"Hello, {{name}}!">>,
    ?assertEqual(true, erlmcp_prompt_template:has_template_syntax(Template)).

test_has_template_syntax_false(_Pid) ->
    Template = <<"Hello, World!">>,
    ?assertEqual(false, erlmcp_prompt_template:has_template_syntax(Template)).

%%====================================================================
%% Render Safe Tests
%%====================================================================

test_render_safe_valid(_Pid) ->
    Template = <<"Hello, {{name}}!">>,
    Variables = #{<<"name">> => <<"Safe">>},
    ?assertMatch({ok, <<"Hello, Safe!">>}, erlmcp_prompt_template:render_safe(Template, Variables)).

test_render_safe_invalid(_Pid) ->
    Template = <<"Hello, {{evil-name}}!">>,
    Variables = #{<<"evil-name">> => <<"Hacker">>},
    ?assertMatch({error, {invalid_variable_name, _}}, erlmcp_prompt_template:render_safe(Template, Variables)).

%%====================================================================
%% Security Tests
%%====================================================================

test_security_invalid_variable_name(_Pid) ->
    Template = <<"Hello, {{evil-name}}!">>,
    Variables = #{<<"evil-name">> => <<"Hacker">>},
    ?assertMatch({error, {invalid_variable_name, _}}, erlmcp_prompt_template:render_safe(Template, Variables)).

test_security_variable_name_too_long(_Pid) ->
    Template = <<"Hello, {{very_long_variable_name}}!">>,
    LongName = binary:copy(<<"a">>, 65),  % MAX_VARIABLE_NAME_LEN is 64
    Variables = #{<<"very_long_variable_name">> => LongName},
    ?assertMatch({error, {variable_name_too_long, _, _, _}}, erlmcp_prompt_template:render_safe(Template, Variables)).

test_security_variable_value_too_large(_Pid) ->
    Template = <<"Value: {{data}}">>,
    LargeValue = binary:copy(<<"x">>, 10241),  % MAX_VARIABLE_VALUE_LEN is 10240
    Variables = #{<<"data">> => LargeValue},
    ?assertMatch({error, {variable_value_too_large, _, _, _}}, erlmcp_prompt_template:render_safe(Template, Variables)).

test_security_nesting_too_deep(_Pid) ->
    %% Create a template with more than 5 levels of nesting
    Template = <<"{{#a}}{{#b}}{{#c}}{{#d}}{{#e}}{{#f}}deep{{/f}}{{/e}}{{/d}}{{/c}}{{/b}}{{/a}}">>,
    Variables = #{<<"a">> => true, <<"b">> => true, <<"c">> => true, <<"d">> => true, <<"e">> => true, <<"f">> => true},
    ?assertMatch({error, {nesting_too_deep, _, _}}, erlmcp_prompt_template:validate(Template)).

test_security_output_too_large(_Pid) ->
    %% Create a template that would generate output larger than MAX_OUTPUT_SIZE (100KB)
    Template = <<"{{#items}}x{{/items}}">>,
    %% Create list large enough to exceed output limit
    LargeList = lists:duplicate(200000, #{<<"x">> => <<"data">>}),
    Variables = #{<<"items">> => LargeList},
    ?assertMatch({error, {output_too_large, _, _}}, erlmcp_prompt_template:render_safe(Template, Variables)).

%%====================================================================
%% Edge Cases Tests
%%====================================================================

test_whitespace_handling(_Pid) ->
    Template = <<"  Hello  ,  {{ name }}  !  ">>,
    Variables = #{<<"name">> => <<"World">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    %% bbmustache trims whitespace around variable names
    ?assertEqual(<<"  Hello  ,  World  !  ">>, Result).

test_empty_template(_Pid) ->
    Template = <<>>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

test_no_variables_template(_Pid) ->
    Template = <<"Hello, World!">>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, World!">>, Result).

test_compile_and_render_separate(_Pid) ->
    Template = <<"Hello, {{name}}!">>,
    {ok, Compiled} = erlmcp_prompt_template:compile(Template),
    Variables1 = #{<<"name">> => <<"Alice">>},
    {ok, Result1} = erlmcp_prompt_template:render(Compiled, Variables1),
    ?assertEqual(<<"Hello, Alice!">>, Result1),
    Variables2 = #{<<"name">> => <<"Bob">>},
    {ok, Result2} = erlmcp_prompt_template:render(Compiled, Variables2),
    ?assertEqual(<<"Hello, Bob!">>, Result2).

%%====================================================================
%% Additional Test Cases for Coverage
%%====================================================================

%% Test for valid variable names with underscores
valid_variable_name_with_underscores_test() ->
    Template = <<"{{my_variable_name}}">>,
    Variables = #{<<"my_variable_name">> => <<"value">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"value">>, Result).

%% Test for valid variable names starting with underscore
valid_variable_name_starting_with_underscore_test() ->
    Template = <<"{{_private}}">>,
    Variables = #{<<"_private">> => <<"secret">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"secret">>, Result).

%% Test for variable name with numbers
valid_variable_name_with_numbers_test() ->
    Template = <<"{{var123}}">>,
    Variables = #{<<"var123">> => <<"numeric">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"numeric">>, Result).

%% Test for invalid variable name starting with number
invalid_variable_name_starting_with_number_test() ->
    Template = <<"{{123var}}">>,
    Variables = #{<<"123var">> => <<"invalid">>},
    ?assertMatch({error, {invalid_variable_name, _}}, erlmcp_prompt_template:render_safe(Template, Variables)).

%% Test for section with list containing non-map items
section_with_non_map_items_test() ->
    Template = <<"{{#items}}{{name}}{{/items}}">>,
    Variables = #{<<"items">> => [not_a_map]},
    ?assertMatch({error, list_items_must_be_maps}, erlmcp_prompt_template:render_safe(Template, Variables)).

%% Test for template exactly at size limit
template_exactly_at_size_limit_test() ->
    Template = binary:copy(<<"a">>, 10240),  % Exactly MAX_TEMPLATE_SIZE
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)).

%% Test for variable name exactly at max length
variable_name_exactly_at_max_length_test() ->
    Template = <<"{{", (binary:copy(<<"x">>, 64))/binary, "}}">>,
    Variables = #{(binary:copy(<<"x">>, 64)) => <<"value">>},
    ?assertMatch({ok, _}, erlmcp_prompt_template:render_safe(Template, Variables)).

%% Test for variable value exactly at max size
variable_value_exactly_at_max_size_test() ->
    Template = <<"{{data}}">>,
    Value = binary:copy(<<"x">>, 10240),  % Exactly MAX_VARIABLE_VALUE_LEN
    Variables = #{<<"data">> => Value},
    ?assertMatch({ok, _}, erlmcp_prompt_template:render_safe(Template, Variables)).

%% Test for unclosed section tag
unclosed_section_tag_test() ->
    Template = <<"{{#section}}content">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(Template)).

%% Test for mismatched closing tag
mismatched_closing_tag_test() ->
    Template = <<"{{#a}}content{{/b}}">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(Template)).

%% Test for nested sections within limit
nested_sections_within_limit_test() ->
    Template = <<"{{#a}}{{#b}}{{#c}}{{#d}}{{#e}}content{{/e}}{{/d}}{{/c}}{{/b}}{{/a}}">>,
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)).

%% Test for floating point value
floating_point_value_test() ->
    Template = <<"{{value}}">>,
    Variables = #{<<"value">> => 3.14159},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"3.14159">>, Result).

%% Test for negative integer value
negative_integer_value_test() ->
    Template = <<"{{value}}">>,
    Variables = #{<<"value">> => -42},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"-42">>, Result).

%% Test for boolean false value
boolean_false_value_test() ->
    Template = <<"{{flag}}">>,
    Variables = #{<<"flag">> => false},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"false">>, Result).

%% Test for inverted section with undefined variable
inverted_section_with_undefined_variable_test() ->
    Template = <<"{{^missing}}Not found{{/missing}}">>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Not found">>, Result).

%% Test for section with undefined variable
section_with_undefined_variable_test() ->
    Template = <<"{{#missing}}content{{/missing}}">>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

%% Test for multiple sections
multiple_sections_test() ->
    Template = <<"{{#a}}A{{/a}}{{#b}}B{{/b}}{{#c}}C{{/c}}">>,
    Variables = #{<<"a">> => true, <<"b">> => true, <<"c">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"ABC">>, Result).

%% Test for mixed content
mixed_content_test() ->
    Template = <<"Start {{#items}}- {{name}} {{/items}}End">>,
    Variables = #{<<"items">> => [#{<<"name">> => <<"A">>}, #{<<"name">> => <<"B">>}]},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Start - A - B End">>, Result).
