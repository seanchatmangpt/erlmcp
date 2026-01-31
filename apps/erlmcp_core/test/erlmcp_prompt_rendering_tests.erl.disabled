%%%====================================================================
%%% @doc Test Suite for erlmcp_prompt_template Rendering
%%% Chicago School TDD - Test API boundaries, no state inspection
%%% @end
%%%====================================================================
-module(erlmcp_prompt_rendering_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Basic Rendering Tests
%%====================================================================

simple_variable_rendering_test_() ->
    [
        ?_test(test_single_variable()),
        ?_test(test_multiple_variables()),
        ?_test(test_missing_variable())
    ].

test_single_variable() ->
    Template = <<"Hello, {{name}}!">>,
    Variables = #{<<"name">> => <<"World">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, World!">>, Result).

test_multiple_variables() ->
    Template = <<"{{greeting}}, {{name}}! Today is {{day}}.">>,
    Variables = #{
        <<"greeting">> => <<"Hello">>,
        <<"name">> => <<"Alice">>,
        <<"day">> => <<"Monday">>
    },
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, Alice! Today is Monday.">>, Result).

test_missing_variable() ->
    Template = <<"Hello, {{name}}! You are {{age}} years old.">>,
    Variables = #{<<"name">> => <<"Bob">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, Bob! You are  years old.">>, Result).

%%====================================================================
%% Value Type Tests
%%====================================================================

value_type_rendering_test_() ->
    [
        ?_test(test_numeric_value()),
        ?_test(test_boolean_value()),
        ?_test(test_floating_point_value()),
        ?_test(test_negative_integer_value())
    ].

test_numeric_value() ->
    Template = <<"Count: {{count}}, Price: ${{price}}">>,
    Variables = #{
        <<"count">> => 42,
        <<"price">> => 19.99
    },
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Count: 42, Price: $19.99">>, Result).

test_boolean_value() ->
    Template = <<"Show: {{flag}}">>,
    Variables = #{<<"flag">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Show: true">>, Result).

test_floating_point_value() ->
    Template = <<"{{value}}">>,
    Variables = #{<<"value">> => 3.14159},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"3.14159">>, Result).

test_negative_integer_value() ->
    Template = <<"{{value}}">>,
    Variables = #{<<"value">> => -42},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"-42">>, Result).

%%====================================================================
%% Section Rendering Tests
%%====================================================================

section_rendering_test_() ->
    [
        ?_test(test_section_truthy()),
        ?_test(test_section_falsy()),
        ?_test(test_section_list()),
        ?_test(test_inverted_section_truthy()),
        ?_test(test_inverted_section_falsy()),
        ?_test(test_inverted_section_empty_list())
    ].

test_section_truthy() ->
    Template = <<"{{#show}}Display this{{/show}}">>,
    Variables = #{<<"show">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Display this">>, Result).

test_section_falsy() ->
    Template = <<"{{#show}}Display this{{/show}}">>,
    Variables = #{<<"show">> => false},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

test_section_list() ->
    Template = <<"{{#items}}- {{name}}\n{{/items}}">>,
    Variables = #{<<"items">> => [
        #{<<"name">> => <<"Item 1">>},
        #{<<"name">> => <<"Item 2">>},
        #{<<"name">> => <<"Item 3">>}
    ]},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"- Item 1\n- Item 2\n- Item 3\n">>, Result).

test_inverted_section_truthy() ->
    Template = <<"{{^hide}}Hidden is false{{/hide}}">>,
    Variables = #{<<"hide">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

test_inverted_section_falsy() ->
    Template = <<"{{^hide}}Hidden is false{{/hide}}">>,
    Variables = #{<<"hide">> => false},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hidden is false">>, Result).

test_inverted_section_empty_list() ->
    Template = <<"{{^items}}No items available{{/items}}">>,
    Variables = #{<<"items">> => []},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"No items available">>, Result).

%%====================================================================
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_whitespace_handling()),
        ?_test(test_empty_template()),
        ?_test(test_no_variables_template()),
        ?_test(test_multiple_sections()),
        ?_test(test_mixed_content())
    ].

test_whitespace_handling() ->
    Template = <<"  Hello  ,  {{ name }}  !  ">>,
    Variables = #{<<"name">> => <<"World">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"  Hello  ,  World  !  ">>, Result).

test_empty_template() ->
    Template = <<>>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

test_no_variables_template() ->
    Template = <<"Hello, World!">>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, World!">>, Result).

test_multiple_sections() ->
    Template = <<"{{#a}}A{{/a}}{{#b}}B{{/b}}{{#c}}C{{/c}}">>,
    Variables = #{<<"a">> => true, <<"b">> => true, <<"c">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"ABC">>, Result).

test_mixed_content() ->
    Template = <<"Start {{#items}}- {{name}} {{/items}}End">>,
    Variables = #{<<"items">> => [#{<<"name">> => <<"A">>}, #{<<"name">> => <<"B">>}]},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Start - A - B End">>, Result).

%%====================================================================
%% Compile and Render Tests
%%====================================================================

compile_and_render_test_() ->
    [
        ?_test(test_compile_and_render_separate())
    ].

test_compile_and_render_separate() ->
    Template = <<"Hello, {{name}}!">>,
    {ok, Compiled} = erlmcp_prompt_template:compile(Template),
    Variables1 = #{<<"name">> => <<"Alice">>},
    {ok, Result1} = erlmcp_prompt_template:render(Compiled, Variables1),
    ?assertEqual(<<"Hello, Alice!">>, Result1),
    Variables2 = #{<<"name">> => <<"Bob">>},
    {ok, Result2} = erlmcp_prompt_template:render(Compiled, Variables2),
    ?assertEqual(<<"Hello, Bob!">>, Result2).

%%====================================================================
%% Variable Name Tests
%%====================================================================

variable_name_tests_test_() ->
    [
        ?_test(test_valid_variable_name_with_underscores()),
        ?_test(test_valid_variable_name_starting_with_underscore()),
        ?_test(test_valid_variable_name_with_numbers())
    ].

test_valid_variable_name_with_underscores() ->
    Template = <<"{{my_variable_name}}">>,
    Variables = #{<<"my_variable_name">> => <<"value">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"value">>, Result).

test_valid_variable_name_starting_with_underscore() ->
    Template = <<"{{_private}}">>,
    Variables = #{<<"_private">> => <<"secret">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"secret">>, Result).

test_valid_variable_name_with_numbers() ->
    Template = <<"{{var123}}">>,
    Variables = #{<<"var123">> => <<"numeric">>},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"numeric">>, Result).

%%====================================================================
%% Undefined Variable Tests
%%====================================================================

undefined_variable_tests_test_() ->
    [
        ?_test(test_inverted_section_with_undefined_variable()),
        ?_test(test_section_with_undefined_variable())
    ].

test_inverted_section_with_undefined_variable() ->
    Template = <<"{{^missing}}Not found{{/missing}}">>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Not found">>, Result).

test_section_with_undefined_variable() ->
    Template = <<"{{#missing}}content{{/missing}}">>,
    Variables = #{},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<>>, Result).

%%====================================================================
%% Boolean Values Tests
%%====================================================================

boolean_values_test_test_() ->
    [
        ?_test(test_boolean_true_value()),
        ?_test(test_boolean_false_value())
    ].

test_boolean_true_value() ->
    Template = <<"{{flag}}">>,
    Variables = #{<<"flag">> => true},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"true">>, Result).

test_boolean_false_value() ->
    Template = <<"{{flag}}">>,
    Variables = #{<<"flag">> => false},
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"false">>, Result).
