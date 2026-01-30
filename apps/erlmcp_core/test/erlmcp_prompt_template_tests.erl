-module(erlmcp_prompt_template_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Ensure bbmustache is available
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Basic Rendering Tests
%%====================================================================

render_simple_variable_test() ->
    Template = <<"Hello {{name}}!">>,
    Variables = #{<<"name">> => <<"World">>},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Hello World!">>, Result).

render_multiple_variables_test() ->
    Template = <<"{{greeting}} {{name}}, welcome to {{place}}!">>,
    Variables = #{
        <<"greeting">> => <<"Hello">>,
        <<"name">> => <<"Alice">>,
        <<"place">> => <<"Wonderland">>
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Hello Alice, welcome to Wonderland!">>, Result).

render_no_variables_test() ->
    Template = <<"Hello World!">>,
    Variables = #{},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Hello World!">>, Result).

render_empty_template_test() ->
    Template = <<"">>,
    Variables = #{<<"name">> => <<"World">>},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"">>, Result).

%%====================================================================
%% Section Rendering Tests (Mustache Sections)
%%====================================================================

render_section_with_list_test() ->
    Template = <<"Users: {{#users}}{{name}}, {{/users}}">>,
    Variables = #{
        <<"users">> => [
            #{<<"name">> => <<"Alice">>},
            #{<<"name">> => <<"Bob">>},
            #{<<"name">> => <<"Charlie">>}
        ]
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Users: Alice, Bob, Charlie, ">>, Result).

render_section_with_boolean_true_test() ->
    Template = <<"{{#show}}This is visible{{/show}}">>,
    Variables = #{<<"show">> => true},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"This is visible">>, Result).

render_section_with_boolean_false_test() ->
    Template = <<"{{#show}}This is hidden{{/show}}">>,
    Variables = #{<<"show">> => false},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"">>, Result).

render_inverted_section_test() ->
    Template = <<"{{^empty}}Not empty{{/empty}}">>,
    Variables = #{<<"empty">> => false},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Not empty">>, Result).

render_inverted_section_with_empty_list_test() ->
    Template = <<"{{^items}}No items{{/items}}">>,
    Variables = #{<<"items">> => []},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"No items">>, Result).

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

render_missing_variable_test() ->
    Template = <<"Hello {{name}}!">>,
    Variables = #{},
    % bbmustache renders missing variables as empty string by default
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Hello !">>, Result).

render_with_special_characters_test() ->
    Template = <<"Message: {{msg}}">>,
    Variables = #{<<"msg">> => <<"<script>alert('XSS')</script>">>},
    Result = erlmcp_prompt_template:render(Template, Variables),
    % Our template engine doesn't escape (escape_fun returns value as-is)
    ?assertEqual(<<"Message: <script>alert('XSS')</script>">>, Result).

render_with_unicode_test() ->
    Template = <<"Hello {{name}}! {{emoji}}">>,
    Variables = #{
        <<"name">> => <<"世界"/utf8>>,
        <<"emoji">> => <<"😊"/utf8>>
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Hello 世界! 😊"/utf8>>, Result).

render_with_numeric_values_test() ->
    Template = <<"The answer is {{answer}}">>,
    Variables = #{<<"answer">> => 42},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"The answer is 42">>, Result).

render_with_float_values_test() ->
    Template = <<"Pi is approximately {{pi}}">>,
    Variables = #{<<"pi">> => 3.14159},
    Result = erlmcp_prompt_template:render(Template, Variables),
    % Float rendering might vary, so we check it contains the value
    ?assert(binary:match(Result, <<"3.14">>) =/= nomatch).

render_malformed_template_test() ->
    Template = <<"Hello {{name">>,  % Missing closing braces
    Variables = #{<<"name">> => <<"World">>},
    % bbmustache should handle this gracefully or error
    % Depending on implementation, this might return as-is or error
    ?assertError({template_render_error, _}, erlmcp_prompt_template:render(Template, Variables)).

%%====================================================================
%% Safe Rendering Tests
%%====================================================================

render_safe_success_test() ->
    Template = <<"Hello {{name}}!">>,
    Variables = #{<<"name">> => <<"World">>},
    Result = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertMatch({ok, <<"Hello World!">>}, Result).

render_safe_error_test() ->
    Template = <<"Hello {{name">>,  % Malformed
    Variables = #{<<"name">> => <<"World">>},
    Result = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Validation Tests
%%====================================================================

validate_valid_template_test() ->
    Template = <<"Hello {{name}}, welcome to {{place}}!">>,
    Result = erlmcp_prompt_template:validate(Template),
    ?assertEqual(ok, Result).

validate_template_with_sections_test() ->
    Template = <<"{{#users}}Name: {{name}}{{/users}}">>,
    Result = erlmcp_prompt_template:validate(Template),
    ?assertEqual(ok, Result).

validate_empty_template_test() ->
    Template = <<"">>,
    Result = erlmcp_prompt_template:validate(Template),
    ?assertEqual(ok, Result).

validate_no_variables_template_test() ->
    Template = <<"This is just plain text">>,
    Result = erlmcp_prompt_template:validate(Template),
    ?assertEqual(ok, Result).

validate_malformed_template_test() ->
    Template = <<"Hello {{name">>,  % Missing closing braces
    Result = erlmcp_prompt_template:validate(Template),
    ?assertMatch({error, {parse_error, _}}, Result).

validate_unclosed_section_test() ->
    Template = <<"{{#section}}Content">>,  % Missing {{/section}}
    Result = erlmcp_prompt_template:validate(Template),
    ?assertMatch({error, {parse_error, _}}, Result).

%%====================================================================
%% Template Syntax Detection Tests
%%====================================================================

has_template_syntax_with_variables_test() ->
    Text = <<"Hello {{name}}!">>,
    ?assert(erlmcp_prompt_template:has_template_syntax(Text)).

has_template_syntax_with_sections_test() ->
    Text = <<"{{#items}}Item{{/items}}">>,
    ?assert(erlmcp_prompt_template:has_template_syntax(Text)).

has_template_syntax_no_syntax_test() ->
    Text = <<"This is plain text">>,
    ?assertNot(erlmcp_prompt_template:has_template_syntax(Text)).

has_template_syntax_partial_syntax_test() ->
    % Has {{ but no }}
    Text = <<"Hello {{name">>,
    ?assert(erlmcp_prompt_template:has_template_syntax(Text)).

has_template_syntax_empty_test() ->
    Text = <<"">>,
    ?assertNot(erlmcp_prompt_template:has_template_syntax(Text)).

has_template_syntax_string_test() ->
    % Test with string (list) instead of binary
    Text = "Hello {{name}}!",
    ?assert(erlmcp_prompt_template:has_template_syntax(Text)).

%%====================================================================
%% Complex Template Tests
%%====================================================================

render_nested_sections_test() ->
    Template = <<"{{#users}}User: {{name}}{{#tags}}, Tag: {{.}}{{/tags}}. {{/users}}">>,
    Variables = #{
        <<"users">> => [
            #{
                <<"name">> => <<"Alice">>,
                <<"tags">> => [<<"admin">>, <<"dev">>]
            },
            #{
                <<"name">> => <<"Bob">>,
                <<"tags">> => [<<"user">>]
            }
        ]
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    % Should render both users with their tags
    ?assert(binary:match(Result, <<"Alice">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Bob">>) =/= nomatch),
    ?assert(binary:match(Result, <<"admin">>) =/= nomatch).

render_code_review_prompt_test() ->
    % Real-world example: code review prompt template
    Template = <<"You are reviewing {{language}} code in {{style}} style. "
                 "Please review the following code and provide feedback:\n\n"
                 "{{#rules}}Rule: {{rule}}\n{{/rules}}">>,
    Variables = #{
        <<"language">> => <<"Erlang">>,
        <<"style">> => <<"OTP">>,
        <<"rules">> => [
            #{<<"rule">> => <<"Check for proper gen_server callbacks">>},
            #{<<"rule">> => <<"Ensure supervision tree is correct">>},
            #{<<"rule">> => <<"Verify error handling">>}
        ]
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assert(binary:match(Result, <<"Erlang">>) =/= nomatch),
    ?assert(binary:match(Result, <<"OTP">>) =/= nomatch),
    ?assert(binary:match(Result, <<"gen_server">>) =/= nomatch).

render_essay_prompt_test() ->
    % Example from the protocol docs
    Template = <<"Write a {{style}} essay about {{topic}}">>,
    Variables = #{
        <<"topic">> => <<"climate change">>,
        <<"style">> => <<"formal">>
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Write a formal essay about climate change">>, Result).

%%====================================================================
%% Integration Tests (Template + Arguments)
%%====================================================================

render_with_mixed_types_test() ->
    Template = <<"Name: {{name}}, Age: {{age}}, Active: {{active}}">>,
    Variables = #{
        <<"name">> => <<"Alice">>,
        <<"age">> => 30,
        <<"active">> => true
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Name: Alice, Age: 30, Active: true">>, Result).

render_with_nested_maps_test() ->
    % Note: bbmustache supports dot notation for nested access
    Template = <<"User: {{user.name}}, Email: {{user.email}}">>,
    Variables = #{
        <<"user">> => #{
            <<"name">> => <<"Bob">>,
            <<"email">> => <<"bob@example.com">>
        }
    },
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"User: Bob, Email: bob@example.com">>, Result).

%%====================================================================
%% String vs Binary Tests
%%====================================================================

render_string_template_test() ->
    % Test with string (list) template
    Template = "Hello {{name}}!",
    Variables = #{<<"name">> => <<"World">>},
    Result = erlmcp_prompt_template:render(Template, Variables),
    ?assertEqual(<<"Hello World!">>, Result).

validate_string_template_test() ->
    Template = "Hello {{name}}!",
    Result = erlmcp_prompt_template:validate(Template),
    ?assertEqual(ok, Result).
