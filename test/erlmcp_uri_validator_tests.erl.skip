%%% erlmcp_uri_validator_tests.erl
%%%
%%% Comprehensive test suite for Gap #41: Resource URI Format Validation
%%%
%%% Tests validate_uri, validate_uri_scheme, validate_uri_template,
%%% URI template variable parsing and substitution.

-module(erlmcp_uri_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Validation Tests
%% ===================================================================

%% Test valid absolute URIs with various schemes
validate_uri_absolute_file_test() ->
    Uri = <<"file:///path/to/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_absolute_http_test() ->
    Uri = <<"http://example.com/api/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_absolute_https_test() ->
    Uri = <<"https://secure.example.com/api/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_absolute_custom_test() ->
    Uri = <<"custom://my-service/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_absolute_data_test() ->
    Uri = <<"data:text/plain;base64,SGVsbG8gV29ybGQ=">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

%% Test valid relative URIs
validate_uri_relative_absolute_path_test() ->
    Uri = <<"/resources/file.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_relative_dot_path_test() ->
    Uri = <<"./resources/file.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_relative_parent_path_test() ->
    Uri = <<"../resources/file.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

validate_uri_relative_simple_test() ->
    Uri = <<"resources/file.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

%% Test invalid URIs
validate_uri_empty_test() ->
    Uri = <<"">>,
    Result = erlmcp_uri_validator:validate_uri(Uri),
    ?assertMatch({error, {empty_uri, _}}, Result).

validate_uri_non_binary_test() ->
    Result = erlmcp_uri_validator:validate_uri("string_not_binary"),
    ?assertMatch({error, {invalid_uri_type, _}}, Result).

validate_uri_invalid_format_test() ->
    % Just a space, not a valid URI
    Uri = <<"   ">>,
    Result = erlmcp_uri_validator:validate_uri(Uri),
    % This should fail because it's not a valid format
    ?assertNotEqual(ok, Result).

%% ===================================================================
%% Scheme Validation Tests
%% ===================================================================

scheme_validation_file_test() ->
    Scheme = <<"file">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_scheme(Scheme)).

scheme_validation_http_test() ->
    Scheme = <<"http">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_scheme(Scheme)).

scheme_validation_https_test() ->
    Scheme = <<"https">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_scheme(Scheme)).

scheme_validation_custom_test() ->
    Scheme = <<"custom">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_scheme(Scheme)).

scheme_validation_unsupported_test() ->
    Scheme = <<"telnet">>,
    Result = erlmcp_uri_validator:validate_uri_scheme(Scheme),
    ?assertMatch({error, {unsupported_scheme, _}}, Result).

scheme_validation_empty_test() ->
    Scheme = <<"">>,
    Result = erlmcp_uri_validator:validate_uri_scheme(Scheme),
    ?assertMatch({error, {empty_scheme, _}}, Result).

%% ===================================================================
%% URI Template Tests
%% ===================================================================

template_validation_simple_test() ->
    Template = <<"/api/{id}">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_template(Template)).

template_validation_multiple_vars_test() ->
    Template = <<"/users/{user_id}/items/{item_id}">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_template(Template)).

template_validation_http_test() ->
    Template = <<"http://example.com/{path}/detail">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_template(Template)).

template_validation_file_test() ->
    Template = <<"file:///data/{name}.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_template(Template)).

template_unbalanced_braces_test() ->
    Template = <<"/api/{id">>,
    Result = erlmcp_uri_validator:validate_uri_template(Template),
    ?assertMatch({error, {unbalanced_braces, _}}, Result).

template_empty_variable_test() ->
    Template = <<"/api/{}">>,
    Result = erlmcp_uri_validator:validate_uri_template(Template),
    ?assertMatch({error, {empty_variable_name, _}}, Result).

template_invalid_variable_name_test() ->
    Template = <<"/api/{@invalid}">>,
    Result = erlmcp_uri_validator:validate_uri_template(Template),
    ?assertMatch({error, {invalid_variable_character, _}}, Result).

template_empty_test() ->
    Template = <<"">>,
    Result = erlmcp_uri_validator:validate_uri_template(Template),
    ?assertMatch({error, {empty_template, _}}, Result).

%% ===================================================================
%% Template Variable Parsing Tests
%% ===================================================================

parse_template_single_variable_test() ->
    Template = <<"/api/{id}">>,
    Variables = erlmcp_uri_validator:parse_uri_template_variables(Template),
    ?assertEqual([<<"id">>], Variables).

parse_template_multiple_variables_test() ->
    Template = <<"/users/{user_id}/posts/{post_id}/comments/{comment_id}">>,
    Variables = erlmcp_uri_validator:parse_uri_template_variables(Template),
    ?assertEqual(
        [<<"comment_id">>, <<"post_id">>, <<"user_id">>],  % Sorted
        Variables
    ).

parse_template_duplicate_variables_test() ->
    Template = <<"/api/{id}/related/{id}">>,
    Variables = erlmcp_uri_validator:parse_uri_template_variables(Template),
    ?assertEqual([<<"id">>], Variables).  % Duplicates removed

parse_template_no_variables_test() ->
    Template = <<"/api/static/path">>,
    Variables = erlmcp_uri_validator:parse_uri_template_variables(Template),
    ?assertEqual([], Variables).

parse_template_underscore_variables_test() ->
    Template = <<"/api/{user_id}/{post_id}">>,
    Variables = erlmcp_uri_validator:parse_uri_template_variables(Template),
    ?assertEqual(
        [<<"post_id">>, <<"user_id">>],
        Variables
    ).

%% ===================================================================
%% Template Variable Substitution Tests
%% ===================================================================

substitute_template_simple_test() ->
    Template = <<"/api/{id}">>,
    Variables = #{<<"id">> => <<"123">>},
    {ok, Result} = erlmcp_uri_validator:substitute_template_variables(Template, Variables),
    ?assertEqual(<<"/api/123">>, Result).

substitute_template_multiple_test() ->
    Template = <<"/users/{user_id}/posts/{post_id}">>,
    Variables = #{
        <<"user_id">> => <<"42">>,
        <<"post_id">> => <<"789">>
    },
    {ok, Result} = erlmcp_uri_validator:substitute_template_variables(Template, Variables),
    ?assertEqual(<<"/users/42/posts/789">>, Result).

substitute_template_with_special_chars_test() ->
    Template = <<"/files/{name}">>,
    Variables = #{<<"name">> => <<"my-file_name.txt">>},
    {ok, Result} = erlmcp_uri_validator:substitute_template_variables(Template, Variables),
    ?assertEqual(<<"/files/my-file_name.txt">>, Result).

substitute_template_missing_variable_test() ->
    Template = <<"/api/{id}/detail">>,
    Variables = #{},
    Result = erlmcp_uri_validator:substitute_template_variables(Template, Variables),
    ?assertMatch({error, {missing_variables, _}}, Result).

substitute_template_extra_variables_test() ->
    Template = <<"/api/{id}">>,
    Variables = #{
        <<"id">> => <<"123">>,
        <<"extra">> => <<"value">>
    },
    {ok, Result} = erlmcp_uri_validator:substitute_template_variables(Template, Variables),
    ?assertEqual(<<"/api/123">>, Result).

substitute_template_non_binary_template_test() ->
    Template = not_binary,
    Variables = #{<<"id">> => <<"123">>},
    Result = erlmcp_uri_validator:substitute_template_variables(Template, Variables),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% Scheme Detection Tests
%% ===================================================================

get_uri_scheme_http_test() ->
    Uri = <<"http://example.com/path">>,
    ?assertEqual({ok, <<"http">>}, erlmcp_uri_validator:get_uri_scheme(Uri)).

get_uri_scheme_https_test() ->
    Uri = <<"https://example.com/path">>,
    ?assertEqual({ok, <<"https">>}, erlmcp_uri_validator:get_uri_scheme(Uri)).

get_uri_scheme_file_test() ->
    Uri = <<"file:///path/to/file">>,
    ?assertEqual({ok, <<"file">>}, erlmcp_uri_validator:get_uri_scheme(Uri)).

get_uri_scheme_no_scheme_test() ->
    Uri = <<"/path/to/file">>,
    ?assertEqual(error, erlmcp_uri_validator:get_uri_scheme(Uri)).

get_uri_scheme_relative_test() ->
    Uri = <<"path/to/file">>,
    ?assertEqual(error, erlmcp_uri_validator:get_uri_scheme(Uri)).

%% ===================================================================
%% Absolute vs Relative URI Tests
%% ===================================================================

is_absolute_uri_http_test() ->
    Uri = <<"http://example.com">>,
    ?assertEqual(true, erlmcp_uri_validator:is_absolute_uri(Uri)).

is_absolute_uri_file_test() ->
    Uri = <<"file:///path">>,
    ?assertEqual(true, erlmcp_uri_validator:is_absolute_uri(Uri)).

is_absolute_uri_relative_test() ->
    Uri = <<"/path/to/file">>,
    ?assertEqual(false, erlmcp_uri_validator:is_absolute_uri(Uri)).

is_relative_uri_absolute_path_test() ->
    Uri = <<"/path/to/file">>,
    ?assertEqual(true, erlmcp_uri_validator:is_relative_uri(Uri)).

is_relative_uri_dot_path_test() ->
    Uri = <<"./path/to/file">>,
    ?assertEqual(true, erlmcp_uri_validator:is_relative_uri(Uri)).

is_relative_uri_parent_path_test() ->
    Uri = <<"../path/to/file">>,
    ?assertEqual(true, erlmcp_uri_validator:is_relative_uri(Uri)).

is_relative_uri_relative_path_test() ->
    Uri = <<"path/to/file">>,
    ?assertEqual(true, erlmcp_uri_validator:is_relative_uri(Uri)).

is_relative_uri_absolute_test() ->
    Uri = <<"http://example.com">>,
    ?assertEqual(false, erlmcp_uri_validator:is_relative_uri(Uri)).

%% ===================================================================
%% Registration Validation Tests
%% ===================================================================

validate_registration_http_test() ->
    Uri = <<"http://api.example.com/resource/123">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_resource_uri_on_registration(Uri)).

validate_registration_file_test() ->
    Uri = <<"file:///home/user/data.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_resource_uri_on_registration(Uri)).

validate_registration_relative_test() ->
    Uri = <<"./resources/data.json">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_resource_uri_on_registration(Uri)).

validate_registration_empty_test() ->
    Uri = <<"">>,
    Result = erlmcp_uri_validator:validate_resource_uri_on_registration(Uri),
    ?assertMatch({error, {empty_uri, _}}, Result).

%% ===================================================================
%% Subscription Validation Tests
%% ===================================================================

validate_subscription_http_test() ->
    Uri = <<"http://api.example.com/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_subscription_uri(Uri)).

validate_subscription_relative_test() ->
    Uri = <<"/resource/path">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_subscription_uri(Uri)).

validate_subscription_empty_test() ->
    Uri = <<"">>,
    Result = erlmcp_uri_validator:validate_subscription_uri(Uri),
    ?assertMatch({error, {empty_uri, _}}, Result).

%% ===================================================================
%% Edge Cases and Security Tests
%% ===================================================================

uri_with_special_chars_test() ->
    Uri = <<"file:///path/with-special_chars.123">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

uri_with_query_params_test() ->
    Uri = <<"http://example.com/api?key=value&other=data">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

uri_with_fragment_test() ->
    Uri = <<"http://example.com/page#section">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

uri_with_port_test() ->
    Uri = <<"http://example.com:8080/api">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

uri_with_auth_test() ->
    Uri = <<"http://user:pass@example.com/api">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

uri_with_unicode_test() ->
    Uri = <<"file:///path/to/文件.txt">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

template_complex_paths_test() ->
    Template = <<"http://example.com/api/v{version}/users/{user_id}/posts/{post_id}?filter={filter}">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_template(Template)).

template_numeric_variables_test() ->
    Template = <<"/api/{v1}/{id2}/{name3}">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri_template(Template)).

%% ===================================================================
%% Boolean Check Tests
%% ===================================================================

is_valid_uri_format_true_test() ->
    Uri = <<"http://example.com">>,
    ?assertEqual(true, erlmcp_uri_validator:is_valid_uri_format(Uri)).

is_valid_uri_format_relative_true_test() ->
    Uri = <<"/path/to/file">>,
    ?assertEqual(true, erlmcp_uri_validator:is_valid_uri_format(Uri)).

is_valid_uri_format_empty_test() ->
    Uri = <<"">>,
    ?assertEqual(false, erlmcp_uri_validator:is_valid_uri_format(Uri)).

is_valid_uri_format_invalid_test() ->
    Uri = <<"   \n">>,
    ?assertEqual(false, erlmcp_uri_validator:is_valid_uri_format(Uri)).
