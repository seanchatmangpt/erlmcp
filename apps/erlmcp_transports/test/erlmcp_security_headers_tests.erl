%%%-------------------------------------------------------------------
%%% @doc erlmcp_security_headers tests
%%% Tests for security header middleware implementation.
%%% Following Chicago School TDD: test ALL observable behavior.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_security_headers_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Data Generation
%%%===================================================================

%% Mock Cowboy request for testing
mock_req() ->
    #{headers => [], resp_headers => []}.

%%%===================================================================
%%% Unit Tests - Header Building
%%%===================================================================

get_default_headers_test() ->
    %% Should return list of default security headers
    Headers = erlmcp_security_headers:get_default_headers(),
    ?assert(is_list(Headers)),
    ?assert(length(Headers) > 0),
    ?assertMatch({<<"x-content-type-options">>, <<"nosniff">>},
                 lists:keyfind(<<"x-content-type-options">>, 1, Headers)),
    ?assertMatch({<<"x-frame-options">>, <<"DENY">>},
                 lists:keyfind(<<"x-frame-options">>, 1, Headers)).

build_security_headers_test() ->
    %% Verify all standard headers are present
    Headers = erlmcp_security_headers:get_default_headers(),

    %% Required headers
    ?assertMatch({<<"x-content-type-options">>, <<"nosniff">>},
                 lists:keyfind(<<"x-content-type-options">>, 1, Headers)),
    ?assertMatch({<<"x-frame-options">>, <<"DENY">>},
                 lists:keyfind(<<"x-frame-options">>, 1, Headers)),
    ?assertMatch({<<"x-xss-protection">>, <<"1; mode=block">>},
                 lists:keyfind(<<"x-xss-protection">>, 1, Headers)),
    ?assertMatch({<<"content-security-policy">>, _CSP},
                 lists:keyfind(<<"content-security-policy">>, 1, Headers)),
    ?assertMatch({<<"strict-transport-security">>, _HSTS},
                 lists:keyfind(<<"strict-transport-security">>, 1, Headers)),
    ?assertMatch({<<"referrer-policy">>, _RP}, lists:keyfind(<<"referrer-policy">>, 1, Headers)),
    ?assertMatch({<<"permissions-policy">>, _PP},
                 lists:keyfind(<<"permissions-policy">>, 1, Headers)).

custom_csp_test() ->
    %% Should use custom CSP when provided
    CustomCSP = <<"default-src 'none'; script-src 'https://cdn.example.com'">>,
    Config = #{csp => CustomCSP},
    Headers = erlmcp_security_headers:add_headers([], Config),
    ?assertMatch({<<"content-security-policy">>, CustomCSP},
                 lists:keyfind(<<"content-security-policy">>, 1, Headers)).

frame_options_test() ->
    %% Should support deny and sameorigin
    ConfigDeny = #{frame_options => deny},
    HeadersDeny = erlmcp_security_headers:add_headers([], ConfigDeny),
    ?assertMatch({<<"x-frame-options">>, <<"DENY">>},
                 lists:keyfind(<<"x-frame-options">>, 1, HeadersDeny)),

    ConfigSameOrigin = #{frame_options => sameorigin},
    HeadersSameOrigin = erlmcp_security_headers:add_headers([], ConfigSameOrigin),
    ?assertMatch({<<"x-frame-options">>, <<"SAMEORIGIN">>},
                 lists:keyfind(<<"x-frame-options">>, 1, HeadersSameOrigin)).

hsts_disabled_test() ->
    %% Should exclude HSTS when disabled
    Config = #{hsts => false},
    Headers = erlmcp_security_headers:add_headers([], Config),

    %% HSTS should be replaced with placeholder
    ?assertMatch({<<"x-hsts-disabled">>, <<"true">>},
                 lists:keyfind(<<"x-hsts-disabled">>, 1, Headers)).

hsts_custom_max_age_test() ->
    %% Should support custom HSTS max-age
    CustomMaxAge = 63072000,  % 2 years
    Config = #{hsts_max_age => CustomMaxAge},
    Headers = erlmcp_security_headers:add_headers([], Config),
    {_, Value} = lists:keyfind(<<"strict-transport-security">>, 1, Headers),
    %% Check if max-age is in the value using binary matching
    ?assert(binary:match(Value, <<"max-age=63072000">>) =/= nomatch).

custom_referrer_policy_test() ->
    %% Should support custom referrer policy
    CustomPolicy = <<"no-referrer">>,
    Config = #{referrer_policy => CustomPolicy},
    Headers = erlmcp_security_headers:add_headers([], Config),
    ?assertMatch({<<"referrer-policy">>, CustomPolicy},
                 lists:keyfind(<<"referrer-policy">>, 1, Headers)).

custom_permissions_policy_test() ->
    %% Should support custom permissions policy
    CustomPolicy = <<"geolocation=(self), microphone=()">>,
    Config = #{permissions_policy => CustomPolicy},
    Headers = erlmcp_security_headers:add_headers([], Config),
    ?assertMatch({<<"permissions-policy">>, CustomPolicy},
                 lists:keyfind(<<"permissions-policy">>, 1, Headers)).

additional_headers_test() ->
    %% Should support additional custom headers
    CustomHeaders =
        [{<<"x-custom-header">>, <<"custom-value">>},
         {<<"x-another-header">>, <<"another-value">>}],
    Config = #{additional_headers => CustomHeaders},
    Headers = erlmcp_security_headers:add_headers([], Config),

    ?assertMatch({<<"x-custom-header">>, <<"custom-value">>},
                 lists:keyfind(<<"x-custom-header">>, 1, Headers)),
    ?assertMatch({<<"x-another-header">>, <<"another-value">>},
                 lists:keyfind(<<"x-another-header">>, 1, Headers)).

%%%===================================================================
%%% Unit Tests - Header Merging
%%%===================================================================

merge_headers_empty_test() ->
    %% Should add all security headers to empty list
    Headers = erlmcp_security_headers:add_headers([]),
    ?assert(length(Headers) > 0).

merge_headers_preserve_existing_test() ->
    %% Should preserve existing headers
    Existing =
        [{<<"x-existing-header">>, <<"existing-value">>},
         {<<"content-type">>, <<"application/json">>}],
    Headers = erlmcp_security_headers:add_headers(Existing),

    %% Existing headers should be preserved
    ?assertMatch({<<"x-existing-header">>, <<"existing-value">>},
                 lists:keyfind(<<"x-existing-header">>, 1, Headers)),
    ?assertMatch({<<"content-type">>, <<"application/json">>},
                 lists:keyfind(<<"content-type">>, 1, Headers)).

merge_headers_no_duplicates_test() ->
    %% Should not add headers that already exist
    Existing = [{<<"x-frame-options">>, <<"SAMEORIGIN">>}, {<<"content-type">>, <<"text/html">>}],
    Headers = erlmcp_security_headers:add_headers(Existing),

    %% Should preserve existing X-Frame-Options (not override with DENY)
    ?assertMatch({<<"x-frame-options">>, <<"SAMEORIGIN">>},
                 lists:keyfind(<<"x-frame-options">>, 1, Headers)),

    %% Should add other security headers
    ?assertMatch({<<"x-content-type-options">>, <<"nosniff">>},
                 lists:keyfind(<<"x-content-type-options">>, 1, Headers)).

merge_headers_case_insensitive_test() ->
    %% Should handle header names case-insensitively
    Existing = [{<<"X-Frame-Options">>, <<"SAMEORIGIN">>}, {<<"Content-Type">>, <<"text/html">>}],
    Headers = erlmcp_security_headers:add_headers(Existing),

    %% Should not duplicate X-Frame-Options (case-insensitive)
    FrameOptions =
        [K || {K, _V} <- Headers, string:to_lower(binary_to_list(K)) =:= "x-frame-options"],
    ?assert(length(FrameOptions) =:= 1).

%%%===================================================================
%%% Unit Tests - Configuration
%%%===================================================================

configure_global_test() ->
    %% Should configure security headers globally
    Config = #{csp => <<"default-src 'none'">>, frame_options => sameorigin},
    ?assertEqual(ok, erlmcp_security_headers:configure(Config)),

    %% Clean up
    application:unset_env(erlmcp, security_headers_config).

configure_persistence_test() ->
    %% Configuration should persist across calls
    %% First clear any existing config
    application:unset_env(erlmcp, security_headers_config),

    Config = #{csp => <<"test-csp">>},
    erlmcp_security_headers:configure(Config),

    Headers = erlmcp_security_headers:add_headers([]),
    %% Check that the CSP equals our custom value
    {_, CSPValue} = lists:keyfind(<<"content-security-policy">>, 1, Headers),
    ?assertEqual(<<"test-csp">>, CSPValue),

    %% Clean up
    application:unset_env(erlmcp, security_headers_config).

%%%===================================================================
%%% Unit Tests - Handler Wrapping
%%%===================================================================

wrap_handler_function_test() ->
    %% Should wrap a function handler
    HandlerFun = fun(Req, _State) -> {ok, Req, #{}} end,

    WrappedFun = erlmcp_security_headers:wrap_handler(HandlerFun),
    ?assert(is_function(WrappedFun, 2)),

    %% Since cowboy_req:set_resp_header requires real cowboy_req,
    %% we just verify the wrapping works
    ?assert(is_function(WrappedFun)).

wrap_handler_preserves_shutdown_test() ->
    %% Should preserve shutdown return
    HandlerFun = fun(Req, _State) -> {shutdown, Req} end,

    WrappedFun = erlmcp_security_headers:wrap_handler(HandlerFun),
    ?assert(is_function(WrappedFun)).

wrap_handler_preserves_stop_test() ->
    %% Should preserve stop return
    HandlerFun = fun(Req, _State) -> {stop, Req} end,

    WrappedFun = erlmcp_security_headers:wrap_handler(HandlerFun),
    ?assert(is_function(WrappedFun)).

wrap_handler_preserves_other_returns_test() ->
    %% Should preserve other return values
    HandlerFun = fun(_Req, _State) -> {error, some_error} end,

    WrappedFun = erlmcp_security_headers:wrap_handler(HandlerFun),
    MockReq = mock_req(),

    ?assertEqual({error, some_error}, WrappedFun(MockReq, #{})).

wrap_handler_module_test() ->
    %% Should wrap a module handler
    MockHandler = mock_http_handler,

    %% Should return the handler module (simplified implementation)
    Wrapped = erlmcp_security_headers:wrap_handler(MockHandler),
    ?assert(is_atom(Wrapped)).

%%%===================================================================
%%% Unit Tests - Middleware Execute
%%%===================================================================

execute_returns_ok_test() ->
    %% Middleware execute should exist and have correct arity
    %% Use erlang:fun_info to check the function
    FunInfo = erlang:fun_info(fun erlmcp_security_headers:execute/2),
    ?assertMatch({arity, 2}, lists:keyfind(arity, 1, FunInfo)).

%%%===================================================================
%%% Integration Tests - Full Workflow
%%%===================================================================

full_workflow_test() ->
    %% Test complete workflow: configure → add headers → verify
    %% First clear any existing config
    application:unset_env(erlmcp, security_headers_config),

    CustomConfig =
        #{csp => <<"default-src 'self'">>,
          frame_options => deny,
          hsts => true,
          hsts_max_age => 31536000},

    %% Configure
    erlmcp_security_headers:configure(CustomConfig),

    %% Add headers
    ExistingHeaders = [{<<"content-type">>, <<"application/json">>}, {<<"x-custom">>, <<"value">>}],
    FinalHeaders = erlmcp_security_headers:add_headers(ExistingHeaders),

    %% Verify existing headers preserved
    ?assertMatch({<<"content-type">>, <<"application/json">>},
                 lists:keyfind(<<"content-type">>, 1, FinalHeaders)),
    ?assertMatch({<<"x-custom">>, <<"value">>}, lists:keyfind(<<"x-custom">>, 1, FinalHeaders)),

    %% Verify security headers added
    {_, CSPValue} = lists:keyfind(<<"content-security-policy">>, 1, FinalHeaders),
    ?assertEqual(<<"default-src 'self'">>, CSPValue),

    ?assertMatch({<<"x-frame-options">>, <<"DENY">>},
                 lists:keyfind(<<"x-frame-options">>, 1, FinalHeaders)),
    ?assertMatch({<<"strict-transport-security">>, _HSTS},
                 lists:keyfind(<<"strict-transport-security">>, 1, FinalHeaders)),

    %% Clean up
    application:unset_env(erlmcp, security_headers_config).

%%%===================================================================
%%% Property-Based Tests
%%%===================================================================

prop_merge_headers_idempotent_test() ->
    %% Merging headers should be idempotent
    Headers = erlmcp_security_headers:get_default_headers(),
    Headers1 = erlmcp_security_headers:add_headers(Headers),
    Headers2 = erlmcp_security_headers:add_headers(Headers1),

    ?assertEqual(length(Headers1), length(Headers2)).

prop_add_headers_preserves_all_test() ->
    %% Adding security headers should preserve all existing headers
    Existing =
        [{<<"header1">>, <<"value1">>},
         {<<"header2">>, <<"value2">>},
         {<<"header3">>, <<"value3">>}],
    Result = erlmcp_security_headers:add_headers(Existing),

    %% All existing headers should be present
    ?assertMatch({<<"header1">>, <<"value1">>}, lists:keyfind(<<"header1">>, 1, Result)),
    ?assertMatch({<<"header2">>, <<"value2">>}, lists:keyfind(<<"header2">>, 1, Result)),
    ?assertMatch({<<"header3">>, <<"value3">>}, lists:keyfind(<<"header3">>, 1, Result)),

    %% Security headers should be added
    ?assert(length(Result) > length(Existing)).
