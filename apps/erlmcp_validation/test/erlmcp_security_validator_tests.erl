%%%-------------------------------------------------------------------
%%% @doc Security Validator Tests
%%%
%%% Tests all 22 security validation checks:
%%% - 4 Authentication checks
%%% - 5 Input validation checks
%%% - 4 Secret management checks
%%% - 4 JWT checks
%%% - 3 Rate limiting checks
%%% - 3 CORS checks
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp processes from erlmcp_test_helpers.
%%% Tests observable behavior through API calls only.
%%% NO internal state inspection or mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_security_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Teardown
%%%===================================================================

setup() ->
    case erlmcp_security_validator:start_link() of
        {ok, Pid} ->
            Pid;
        {error, {already_started, Pid}} ->
            Pid
    end.

cleanup(_Pid) ->
    %% Don't stop the server between tests to avoid restart issues
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Helper to find a check by name (works with both map and tuple formats)
find_check(Checks, CheckName) ->
    lists:search(fun(Check) ->
                    case Check of
                        #{name := Name} ->
                            Name =:= CheckName;
                        {CheckName, _} ->
                            true;
                        _ ->
                            false
                    end
                 end,
                 Checks).

%% Helper to get status from a check (works with both map and tuple formats)
get_status(Check) ->
    case Check of
        #{status := S} ->
            S;
        {_, #{status := S}} ->
            S
    end.

%%%===================================================================
%%% Authentication Tests (4 checks)
%%%===================================================================

authentication_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_auth_mechanism/1,
      fun test_token_handling/1,
      fun test_session_management/1,
      fun test_authorization/1]}.

test_auth_mechanism(_Pid) ->
    fun() ->
       %% Test auth mechanism via observable behavior
       Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
       Checks = maps:get(checks, Result),
       AuthCheck = find_check(Checks, auth_mechanism),
       ?assertNotEqual(false, AuthCheck),
       {value, CheckValue} = AuthCheck,
       Status = get_status(CheckValue),
       ?assert(Status =:= passed orelse Status =:= warning)
    end.

test_token_handling(_Pid) ->
    fun() ->
       %% Test token handling via API
       Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
       Checks = maps:get(checks, Result),
       TokenCheck = find_check(Checks, token_handling),
       ?assertNotEqual(false, TokenCheck)
    end.

test_session_management(_Pid) ->
    fun() ->
       %% Test session management via API
       Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
       Checks = maps:get(checks, Result),
       SessionCheck = find_check(Checks, session_management),
       ?assertNotEqual(false, SessionCheck)
    end.

test_authorization(_Pid) ->
    fun() ->
       %% Test authorization via API
       Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
       Checks = maps:get(checks, Result),
       AuthzCheck = find_check(Checks, authorization),
       ?assertNotEqual(false, AuthzCheck)
    end.

%%%===================================================================
%%% Input Validation Tests (5 checks)
%%%===================================================================

input_validation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_json_schema_validation/1,
      fun test_parameter_sanitization/1,
      fun test_sql_injection_prevention/1,
      fun test_xss_prevention/1,
      fun test_path_traversal_prevention/1]}.

test_json_schema_validation(_Pid) ->
    %% Test JSON schema validation via API
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SchemaCheck = find_check(Checks, json_schema_validation),
    ?assertNotEqual(false, SchemaCheck).

test_parameter_sanitization(_Pid) ->
    %% Test parameter sanitization via API
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SanitizeCheck = find_check(Checks, parameter_sanitization),
    ?assertNotEqual(false, SanitizeCheck).

test_sql_injection_prevention(_Pid) ->
    %% Test SQL injection prevention via API
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SqlCheck = find_check(Checks, sql_injection_prevention),
    ?assertNotEqual(false, SqlCheck).

test_xss_prevention(_Pid) ->
    %% Test XSS prevention via API
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    XssCheck = find_check(Checks, xss_prevention),
    ?assertNotEqual(false, XssCheck).

test_path_traversal_prevention(_Pid) ->
    %% Test path traversal prevention via API
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    PathCheck = find_check(Checks, path_traversal_prevention),
    ?assertNotEqual(false, PathCheck).

%%%===================================================================
%%% Secret Management Tests (4 checks)
%%%===================================================================

secret_management_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_no_hardcoded_secrets/1,
      fun test_env_variable_usage/1,
      fun test_secret_encryption/1,
      fun test_key_rotation/1]}.

test_no_hardcoded_secrets(_Pid) ->
    %% Test no hardcoded secrets via observable behavior
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SecretsCheck = find_check(Checks, no_hardcoded_secrets),
    ?assertNotEqual(false, SecretsCheck),
    {value, CheckValue} = SecretsCheck,
    Status = get_status(CheckValue),
    ?assertMatch(passed, Status).

test_env_variable_usage(_Pid) ->
    %% Test env variable usage via API
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    EnvCheck = find_check(Checks, env_variable_usage),
    ?assertNotEqual(false, EnvCheck).

test_secret_encryption(_Pid) ->
    %% Test secret encryption via API
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    EncryptCheck = find_check(Checks, secret_encryption),
    ?assertNotEqual(false, EncryptCheck).

test_key_rotation(_Pid) ->
    %% Test key rotation via API
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    RotationCheck = find_check(Checks, key_rotation),
    ?assertNotEqual(false, RotationCheck).

%%%===================================================================
%%% JWT Tests (4 checks)
%%%===================================================================

jwt_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_jwt_structure/1,
      fun test_jwt_signature/1,
      fun test_jwt_validation/1,
      fun test_jwt_expiration/1]}.

test_jwt_structure(_Pid) ->
    %% Test JWT structure via API
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    StructureCheck = find_check(Checks, jwt_structure),
    ?assertNotEqual(false, StructureCheck).

test_jwt_signature(_Pid) ->
    %% Test JWT signature via API
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SigCheck = find_check(Checks, jwt_signature),
    ?assertNotEqual(false, SigCheck).

test_jwt_validation(_Pid) ->
    %% Test JWT validation via API
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    ValidationCheck = find_check(Checks, jwt_validation),
    ?assertNotEqual(false, ValidationCheck).

test_jwt_expiration(_Pid) ->
    %% Test JWT expiration via API
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    ExpCheck = find_check(Checks, jwt_expiration),
    ?assertNotEqual(false, ExpCheck).

%%%===================================================================
%%% Rate Limiting Tests (3 checks)
%%%===================================================================

rate_limiting_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_rate_limit_configured/1,
      fun test_rate_limit_enforcement/1,
      fun test_rate_limit_bypass/1]}.

test_rate_limit_configured(_Pid) ->
    %% Test rate limiting configured via API
    Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    ConfigCheck = find_check(Checks, rate_limit_configured),
    ?assertNotEqual(false, ConfigCheck).

test_rate_limit_enforcement(_Pid) ->
    %% Test rate limiting enforcement via API
    Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    EnforceCheck = find_check(Checks, rate_limit_enforcement),
    ?assertNotEqual(false, EnforceCheck).

test_rate_limit_bypass(_Pid) ->
    %% Test rate limiting bypass protection via API
    Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    BypassCheck = find_check(Checks, rate_limit_bypass),
    ?assertNotEqual(false, BypassCheck).

%%%===================================================================
%%% CORS Tests (3 checks)
%%%===================================================================

cors_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_cors_headers/1, fun test_origin_validation/1, fun test_cors_policies/1]}.

test_cors_headers(_Pid) ->
    %% Test CORS headers via API
    Result = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    HeadersCheck = find_check(Checks, cors_headers),
    ?assertNotEqual(false, HeadersCheck).

test_origin_validation(_Pid) ->
    %% Test origin validation via API
    Result = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    OriginCheck = find_check(Checks, origin_validation),
    ?assertNotEqual(false, OriginCheck).

test_cors_policies(_Pid) ->
    %% Test CORS policies via API
    Result = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    PoliciesCheck = find_check(Checks, cors_policies),
    ?assertNotEqual(false, PoliciesCheck).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_run_validation/1, fun test_generate_report/1, fun test_all_22_checks_present/1]}.

test_run_validation(Pid) ->
    %% Test full validation run via API
    {ok, Summary} = erlmcp_security_validator:run(erlmcp_transport_stdio),
    ?assertMatch(#{transport := erlmcp_transport_stdio}, Summary),
    ?assert(maps:is_key(compliance, Summary)),
    ?assert(maps:is_key(total_checks, Summary)).

test_generate_report(Pid) ->
    %% Test report generation via API
    {ok, _} = erlmcp_security_validator:generate_report(),
    ?assert(true).

test_all_22_checks_present(_Pid) ->
    %% Test all 22 security checks are present via API
    AuthResult = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
    ?assertEqual(4, length(maps:get(checks, AuthResult))),

    InputResult = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    ?assertEqual(5, length(maps:get(checks, InputResult))),

    SecretsResult = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    ?assertEqual(4, length(maps:get(checks, SecretsResult))),

    JwtResult = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    ?assertEqual(4, length(maps:get(checks, JwtResult))),

    RateResult = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    ?assertEqual(3, length(maps:get(checks, RateResult))),

    CorsResult = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    ?assertEqual(3, length(maps:get(checks, CorsResult))),

    %% Total: 4 + 5 + 4 + 4 + 3 + 3 = 22 checks
    ?assert(true).
