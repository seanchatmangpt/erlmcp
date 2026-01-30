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
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_security_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Teardown
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_security_validator:start_link(),
    Pid.

cleanup(_Pid) ->
    erlmcp_security_validator:stop().

%%%===================================================================
%%% Authentication Tests (4 checks)
%%%===================================================================

authentication_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_auth_mechanism/1,
      fun test_token_handling/1,
      fun test_session_management/1,
      fun test_authorization/1
     ]}.

test_auth_mechanism(_Pid) ->
    Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    AuthCheck = lists:keyfind(auth_mechanism, 1, Checks),
    ?assertNotEqual(false, AuthCheck),
    ?assert(maps:get(status, AuthCheck) =:= passed orelse maps:get(status, AuthCheck) =:= warning).

test_token_handling(_Pid) ->
    Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    TokenCheck = lists:keyfind(token_handling, 1, Checks),
    ?assertNotEqual(false, TokenCheck).

test_session_management(_Pid) ->
    Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SessionCheck = lists:keyfind(session_management, 1, Checks),
    ?assertNotEqual(false, SessionCheck).

test_authorization(_Pid) ->
    Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    AuthzCheck = lists:keyfind(authorization, 1, Checks),
    ?assertNotEqual(false, AuthzCheck).

%%%===================================================================
%%% Input Validation Tests (5 checks)
%%%===================================================================

input_validation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_json_schema_validation/1,
      fun test_parameter_sanitization/1,
      fun test_sql_injection_prevention/1,
      fun test_xss_prevention/1,
      fun test_path_traversal_prevention/1
     ]}.

test_json_schema_validation(_Pid) ->
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SchemaCheck = lists:keyfind(json_schema_validation, 1, Checks),
    ?assertNotEqual(false, SchemaCheck).

test_parameter_sanitization(_Pid) ->
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SanitizeCheck = lists:keyfind(parameter_sanitization, 1, Checks),
    ?assertNotEqual(false, SanitizeCheck).

test_sql_injection_prevention(_Pid) ->
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SqlCheck = lists:keyfind(sql_injection_prevention, 1, Checks),
    ?assertNotEqual(false, SqlCheck).

test_xss_prevention(_Pid) ->
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    XssCheck = lists:keyfind(xss_prevention, 1, Checks),
    ?assertNotEqual(false, XssCheck).

test_path_traversal_prevention(_Pid) ->
    Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    PathCheck = lists:keyfind(path_traversal_prevention, 1, Checks),
    ?assertNotEqual(false, PathCheck).

%%%===================================================================
%%% Secret Management Tests (4 checks)
%%%===================================================================

secret_management_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_no_hardcoded_secrets/1,
      fun test_env_variable_usage/1,
      fun test_secret_encryption/1,
      fun test_key_rotation/1
     ]}.

test_no_hardcoded_secrets(_Pid) ->
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SecretsCheck = lists:keyfind(no_hardcoded_secrets, 1, Checks),
    ?assertNotEqual(false, SecretsCheck),
    Status = maps:get(status, SecretsCheck),
    ?assertMatch(passed, Status).

test_env_variable_usage(_Pid) ->
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    EnvCheck = lists:keyfind(env_variable_usage, 1, Checks),
    ?assertNotEqual(false, EnvCheck).

test_secret_encryption(_Pid) ->
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    EncryptCheck = lists:keyfind(secret_encryption, 1, Checks),
    ?assertNotEqual(false, EncryptCheck).

test_key_rotation(_Pid) ->
    Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    RotationCheck = lists:keyfind(key_rotation, 1, Checks),
    ?assertNotEqual(false, RotationCheck).

%%%===================================================================
%%% JWT Tests (4 checks)
%%%===================================================================

jwt_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_jwt_structure/1,
      fun test_jwt_signature/1,
      fun test_jwt_validation/1,
      fun test_jwt_expiration/1
     ]}.

test_jwt_structure(_Pid) ->
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    StructureCheck = lists:keyfind(jwt_structure, 1, Checks),
    ?assertNotEqual(false, StructureCheck).

test_jwt_signature(_Pid) ->
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    SigCheck = lists:keyfind(jwt_signature, 1, Checks),
    ?assertNotEqual(false, SigCheck).

test_jwt_validation(_Pid) ->
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    ValidationCheck = lists:keyfind(jwt_validation, 1, Checks),
    ?assertNotEqual(false, ValidationCheck).

test_jwt_expiration(_Pid) ->
    Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    ExpCheck = lists:keyfind(jwt_expiration, 1, Checks),
    ?assertNotEqual(false, ExpCheck).

%%%===================================================================
%%% Rate Limiting Tests (3 checks)
%%%===================================================================

rate_limiting_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_rate_limit_configured/1,
      fun test_rate_limit_enforcement/1,
      fun test_rate_limit_bypass/1
     ]}.

test_rate_limit_configured(_Pid) ->
    Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    ConfigCheck = lists:keyfind(rate_limit_configured, 1, Checks),
    ?assertNotEqual(false, ConfigCheck).

test_rate_limit_enforcement(_Pid) ->
    Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    EnforceCheck = lists:keyfind(rate_limit_enforcement, 1, Checks),
    ?assertNotEqual(false, EnforceCheck).

test_rate_limit_bypass(_Pid) ->
    Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    BypassCheck = lists:keyfind(rate_limit_bypass, 1, Checks),
    ?assertNotEqual(false, BypassCheck).

%%%===================================================================
%%% CORS Tests (3 checks)
%%%===================================================================

cors_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_cors_headers/1,
      fun test_origin_validation/1,
      fun test_cors_policies/1
     ]}.

test_cors_headers(_Pid) ->
    Result = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    HeadersCheck = lists:keyfind(cors_headers, 1, Checks),
    ?assertNotEqual(false, HeadersCheck).

test_origin_validation(_Pid) ->
    Result = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    OriginCheck = lists:keyfind(origin_validation, 1, Checks),
    ?assertNotEqual(false, OriginCheck).

test_cors_policies(_Pid) ->
    Result = erlmcp_security_validator:validate_cors(erlmcp_transport_stdio),
    Checks = maps:get(checks, Result),
    PoliciesCheck = lists:keyfind(cors_policies, 1, Checks),
    ?assertNotEqual(false, PoliciesCheck).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_run_validation/1,
      fun test_generate_report/1,
      fun test_all_22_checks_present/1
     ]}.

test_run_validation(Pid) ->
    {ok, Summary} = erlmcp_security_validator:run(erlmcp_transport_stdio),
    ?assertMatch(#{transport := erlmcp_transport_stdio}, Summary),
    ?assert(maps:is_key(compliance, Summary)),
    ?assert(maps:is_key(total_checks, Summary)).

test_generate_report(Pid) ->
    {ok, _} = erlmcp_security_validator:generate_report(),
    ?assert(true).

test_all_22_checks_present(_Pid) ->
    % Run all validations and verify all 22 checks are present
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

    % Total: 4 + 5 + 4 + 4 + 3 + 3 = 22 checks
    ?assert(true).
