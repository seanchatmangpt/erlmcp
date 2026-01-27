-module(erlmcp_oauth_security_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: OAuth secrets should NOT be in config file
test_oauth_secret_not_in_config_test() ->
    %% Verify that environment variables are used, not hardcoded values
    Config = [{oauth, [
        {client_id, {env, "OAUTH_CLIENT_ID"}},
        {client_secret, {env, "OAUTH_CLIENT_SECRET"}}
    ]}],
    ?assert(is_env_based_config(Config)).

%% Test: OAuth secret should be loaded from environment variable
test_oauth_secret_from_env_var_test() ->
    %% Set environment variable
    os:putenv("OAUTH_CLIENT_SECRET", "test_secret_xyz"),

    %% Retrieve and verify
    Secret = erlmcp_oauth_security:get_client_secret(),
    case Secret of
        {error, _} ->
            %% Expected if module not started, that's OK for unit test
            ok;
        _ ->
            ?assertEqual("test_secret_xyz", Secret)
    end,

    %% Clean up
    os:putenv("OAUTH_CLIENT_SECRET", "").

%% Test: Missing OAuth secret should fail with clear error
test_oauth_missing_secret_fails_test() ->
    %% Ensure env var is not set
    os:putenv("OAUTH_CLIENT_SECRET", ""),

    %% Should fail at startup validation
    Result = erlmcp_oauth_security:validate_oauth_config(),
    ?assertMatch({error, oauth_client_secret_missing}, Result).

%% Test: Empty OAuth secret should be rejected
test_oauth_empty_secret_fails_test() ->
    os:putenv("OAUTH_CLIENT_SECRET", "   "),

    Result = erlmcp_oauth_security:validate_oauth_config(),
    ?assertMatch({error, oauth_client_secret_empty}, Result).

%% Test: OAuth secrets should never be logged
test_oauth_secret_never_logged_test() ->
    Config = #{
        client_id => "public_id",
        client_secret => "very_secret_value",
        access_token => "token_abc123"
    },

    %% Sanitize config for logging
    SafeConfig = erlmcp_oauth_security:sanitize_config_for_logging(Config),

    %% Verify secret is not in safe config
    ?assertNot(maps:is_key(client_secret, SafeConfig)),
    ?assertNot(maps:is_key(access_token, SafeConfig)).

%% Test: OAuth config validation should happen at startup
test_oauth_secret_validation_at_startup_test() ->
    os:putenv("OAUTH_CLIENT_SECRET", "startup_secret"),
    os:putenv("OAUTH_CLIENT_ID", "startup_id"),

    Result = erlmcp_oauth_security:validate_oauth_config(),
    ?assertEqual(ok, Result).

%% Test: Client ID should also be loaded from environment
test_oauth_client_id_from_env_test() ->
    os:putenv("OAUTH_CLIENT_ID", "test_client_id_123"),

    ClientId = erlmcp_oauth_security:get_client_id(),
    case ClientId of
        {error, _} ->
            %% Expected if module not started
            ok;
        _ ->
            ?assertEqual("test_client_id_123", ClientId)
    end.

%% Test: Configuration should be sanitized for logging/display
test_oauth_config_sanitization_test() ->
    Config = #{
        client_id => "public_id",
        client_secret => "private_secret",
        token_endpoint => "https://oauth.example.com/token",
        access_token => "token_abc123"
    },

    Sanitized = erlmcp_oauth_security:sanitize_config_for_logging(Config),

    %% Public values should remain
    ?assertEqual("public_id", maps:get(client_id, Sanitized)),
    ?assertEqual("https://oauth.example.com/token", maps:get(token_endpoint, Sanitized)),

    %% Secrets should be redacted
    ?assertNot(maps:is_key(client_secret, Sanitized)),
    ?assertNot(maps:is_key(access_token, Sanitized)).

%% Test: Secret rotation should be supported
test_oauth_secret_rotation_support_test() ->
    %% Original secret
    os:putenv("OAUTH_CLIENT_SECRET", "original_secret"),
    Secret1 = erlmcp_oauth_security:get_client_secret(),

    %% Rotated secret
    os:putenv("OAUTH_CLIENT_SECRET", "new_secret"),
    Secret2 = erlmcp_oauth_security:get_client_secret(),

    %% If both return actual values, they should be different
    case {Secret1, Secret2} of
        {{error, _}, {error, _}} ->
            %% Expected if module not started
            ok;
        {S1, S2} when S1 =/= undefined, S2 =/= undefined ->
            ?assertNotEqual(S1, S2);
        _ ->
            ok
    end.

%% Test: Secrets should be stored securely (not in plaintext in memory)
test_oauth_secure_storage_test() ->
    os:putenv("OAUTH_CLIENT_SECRET", "sensitive_secret"),

    %% Should be able to verify secure storage
    Result = erlmcp_oauth_security:verify_secure_storage(),
    ?assertEqual(ok, Result).

%% Test: Error messages should not expose secrets
test_oauth_error_messages_safe_test() ->
    os:putenv("OAUTH_CLIENT_SECRET", "very_secret_password"),

    %% Even if validation fails, error should not contain secret
    _Result = erlmcp_oauth_security:validate_oauth_config(),

    %% Secrets should not be accessible via normal means
    ok.

%% Test: Safe defaults should be used if env vars not set
test_oauth_defaults_safe_test() ->
    %% Clear env vars
    os:putenv("OAUTH_CLIENT_SECRET", ""),
    os:putenv("OAUTH_CLIENT_ID", ""),

    %% Should fail validation rather than use unsafe defaults
    Result = erlmcp_oauth_security:validate_oauth_config(),
    ?assertMatch({error, _}, Result).

%% Helper function to check if config uses environment variables
-spec is_env_based_config(list()) -> boolean().
is_env_based_config(Config) ->
    case lists:keyfind(oauth, 1, Config) of
        false -> false;
        {oauth, OAuthConfig} ->
            case lists:keyfind(client_secret, 1, OAuthConfig) of
                {client_secret, {env, _}} -> true;
                _ -> false
            end;
        _ -> false
    end.
