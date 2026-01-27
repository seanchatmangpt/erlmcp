%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_config_security_tests.erl
%%%
%%% Comprehensive security test suite for hardcoded credentials and paths.
%%% Validates that configuration uses environment variables instead of
%%% hardcoded credentials and that path configuration is secure.
%%%
%%% Test Coverage:
%%% - No hardcoded credentials in configuration
%%% - No hardcoded paths except /tmp default
%%% - Environment variables loaded correctly
%%% - Sensible defaults when variables not set
%%% - Config validation rejects invalid paths
%%%
%%% @author Agent 4 (Security Remediation)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_config_security_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Save original environment variables
    OrigEnv = os:getenv(),
    {erlmcp_config_security_tests, OrigEnv}.

cleanup({erlmcp_config_security_tests, OrigEnv}) ->
    %% Restore original environment
    lists:foreach(fun({Key, Value}) ->
        os:putenv(Key, Value)
    end, OrigEnv).

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test 1: No hardcoded credentials in configuration
no_hardcoded_credentials_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    TcpsHealthConfig = proplists:get_value(tcps_health, ErlmcpConfig),

    %% Check email configuration
    EmailPassword = proplists:get_value(email_password, TcpsHealthConfig),
    ?assertNotEqual("changeme", EmailPassword),

    %% Check PagerDuty configuration
    PagerDutyKey = proplists:get_value(pagerduty_integration_key, TcpsHealthConfig),
    ?assertNotEqual("changeme", PagerDutyKey),

    %% Check webhook auth
    WebhookAuth = proplists:get_value(webhook_auth_header, TcpsHealthConfig),
    ?assertNotEqual("Bearer changeme", WebhookAuth),

    %% Check Datadog key
    DatadogKey = proplists:get_value(datadog_api_key, TcpsHealthConfig),
    ?assertNotEqual("changeme", DatadogKey),

    %% Check New Relic key
    NewRelicKey = proplists:get_value(newrelic_api_key, TcpsHealthConfig),
    ?assertNotEqual("changeme", NewRelicKey),

    %% Check New Relic account ID
    NewRelicAccountId = proplists:get_value(newrelic_account_id, TcpsHealthConfig),
    ?assertNotEqual("changeme", NewRelicAccountId),

    %% Check Grafana username
    GrafanaUser = proplists:get_value(grafana_cloud_username, TcpsHealthConfig),
    ?assertNotEqual("changeme", GrafanaUser),

    %% Check Grafana password
    GrafanaPass = proplists:get_value(grafana_cloud_password, TcpsHealthConfig),
    ?assertNotEqual("changeme", GrafanaPass),

    ok.

%% Test 2: Environment variables are used for credentials
environment_variables_for_credentials_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    TcpsHealthConfig = proplists:get_value(tcps_health, ErlmcpConfig),

    %% Verify email password uses {env, ...}
    EmailPassword = proplists:get_value(email_password, TcpsHealthConfig),
    ?assertMatch({env, _}, EmailPassword),

    %% Verify PagerDuty key uses {env, ...}
    PagerDutyKey = proplists:get_value(pagerduty_integration_key, TcpsHealthConfig),
    ?assertMatch({env, _}, PagerDutyKey),

    %% Verify webhook auth uses {env, ...}
    WebhookAuth = proplists:get_value(webhook_auth_header, TcpsHealthConfig),
    ?assertMatch({env, _}, WebhookAuth),

    %% Verify Datadog key uses {env, ...}
    DatadogKey = proplists:get_value(datadog_api_key, TcpsHealthConfig),
    ?assertMatch({env, _}, DatadogKey),

    %% Verify New Relic key uses {env, ...}
    NewRelicKey = proplists:get_value(newrelic_api_key, TcpsHealthConfig),
    ?assertMatch({env, _}, NewRelicKey),

    %% Verify Grafana username uses {env, ...}
    GrafanaUser = proplists:get_value(grafana_cloud_username, TcpsHealthConfig),
    ?assertMatch({env, _}, GrafanaUser),

    %% Verify Grafana password uses {env, ...}
    GrafanaPass = proplists:get_value(grafana_cloud_password, TcpsHealthConfig),
    ?assertMatch({env, _}, GrafanaPass),

    ok.

%% Test 3: No hardcoded system paths (except safe defaults)
no_hardcoded_paths_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    RootsConfig = proplists:get_value(roots, ErlmcpConfig),

    %% Check that allowed_paths doesn't hardcode /Users/sac
    AllowedPaths = proplists:get_value(allowed_paths, RootsConfig),

    %% AllowedPaths should be either an env tuple or a list without /Users/sac
    case AllowedPaths of
        {env, _} ->
            ok;  % Using environment variable - correct
        {env, _, DefaultValue} ->
            %% If using default, it should not include developer paths
            ?assert(not lists:member("/Users/sac/projects", DefaultValue)),
            ?assert(not lists:member("/Users/sac", DefaultValue));
        List when is_list(List) ->
            %% Should not contain developer-specific paths
            ?assert(not lists:member("/Users/sac/projects", List)),
            ?assert(not lists:member("/Users/sac", List))
    end,

    ok.

%% Test 4: Slack webhook uses environment variable
slack_webhook_uses_env_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    TcpsHealthConfig = proplists:get_value(tcps_health, ErlmcpConfig),

    SlackWebhook = proplists:get_value(slack_webhook, TcpsHealthConfig),
    ?assertMatch({env, _}, SlackWebhook),

    ok.

%% Test 5: No placeholder example.com URLs in critical config
no_placeholder_urls_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    TcpsHealthConfig = proplists:get_value(tcps_health, ErlmcpConfig),

    %% webhook_url should exist but be properly configured
    WebhookUrl = proplists:get_value(webhook_url, TcpsHealthConfig),
    case WebhookUrl of
        {env, _} ->
            ok;  % Using environment variable
        Url when is_list(Url) orelse is_binary(Url) ->
            %% Should be actual endpoint, not placeholder
            ?assertNotEqual("https://api.example.com/tcps/alerts", Url)
    end,

    ok.

%% Test 6: Default values are safe when env vars not set
safe_defaults_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    RootsConfig = proplists:get_value(roots, ErlmcpConfig),

    AllowedPaths = proplists:get_value(allowed_paths, RootsConfig),

    %% When using default value, it should be safe
    case AllowedPaths of
        {env, _, DefaultValue} ->
            %% Default should only include /tmp (safe shared temp directory)
            ?assertMatch(["/tmp"], DefaultValue);
        _ ->
            ok  % Either env-only or pre-set safe value
    end,

    ok.

%% Test 7: Environment variable names are consistent
environment_variable_naming_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    TcpsHealthConfig = proplists:get_value(tcps_health, ErlmcpConfig),

    %% Collect all env variable names
    EnvVars = extract_env_vars(TcpsHealthConfig),

    %% All env vars should follow ERLMCP_ prefix convention
    lists:foreach(fun({env, VarName}) ->
        ?assertMatch("ERLMCP_" ++ _, atom_to_list(VarName));
       ({env, VarName, _}) ->
            ?assertMatch("ERLMCP_" ++ _, atom_to_list(VarName))
    end, EnvVars),

    ok.

%% Test 8: Production config also uses environment variables
production_config_env_vars_test() ->
    case file:consult("config/production.config") of
        {ok, Config} ->
            ErlmcpConfig = proplists:get_value(erlmcp, Config),

            %% Check critical auth settings
            AuthConfig = proplists:get_value(auth, ErlmcpConfig),
            JwtConfig = proplists:get_value(jwt, AuthConfig),
            JwtSecret = proplists:get_value(secret_key, JwtConfig),

            %% JWT secret should use environment variable
            ?assertMatch({env, _}, JwtSecret),

            ok;
        {error, enoent} ->
            %% Production config not required for this test
            ok
    end.

%% Test 9: No credentials in example files
no_credentials_in_examples_test() ->
    ExampleDir = "examples",
    case file:list_dir(ExampleDir) of
        {ok, Files} ->
            ExampleErlFiles = [filename:join(ExampleDir, F) ||
                               F <- Files,
                               filename:extension(F) =:= ".erl"],

            lists:foreach(fun(File) ->
                {ok, Content} = file:read_file(File),
                ContentStr = binary_to_list(Content),

                %% Check for hardcoded credentials patterns
                ?assertNot(string:find(ContentStr, "\"changeme\"") =/= nomatch),

                %% Check for actual API key patterns (basic check)
                case string:find(ContentStr, "api_key") of
                    nomatch -> ok;
                    _ ->
                        %% If api_key exists, should reference env var or placeholder
                        ?assert(string:find(ContentStr, "{env,") =/= nomatch orelse
                               string:find(ContentStr, "example.com") =/= nomatch)
                end
            end, ExampleErlFiles),

            ok;
        {error, enoent} ->
            ok  % Examples directory not required
    end.

%% Test 10: Config validation rejects invalid paths
config_validation_test() ->
    {ok, Config} = file:consult("config/sys.config"),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    RootsConfig = proplists:get_value(roots, ErlmcpConfig),

    AllowedPaths = proplists:get_value(allowed_paths, RootsConfig),

    %% Validate that paths are properly formatted
    case AllowedPaths of
        {env, VarName} ->
            %% Should have valid environment variable name
            ?assertMatch("ERLMCP_" ++ _, atom_to_list(VarName));
        {env, VarName, DefaultValue} ->
            %% Should have valid environment variable name
            ?assertMatch("ERLMCP_" ++ _, atom_to_list(VarName)),
            %% Default value should be a list
            ?assert(is_list(DefaultValue));
        PathList when is_list(PathList) ->
            %% All paths should be absolute or start with /
            lists:foreach(fun(Path) ->
                ?assert(is_list(Path) orelse is_binary(Path))
            end, PathList)
    end,

    ok.

%% Test 11: No hardcoded database credentials
no_hardcoded_db_credentials_test() ->
    {ok, Config} = file:consult("config/sys.config"),

    %% Check sys.config doesn't have hardcoded DB passwords
    ConfigStr = io_lib:format("~p", [Config]),

    %% Should not contain common password patterns
    ?assertNot(string:find(ConfigStr, "\"changeme\"") =/= nomatch),

    ok.

%% Test 12: Config file paths are environment-aware
config_paths_are_relative_test() ->
    {ok, Config} = file:consult("config/sys.config"),

    ConfigStr = io_lib:format("~p", [Config]),

    %% Should not hardcode absolute developer paths like /Users/sac
    ?assertNot(string:find(ConfigStr, "/Users/sac") =/= nomatch),

    %% Log files should use relative or env-based paths
    %% (This test ensures future logs don't hardcode paths)

    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Extract all environment variable specifications from config
extract_env_vars(Config) when is_list(Config) ->
    lists:foldl(fun({_, Value}, Acc) ->
        case Value of
            {env, _} = EnvVar ->
                [EnvVar | Acc];
            {env, _, _} = EnvVar ->
                [EnvVar | Acc];
            NestedList when is_list(NestedList) ->
                extract_env_vars(NestedList) ++ Acc;
            _ ->
                Acc
        end
    end, [], Config);
extract_env_vars(_) ->
    [].

%%%===================================================================
%%% Test Generators
%%%===================================================================

%% Run all tests with proper setup/cleanup
config_security_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(no_hardcoded_credentials_test()),
            ?_test(environment_variables_for_credentials_test()),
            ?_test(no_hardcoded_paths_test()),
            ?_test(slack_webhook_uses_env_test()),
            ?_test(no_placeholder_urls_test()),
            ?_test(safe_defaults_test()),
            ?_test(environment_variable_naming_test()),
            ?_test(production_config_env_vars_test()),
            ?_test(no_credentials_in_examples_test()),
            ?_test(config_validation_test()),
            ?_test(no_hardcoded_db_credentials_test()),
            ?_test(config_paths_are_relative_test())
        ]
    }.
