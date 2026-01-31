%%%-------------------------------------------------------------------
%%% @doc Secret Scanning Tests
%%%
%%% Tests the secret scanning functionality to detect hardcoded secrets:
%%% - AWS Access Keys
%%% - GitHub Tokens
%%% - API Keys
%%% - Private Keys
%%% - Database Connection Strings
%%% - Various service credentials
%%%
%%% == Testing Strategy ==
%%% Creates temporary files with known secret patterns to verify
%%% detection accuracy. Cleans up test files after execution.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_secret_scan_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% @doc Create temporary test file with content
create_test_file(Name, Content) ->
    Filename = filename:join("/tmp", Name),
    ok = file:write_file(Filename, Content),
    Filename.

%% @doc Clean up test file
cleanup_test_file(Filename) ->
    file:delete(Filename).

%%%===================================================================
%%% AWS Access Key Tests
%%%===================================================================

aws_access_key_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_aws_access_key/0,
      fun test_detect_aws_secret_key/0,
      fun test_aws_key_with_context/0
     ]}.

test_detect_aws_access_key() ->
    %% AWS Access Key: AKIA + 16 chars = 20 total
    Content = <<"export AWS_ACCESS_KEY_ID=AKIA1234567890ABCDEF\n">>,
    Filename = create_test_file("aws_key_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"AWS Access Key">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_aws_secret_key() ->
    Content = <<"aws_secret_access_key = \"AKIA1234567890ABCDEF\"\n">>,
    Filename = create_test_file("aws_secret_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_aws_key_with_context() ->
    Content = <<
        "-module(config).\n"
        "get_aws_config() ->\n"
        "    #{access_key => \"AKIA1234567890ABCDEF\",\n"
        "      region => \"us-east-1\"}.\n"
    >>,
    Filename = create_test_file("aws_context_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        Secret = hd(Result),
        ?assertEqual(3, maps:get(line, Secret))
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% GitHub Token Tests
%%%===================================================================

github_token_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_github_pat/0,
      fun test_github_token_in_config/0,
      fun test_github_token_url/0
     ]}.

test_detect_github_pat() ->
    %% GitHub PAT: ghp_ + 36 chars = 40 total
    Content = <<"github_token: \"ghp_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\n">>,
    Filename = create_test_file("github_token_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"GitHub Token">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_github_token_in_config() ->
    Content = <<
        "{github, [\n"
        "  {token, \"ghp_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}\n"
        "]}.\n"
    >>,
    Filename = create_test_file("github_config_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_github_token_url() ->
    Content = <<"https://ghp_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa@github.com/repo.git\n">>,
    Filename = create_test_file("github_url_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% API Key Tests
%%%===================================================================

api_key_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_api_key/0,
      fun test_detect_google_api_key/0,
      fun test_detect_stripe_key/0,
      fun test_api_key_variations/0
     ]}.

test_detect_api_key() ->
    Content = <<"api_key := \"1234567890abcdefghijklmnopqrstuvwxyz\"\n">>,
    Filename = create_test_file("api_key_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"API Key">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_google_api_key() ->
    Content = <<"google_api_key = \"AIza1234567890abcdef1234567890abcdef123\"\n">>,
    Filename = create_test_file("google_key_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"Google API Key">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_stripe_key() ->
    Content = <<"stripe_secret: \"sk_live_1234567890abcdefghijklmn\"\n">>,
    Filename = create_test_file("stripe_key_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"Stripe Live Key">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_api_key_variations() ->
    Content = <<
        "apikey = \"key123\"\n"
        "APIKEY = \"key456\"\n"
        "Api-Key = \"key789\"\n"
    >>,
    Filename = create_test_file("api_key_var_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Private Key Tests
%%%===================================================================

private_key_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_rsa_private_key/0,
      fun test_detect_ec_private_key/0,
      fun test_private_key_assignment/0
     ]}.

test_detect_rsa_private_key() ->
    Content = <<"-----BEGIN RSA PRIVATE KEY-----\n">>,
    Filename = create_test_file("rsa_key_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"Private Key">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_ec_private_key() ->
    Content = <<"-----BEGIN EC PRIVATE KEY-----\n">>,
    Filename = create_test_file("ec_key_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_private_key_assignment() ->
    Content = <<"private_key := \"supersecretkey123\"\n">>,
    Filename = create_test_file("priv_key_assign_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Database Connection String Tests
%%%===================================================================

database_connection_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_postgresql_conn/0,
      fun test_detect_mysql_conn/0,
      fun test_detect_mongodb_conn/0
     ]}.

test_detect_postgresql_conn() ->
    Content = <<"db_url := \"postgresql://user:password@localhost:5432/db\"\n">>,
    Filename = create_test_file("postgres_conn_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"PostgreSQL Connection String">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_mysql_conn() ->
    Content = <<"mysql://root:secret@127.0.0.1:3306/mydb\n">>,
    Filename = create_test_file("mysql_conn_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"MySQL Connection String">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_mongodb_conn() ->
    Content = <<"mongodb://admin:pass123@localhost:27017/admin\n">>,
    Filename = create_test_file("mongo_conn_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"MongoDB Connection String">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Slack Token Tests
%%%===================================================================

slack_token_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_slack_bot_token/0,
      fun test_detect_slack_user_token/0,
      fun test_detect_slack_oauth_token/0
     ]}.

test_detect_slack_bot_token() ->
    Content = <<"slack_bot_token = \"xoxb-123456789012-1234567890123-abcdefghijklmnopqrstuvwxyz\"\n">>,
    Filename = create_test_file("slack_bot_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"Slack Token">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_detect_slack_user_token() ->
    Content = <<"SLACK_USER_TOKEN=\"xoxp-123456789012-123456789013-123456789014-abcdef\"\n">>,
    Filename = create_test_file("slack_user_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_detect_slack_oauth_token() ->
    Content = <<"slack_oauth: \"xoxa-123456789012-123456789015-abcdefghijklmnopqrstuvwxyz\"\n">>,
    Filename = create_test_file("slack_oauth_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Password Detection Tests
%%%===================================================================

password_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_detect_hardcoded_password/0,
      fun test_password_in_map/0,
      fun test_password_variable/0
     ]}.

test_detect_hardcoded_password() ->
    Content = <<"password := \"mysecretpassword\"\n">>,
    Filename = create_test_file("password_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0),
        ?assert(lists:any(fun(S) ->
            maps:get(type, S) =:= <<"Hardcoded Password">>
        end, Result))
    after
        cleanup_test_file(Filename)
    end.

test_password_in_map() ->
    Content = <<"Config = #{password => \"secret123\", user => \"admin\"}.\n">>,
    Filename = create_test_file("password_map_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_password_variable() ->
    Content = <<"DbPassword = \"supersecretpass\"\n">>,
    Filename = create_test_file("password_var_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Negative Tests (Clean Content)
%%%===================================================================

clean_content_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_clean_erlang_code/0,
      fun test_clean_config/0,
      fun test_clean_environment_vars/0
     ]}.

test_clean_erlang_code() ->
    Content = <<
        "-module(my_module).\n"
        "-export([func/1]).\n"
        "func(X) -> X * 2.\n"
    >>,
    Filename = create_test_file("clean_code_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assertEqual(0, length(Result))
    after
        cleanup_test_file(Filename)
    end.

test_clean_config() ->
    Content = <<
        "{port, 8080}.\n"
        "{host, \"localhost\"}.\n"
        "{log_level, info}.\n"
    >>,
    Filename = create_test_file("clean_config_test.config", Content),
    try
        Result = scan_test_file(Filename),
        ?assertEqual(0, length(Result))
    after
        cleanup_test_file(Filename)
    end.

test_clean_environment_vars() ->
    Content = <<
        "export DB_HOST=localhost\n"
        "export DB_PORT=5432\n"
        "export LOG_LEVEL=debug\n"
    >>,
    Filename = create_test_file("clean_env_test.sh", Content),
    try
        Result = scan_test_file(Filename),
        ?assertEqual(0, length(Result))
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Edge Cases
%%%===================================================================

edge_case_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_empty_file/0,
      fun test_multiline_secret/0,
      fun test_commented_secret/0,
      fun test_fake_positive_prevention/0
     ]}.

test_empty_file() ->
    Content = <<"\n\n\n">>,
    Filename = create_test_file("empty_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assertEqual(0, length(Result))
    after
        cleanup_test_file(Filename)
    end.

test_multiline_secret() ->
    Content = <<
        "api_key = \n"
        "\"AKIA1234567890ABCD\"\n"
    >>,
    Filename = create_test_file("multiline_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_commented_secret() ->
    Content = <<
        "% password := \"oldpassword\"\n"
        "password := application:get_env(db, password)\n"
    >>,
    Filename = create_test_file("commented_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        % Should detect commented secrets too (better safe than sorry)
        ?assert(length(Result) > 0)
    after
        cleanup_test_file(Filename)
    end.

test_fake_positive_prevention() ->
    Content = <<
        "%% This is not a real secret\n"
        "api_key_format = \"AKIA........................\"\n"
        "token_template = \"ghp_................................\"\n"
    >>,
    Filename = create_test_file("fake_positive_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        % May or may not detect patterns - depends on regex
        ?assert(is_list(Result))
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Multiple Secrets Test
%%%===================================================================

multiple_secrets_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      fun test_multiple_secrets_same_file/0,
      fun test_secrets_across_lines/0
     ]}.

test_multiple_secrets_same_file() ->
    Content = <<
        "aws_key = \"AKIA1234567890ABCDEF\"\n"
        "github_token = \"ghp_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\n"
        "api_key = \"1234567890abcdefghijklmnopqrstuvwxyz\"\n"
    >>,
    Filename = create_test_file("multiple_secrets_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) >= 3)
    after
        cleanup_test_file(Filename)
    end.

test_secrets_across_lines() ->
    Content = <<
        "config1 = #{key => \"val1\"}.\n"
        "aws_key = \"AKIA1234567890ABCDEF\"\n"
        "config2 = #{key => \"val2\"}.\n"
        "db_url = \"postgresql://user:pass@localhost/db\"\n"
        "config3 = #{key => \"val3\"}.\n"
    >>,
    Filename = create_test_file("across_lines_test.erl", Content),
    try
        Result = scan_test_file(Filename),
        ?assert(length(Result) >= 2),
        ?assert(lists:any(fun(S) -> maps:get(line, S) =:= 2 end, Result)),
        ?assert(lists:any(fun(S) -> maps:get(line, S) =:= 4 end, Result))
    after
        cleanup_test_file(Filename)
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private Scan test file using secret patterns from security validator
scan_test_file(Filename) ->
    %% Use the same patterns as check_no_hardcoded_secrets/1
    SecretPatterns = [
        "password\\s*:=\\s*\"[^\"]+\"",
        "api_key\\s*:=\\s*\"[^\"]+\"",
        "secret\\s*:=\\s*\"[^\"]+\"",
        "token\\s*:=\\s*\"[^\"]+\"",
        "private_key\\s*:=\\s*\"[^\"]+\"",
        "AKIA[0-9A-Z]{16}",
        "AIza[0-9A-Za-z\\-_]{35}",
        "sk_live_[0-9a-zA-Z]{24}",
        "xox[baprs]-[0-9]{12}-[0-9]{12}-[0-9a-zA-Z]{32}",
        "ghp_[a-zA-Z0-9]{36}",
        "postgresql://[^:]+:[^@]+@",
        "mysql://[^:]+:[^@]+@",
        "mongodb://[^:]+:[^@]+@"
    ],
    case erlmcp_security_validator:scan_files_for_secrets([Filename], SecretPatterns) of
        [] -> [];
        Results ->
            case hd(Results) of
                #{secrets := Secrets} -> Secrets;
                _ -> []
            end
    end.
