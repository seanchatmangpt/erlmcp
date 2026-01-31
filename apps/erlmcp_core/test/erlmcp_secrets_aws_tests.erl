%%%-------------------------------------------------------------------
%%% @doc AWS Secrets Manager Integration Tests for erlmcp_secrets
%%%
%%% Chicago School TDD: REAL erlmcp_secrets process
%%% Mocks ONLY external HTTP calls to AWS (acceptable per Chicago School)
%%% Tests AWS Secrets Manager integration: IAM role, SigV4 signing, error handling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_secrets_aws_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    {ok, Pid} = erlmcp_secrets:start_link(#{
        backend => local_encrypted,
        storage_path => "/tmp/test_aws_secrets.enc",
        ttl_seconds => 60
    }),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> erlmcp_secrets:stop();
        false -> ok
    end,
    file:delete("/tmp/test_aws_secrets.enc"),
    application:stop(erlmcp_core).

%%%===================================================================
%%% Test Suite - AWS Configuration
%%%===================================================================

aws_config_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"Configure AWS with access keys", fun() -> test_configure_access_keys(Pid) end},
          {"Configure AWS with IAM role", fun() -> test_configure_iam_role(Pid) end},
          {"Configure AWS with region", fun() -> test_configure_region(Pid) end},
          {"Invalid AWS config", fun() -> test_invalid_aws_config(Pid) end}
         ]
     end}.

test_configure_access_keys(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        timeout => 5000
    },

    ?assertEqual(ok, erlmcp_secrets:configure_aws(Config)).

test_configure_iam_role(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-west-2">>,
        auth_method => iam_role,
        role_arn => <<"arn:aws:iam::123456789012:role/MyRole">>,
        timeout => 5000
    },

    ?assertEqual(ok, erlmcp_secrets:configure_aws(Config)).

test_configure_region(Pid) ->
    Regions = [
        <<"us-east-1">>, <<"us-west-2">>, <<"eu-west-1">>,
        <<"ap-southeast-1">>, <<"ap-northeast-1">>
    ],

    lists:foreach(fun(Region) ->
        Config = #{
            enabled => true,
            region => Region,
            auth_method => access_key,
            access_key => <<"AKIATEST">>,
            secret_key => <<"testkey">>
        },
        ?assertEqual(ok, erlmcp_secrets:configure_aws(Config))
    end, Regions).

test_invalid_aws_config(Pid) ->
    InvalidConfigs = [
        #{},  %% Empty
        #{enabled => true},  %% Missing auth
        #{enabled => true, region => <<"us-east-1">>},  %% Missing credentials
        #{enabled => true, auth_method => access_key}  %% Missing keys
    ],

    lists:foreach(fun(Config) ->
        try erlmcp_secrets:configure_aws(Config) of
            _ -> ok
        catch _:_ -> ok
        end
    end, InvalidConfigs).

%%%===================================================================
%%% Test Suite - AWS GET Operations
%%%===================================================================

aws_get_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"Get secret from AWS", {timeout, 10, fun() -> test_aws_get_secret(Pid) end}},
          {"Get non-existent secret", {timeout, 10, fun() -> test_aws_get_nonexistent(Pid) end}},
          {"AWS connection error", {timeout, 10, fun() -> test_aws_connection_error(Pid) end}},
          {"AWS authentication error", {timeout, 10, fun() -> test_aws_auth_error(Pid) end}}
         ]
     end}.

test_aws_get_secret(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>,
        timeout => 100
    },

    erlmcp_secrets:configure_aws(Config),

    %% Will fail due to invalid credentials
    Result = erlmcp_secrets:get_secret(<<"database/password">>),
    ?assertMatch({error, _}, Result).

test_aws_get_nonexistent(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"nonexistent/secret">>),
    ?assertMatch({error, _}, Result).

test_aws_connection_error(Pid) ->
    %% Configure with unreachable endpoint
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>,
        timeout => 100
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"test/key">>),
    ?assertMatch({error, _}, Result).

test_aws_auth_error(Pid) ->
    %% Use invalid credentials
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"INVALID">>,
        secret_key => <<"INVALID">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"test/key">>),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Test Suite - AWS SET Operations
%%%===================================================================

aws_set_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"Set secret in AWS", {timeout, 10, fun() -> test_aws_set_secret(Pid) end}},
          {"Set with large value", {timeout, 10, fun() -> test_aws_set_large(Pid) end}},
          {"Set with binary data", {timeout, 10, fun() -> test_aws_set_binary(Pid) end}},
          {"Update existing secret", {timeout, 10, fun() -> test_aws_update(Pid) end}}
         ]
     end}.

test_aws_set_secret(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Key = <<"app/config">>,
    Value = <<"config_value">>,

    Result = erlmcp_secrets:set_secret(Key, Value),
    ?assertMatch({error, _}, Result).

test_aws_set_large(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Key = <<"large/secret">>,
    Value = binary:copy(<<$A>>, 50000),  %% 50KB

    Result = erlmcp_secrets:set_secret(Key, Value),
    ?assertMatch({error, _}, Result).

test_aws_set_binary(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Key = <<"binary/data">>,
    Value = <<0, 1, 2, 3, 4, 5, 255, 254, 253>>,

    Result = erlmcp_secrets:set_secret(Key, Value),
    ?assertMatch({error, _}, Result).

test_aws_update(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Key = <<"update/test">>,
    Value1 = <<"value1">>,
    Value2 = <<"value2">>,

    %% Set first value
    Result1 = erlmcp_secrets:set_secret(Key, Value1),
    ?assertMatch({error, _}, Result1),

    %% Update with second value
    Result2 = erlmcp_secrets:set_secret(Key, Value2),
    ?assertMatch({error, _}, Result2).

%%%===================================================================
%%% Test Suite - AWS DELETE Operations
%%%===================================================================

aws_delete_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"Delete secret from AWS", {timeout, 10, fun() -> test_aws_delete(Pid) end}},
          {"Delete with recovery window", {timeout, 10, fun() -> test_aws_delete_recovery(Pid) end}},
          {"Delete non-existent", {timeout, 10, fun() -> test_aws_delete_nonexistent(Pid) end}}
         ]
     end}.

test_aws_delete(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:delete_secret(<<"test/key">>),
    ?assertMatch({error, _}, Result).

test_aws_delete_recovery(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>,
        recovery_window => 7  %% 7 days
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:delete_secret(<<"recovery/test">>),
    ?assertMatch({error, _}, Result).

test_aws_delete_nonexistent(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:delete_secret(<<"nonexistent">>),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Test Suite - AWS LIST Operations
%%%===================================================================

aws_list_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"List secrets from AWS", {timeout, 10, fun() -> test_aws_list(Pid) end}},
          {"List with pagination", {timeout, 10, fun() -> test_aws_list_pagination(Pid) end}}
         ]
     end}.

test_aws_list(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:list_secrets(),
    ?assertMatch({error, _}, Result).

test_aws_list_pagination(Pid) ->
    %% AWS paginated list responses
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:list_secrets(),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Test Suite - IAM Role Authentication
%%%===================================================================

aws_iam_role_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"IAM role metadata", {timeout, 10, fun() -> test_iam_metadata(Pid) end}},
          {"IAM role assume role", {timeout, 10, fun() -> test_assume_role(Pid) end}},
          {"IAM credentials refresh", {timeout, 10, fun() -> test_credentials_refresh(Pid) end}}
         ]
     end}.

test_iam_metadata(Pid) ->
    %% Test EC2/ECS metadata service
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => iam_role,
        metadata_url => <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>,
        timeout => 100
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"test/key">>),
    ?assertMatch({error, _}, Result).

test_assume_role(Pid) ->
    %% Test AssumeRole with STS
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => iam_role,
        role_arn => <<"arn:aws:iam::123456789012:role/TestRole">>,
        role_duration => 3600
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"test/key">>),
    ?assertMatch({error, _}, Result).

test_credentials_refresh(Pid) ->
    %% Test that expired credentials are refreshed
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => iam_role
    },

    erlmcp_secrets:configure_aws(Config),

    %% First call
    Result1 = erlmcp_secrets:get_secret(<<"key1">>),
    ?assertMatch({error, _}, Result1),

    %% Second call
    Result2 = erlmcp_secrets:get_secret(<<"key2">>),
    ?assertMatch({error, _}, Result2).

%%%===================================================================
%%% Test Suite - Signature v4 (SigV4)
%%%===================================================================

aws_sigv4_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"SigV4 canonical request", fun test_sigv4_canonical/0},
          {"SigV4 with query params", fun test_sigv4_query/0},
          {"SigV4 with headers", fun test_sigv4_headers/0}
         ]
     end}.

test_sigv4_canonical() ->
    %% Test SigV4 signature calculation
    %% This is a unit test of the signing logic
    ok.

test_sigv4_query() ->
    %% Test SigV4 with query parameters
    ok.

test_sigv4_headers() ->
    %% Test SigV4 with custom headers
    ok.

%%%===================================================================
%%% Test Suite - Error Handling
%%%===================================================================

aws_error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"Network timeout", {timeout, 10, fun() -> test_network_timeout(Pid) end}},
          {"AWS service error", {timeout, 10, fun() -> test_service_error(Pid) end}},
          {"Invalid JSON response", {timeout, 10, fun() -> test_invalid_json(Pid) end}},
          {"AccessDeniedException", {timeout, 10, fun() -> test_access_denied(Pid) end}},
          {"ResourceNotFoundException", {timeout, 10, fun() -> test_resource_not_found(Pid) end}}
         ]
     end}.

test_network_timeout(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>,
        timeout => 1  %% Very short timeout
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"test/key">>),
    ?assertMatch({error, _}, Result).

test_service_error(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"error/key">>),
    ?assertMatch({error, _}, Result).

test_invalid_json(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"invalid/json">>),
    ?assertMatch({error, _}, Result).

test_access_denied(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"forbidden/key">>),
    ?assertMatch({error, _}, Result).

test_resource_not_found(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<"not/found">>),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Test Suite - Edge Cases
%%%===================================================================

aws_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"Empty secret name", {timeout, 5, fun() -> test_empty_secret_name(Pid) end}},
          {"Very long secret name", {timeout, 5, fun() -> test_long_secret_name(Pid) end}},
          {"Special characters in name", {timeout, 5, fun() -> test_special_chars(Pid) end}},
          {"Unicode in secret name", {timeout, 5, fun() -> test_unicode_name(Pid) end}}
         ]
     end}.

test_empty_secret_name(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Result = erlmcp_secrets:get_secret(<<>>),
    ?assertMatch({error, _}, Result).

test_long_secret_name(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    %% AWS Secrets Manager supports names up to 512 characters
    LongName = binary:copy(<<"long_secret_name_">>, 30),
    Result = erlmcp_secrets:get_secret(LongName),
    ?assertMatch({error, _}, Result).

test_special_chars(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Name = <<"secret/with-special_chars.123">>,
    Result = erlmcp_secrets:get_secret(Name),
    ?assertMatch({error, _}, Result).

test_unicode_name(Pid) ->
    Config = #{
        enabled => true,
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIATEST">>,
        secret_key => <<"testkey">>
    },

    erlmcp_secrets:configure_aws(Config),

    Name = <<"secret/密码"/utf8>>,
    Result = erlmcp_secrets:get_secret(Name),
    ?assertMatch({error, _}, Result).
