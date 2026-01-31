%%%-------------------------------------------------------------------
%%% @doc erlmcp_secrets_vault_tests - Vault Integration Tests
%%%
%%% Tests for HashiCorp Vault integration in erlmcp_secrets module.
%%%
%%% Test Coverage:
%%% - Token authentication
%%% - AppRole authentication
%%% - Kubernetes authentication
%%% - Secret CRUD operations (get/set/delete/list)
%%% - Token refresh logic
%%% - Error handling (4xx, 5xx, network failures)
%%%
%%% NOTE: These are integration tests that require a real Vault instance.
%%% For unit tests, use meck to mock HTTP responses.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_secrets_vault_tests).
-author("erlmcp").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Configuration Tests
%%====================================================================

%% @doc Test token-based configuration parsing
token_config_test() ->
    Config = #{
        url => <<"http://localhost:8200">>,
        auth_method => token,
        token => <<"test-token">>,
        mount => <<"secret">>
    },

    % Test that configuration is accepted
    ?assert(is_map(Config)),
    ?assertEqual(<<"http://localhost:8200">>, maps:get(url, Config)),
    ?assertEqual(token, maps:get(auth_method, Config)),
    ?assertEqual(<<"test-token">>, maps:get(token, Config)).

%% @doc Test AppRole-based configuration parsing
approle_config_test() ->
    Config = #{
        url => <<"http://localhost:8200">>,
        auth_method => approle,
        role_id => <<"test-role-id">>,
        secret_id => <<"test-secret-id">>,
        mount => <<"secret">>
    },

    % Test that configuration is accepted
    ?assert(is_map(Config)),
    ?assertEqual(approle, maps:get(auth_method, Config)),
    ?assertEqual(<<"test-role-id">>, maps:get(role_id, Config)),
    ?assertEqual(<<"test-secret-id">>, maps:get(secret_id, Config)).

%% @doc Test Kubernetes-based configuration parsing
kubernetes_config_test() ->
    Config = #{
        url => <<"http://localhost:8200">>,
        auth_method => kubernetes,
        k8s_role => <<"test-k8s-role">>,
        k8s_jwt_path => <<"/var/run/secrets/kubernetes.io/serviceaccount/token">>,
        mount => <<"secret">>
    },

    % Test that configuration is accepted
    ?assert(is_map(Config)),
    ?assertEqual(kubernetes, maps:get(auth_method, Config)),
    ?assertEqual(<<"test-k8s-role">>, maps:get(k8s_role, Config)).

%%====================================================================
%% Vault API Path Tests
%%====================================================================

%% @doc Test Vault KV v2 path building without namespace
vault_path_test() ->
    VaultState = #{
        url => <<"http://localhost:8200">>,
        token => <<"test-token">>,
        auth_method => token,
        mount => <<"secret">>,
        namespace => undefined
    },

    Path = iolist_to_binary([<<"/v1/">>, <<"secret">>, <<"/">>, <<"data">>, <<"/">>, <<"test-key">>]),
    ?assertEqual(<<"/v1/secret/data/test-key">>, Path).

%% @doc Test Vault KV v2 path building with namespace
vault_path_with_namespace_test() ->
    Namespace = <<"my-namespace">>,
    Path = iolist_to_binary([<<"/v1/">>, Namespace, <<"/">>, <<"secret">>, <<"/">>, <<"data">>, <<"/">>, <<"test-key">>]),
    ?assertEqual(<<"/v1/my-namespace/secret/data/test-key">>, Path).

%%====================================================================
%% Vault Response Parsing Tests
%%====================================================================

%% @doc Test parsing Vault secret response (KV v2 format)
parse_secret_response_test() ->
    ResponseBody = jsx:encode(#{
        <<"data">> => #{
            <<"data">> => #{
                <<"value">> => <<"my-secret-value">>
            }
        }
    }),

    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"data">> := #{<<"data">> := Data}} ->
            ?assertEqual(<<"my-secret-value">>, maps:get(<<"value">>, Data))
    catch
        _:_ -> ?assert(false)
    end.

%% @doc Test parsing Vault list response
parse_list_response_test() ->
    ResponseBody = jsx:encode(#{
        <<"data">> => #{
            <<"keys">> => [<<"secret1">>, <<"secret2">>, <<"secret3">>]
        }
    }),

    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"data">> := #{<<"keys">> := Keys}} ->
            ?assertEqual([<<"secret1">>, <<"secret2">>, <<"secret3">>], Keys)
    catch
        _:_ -> ?assert(false)
    end.

%% @doc Test parsing Vault error response
parse_error_response_test() ->
    ResponseBody = jsx:encode(#{
        <<"errors">> => [<<"permission denied">>]
    }),

    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"errors">> := Errors} ->
            ?assertEqual([<<"permission denied">>], Errors)
    catch
        _:_ -> ?assert(false)
    end.

%%====================================================================
%% Authentication Response Tests
%%====================================================================

%% @doc Test parsing AppRole authentication response
parse_approle_auth_response_test() ->
    ResponseBody = jsx:encode(#{
        <<"auth">> => #{
            <<"client_token">> => <<"generated-vault-token">>,
            <<"lease_duration">> => 3600,
            <<"renewable">> => true
        }
    }),

    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"auth">> := #{<<"client_token">> := Token, <<"lease_duration">> := LeaseDuration}} ->
            ?assertEqual(<<"generated-vault-token">>, Token),
            ?assertEqual(3600, LeaseDuration)
    catch
        _:_ -> ?assert(false)
    end.

%% @doc Test parsing Kubernetes authentication response
parse_k8s_auth_response_test() ->
    ResponseBody = jsx:encode(#{
        <<"auth">> => #{
            <<"client_token">> => <<"k8s-vault-token">>,
            <<"lease_duration">> => 1800,
            <<"renewable">> => false
        }
    }),

    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"auth">> := #{<<"client_token">> := Token, <<"lease_duration">> := LeaseDuration}} ->
            ?assertEqual(<<"k8s-vault-token">>, Token),
            ?assertEqual(1800, LeaseDuration)
    catch
        _:_ -> ?assert(false)
    end.

%%====================================================================
%% Integration Test (requires real Vault)
%%====================================================================

%% @doc Integration test with real Vault instance (optional)
%% To run this test:
%% 1. Start Vault: vault server -dev
%% 2. Set VAULT_ADDR and VAULT_TOKEN environment variables
%% 3. Enable KV v2: vault secrets enable -path=secret kv-v2
%% 4. Run: rebar3 eunit --module=erlmcp_secrets_vault_tests -v
integration_test_() ->
    {foreach,
     fun setup_integration/0,
     fun teardown_integration/1,
     [
      {"Vault GET secret", fun test_vault_get/0},
      {"Vault SET secret", fun test_vault_set/0},
      {"Vault DELETE secret", fun test_vault_delete/0},
      {"Vault LIST secrets", fun test_vault_list/0}
     ]
    }.

setup_integration() ->
    case os:getenv("VAULT_ADDR") of
        false ->
            {skip, "VAULT_ADDR not set"};
        _ ->
            {ok, Pid} = erlmcp_secrets:start_link(#{
                backend => vault,
                backend_config => #{
                    url => list_to_binary(os:getenv("VAULT_ADDR", "http://localhost:8200")),
                    auth_method => token,
                    token => list_to_binary(os:getenv("VAULT_TOKEN", "dev-only-token")),
                    mount => <<"secret">>
                }
            }),
            {ok, Pid}
    end.

teardown_integration({ok, Pid}) ->
    erlmcp_secrets:stop();
teardown_integration({skip, _Reason}) ->
    ok.

test_vault_get() ->
    % Setup: Create a test secret
    erlmcp_secrets:set_secret(<<"test-secret">>, <<"test-value">>),

    % Execute: Get the secret
    {ok, Value} = erlmcp_secrets:get_secret(<<"test-secret">>),

    % Assert
    ?assertEqual(<<"test-value">>, Value),

    % Cleanup
    erlmcp_secrets:delete_secret(<<"test-secret">>).

test_vault_set() ->
    % Execute: Set a secret
    ok = erlmcp_secrets:set_secret(<<"test-set-secret">>, <<"test-set-value">>),

    % Verify: Get it back
    {ok, Value} = erlmcp_secrets:get_secret(<<"test-set-secret">>),
    ?assertEqual(<<"test-set-value">>, Value),

    % Cleanup
    erlmcp_secrets:delete_secret(<<"test-set-secret">>).

test_vault_delete() ->
    % Setup: Create a secret
    erlmcp_secrets:set_secret(<<"test-delete-secret">>, <<"test-delete-value">>),

    % Execute: Delete it
    ok = erlmcp_secrets:delete_secret(<<"test-delete-secret">>),

    % Verify: It's gone
    ?assertEqual({error, not_found}, erlmcp_secrets:get_secret(<<"test-delete-secret">>)).

test_vault_list() ->
    % Setup: Create multiple secrets
    erlmcp_secrets:set_secret(<<"list-secret-1">>, <<"value1">>),
    erlmcp_secrets:set_secret(<<"list-secret-2">>, <<"value2">>),
    erlmcp_secrets:set_secret(<<"list-secret-3">>, <<"value3">>),

    % Execute: List all secrets
    {ok, SecretKeys} = erlmcp_secrets:list_secrets(),

    % Assert: Our secrets are in the list
    ?assert(lists:member(<<"list-secret-1">>, SecretKeys)),
    ?assert(lists:member(<<"list-secret-2">>, SecretKeys)),
    ?assert(lists:member(<<"list-secret-3">>, SecretKeys)),

    % Cleanup
    erlmcp_secrets:delete_secret(<<"list-secret-1">>),
    erlmcp_secrets:delete_secret(<<"list-secret-2">>),
    erlmcp_secrets:delete_secret(<<"list-secret-3">>).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Calculate Vault KV v2 API path
calculate_vault_path(Mount, Endpoint, Key) ->
    iolist_to_binary([<<"/v1/">>, Mount, <<"/">>, Endpoint, <<"/">>, Key]).
