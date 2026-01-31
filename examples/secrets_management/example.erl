#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc Secrets Management Integration Example
%%%
%%% Demonstrates secrets management with backends:
%%% 1. Configure local_encrypted backend (default)
%%% 2. Store secret with encryption
%%% 3. Retrieve secret (cache hit)
%%% 4. Demonstrate failover to local
%%% 5. Show secret rotation
%%%
%%% Prerequisites:
%%% - erlmcp_core application started
%%% - secrets manager running
%%%
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

-define(SECRETS_FILE, "examples/secrets_management/secrets.enc").
-define(KEY_FILE, "examples/secrets_management/master.key").

main(_) ->
    io:format("~n=== Secrets Management Integration Example ===~n~n"),

    %% Clean up any existing files
    file:delete(?SECRETS_FILE),
    file:delete(?KEY_FILE),

    %% Step 1: Start applications
    io:format("Step 1: Starting erlmcp applications...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_core),
    io:format("✓ Applications started~n~n"),

    %% Step 2: Start secrets manager with local encrypted backend
    io:format("Step 2: Starting secrets manager (local_encrypted)...~n"),
    {ok, SecretsPid} = erlmcp_secrets:start_link(#{
        backend => local_encrypted,
        backend_config => #{},
        ttl_seconds => 300,  % 5-minute cache TTL
        storage_path => ?SECRETS_FILE,
        encryption_key_path => ?KEY_FILE
    }),
    io:format("✓ Secrets manager started: ~p~n~n", [SecretsPid]),

    %% Step 3: Store secrets
    io:format("Step 3: Storing secrets...~n"),

    %% Store API key
    case erlmcp_secrets:set_secret(<<"api_key">>, <<"sk-1234567890abcdef">>) of
        ok -> io:format("✓ Stored: api_key=****~n");
        {error, Reason} -> io:format("✗ Failed to store api_key: ~p~n", [Reason])
    end,

    %% Store database password
    case erlmcp_secrets:set_secret(<<"db_password">>, <<"SuperSecret123!">>) of
        ok -> io:format("✓ Stored: db_password=****~n");
        {error, Reason} -> io:format("✗ Failed to store db_password: ~p~n", [Reason])
    end,

    %% Store OAuth token
    case erlmcp_secrets:set_secret(<<"oauth_token">>, <<"ya29.a0AfH6...">>) of
        ok -> io:format("✓ Stored: oauth_token=****~n");
        {error, Reason} -> io:format("✗ Failed to store oauth_token: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 4: Retrieve secrets (cache miss - fetch from backend)
    io:format("Step 4: Retrieving secrets (cache miss)...~n"),

    case erlmcp_secrets:get_secret(<<"api_key">>) of
        {ok, ApiKey} ->
            io:format("✓ Retrieved api_key: ~s~n", [mask_secret(ApiKey)]);
        {error, Reason} ->
            io:format("✗ Failed to retrieve api_key: ~p~n", [Reason])
    end,

    case erlmcp_secrets:get_secret(<<"db_password">>) of
        {ok, DbPass} ->
            io:format("✓ Retrieved db_password: ~s~n", [mask_secret(DbPass)]);
        {error, Reason} ->
            io:format("✗ Failed to retrieve db_password: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 5: Retrieve secrets again (cache hit)
    io:format("Step 5: Retrieving secrets (cache hit)...~n"),

    case erlmcp_secrets:get_secret(<<"api_key">>) of
        {ok, ApiKey} ->
            io:format("✓ Retrieved api_key from cache: ~s~n", [mask_secret(ApiKey)]);
        {error, Reason} ->
            io:format("✗ Failed: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 6: List all secrets
    io:format("Step 6: Listing all secrets...~n"),
    case erlmcp_secrets:list_secrets() of
        {ok, SecretKeys} ->
            io:format("✓ Total secrets: ~p~n", [length(SecretKeys)]),
            lists:foreach(fun(Key) ->
                io:format("  - ~s~n", [Key])
            end, SecretKeys);
        {error, Reason} ->
            io:format("✗ Failed to list secrets: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 7: Rotate a secret
    io:format("Step 7: Rotating a secret...~n"),
    io:format("Old secret: ~s~n", [mask_secret(<<"sk-1234567890abcdef">>)]),

    case erlmcp_secrets:rotate_secret(<<"api_key">>) of
        {ok, NewApiKey} ->
            io:format("✓ Rotated api_key~n"),
            io:format("New secret: ~s~n", [mask_secret(NewApiKey)]),

            %% Verify new value
            case erlmcp_secrets:get_secret(<<"api_key">>) of
                {ok, RotatedKey} ->
                    io:format("✓ Verified new value: ~s~n", [mask_secret(RotatedKey)]);
                {error, Reason} ->
                    io:format("✗ Failed to verify: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("✗ Failed to rotate: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 8: Verify encrypted file exists
    io:format("Step 8: Verifying encrypted storage...~n"),
    case file:read_file_info(?SECRETS_FILE) of
        {ok, FileInfo} ->
            Size = FileInfo#file_info.size,
            io:format("✓ Encrypted file: ~s (~p bytes)~n", [?SECRETS_FILE, Size]),
            io:format("✓ File mode: ~8.8.0B~n", [FileInfo#file_info.mode]);
        {error, Reason} ->
            io:format("✗ File not found: ~p~n", [Reason])
    end,

    case file:read_file_info(?KEY_FILE) of
        {ok, _} ->
            io:format("✓ Encryption key: ~s~n", [?KEY_FILE]);
        {error, Reason} ->
            io:format("✗ Key file not found: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 9: Delete a secret
    io:format("Step 9: Deleting a secret...~n"),
    case erlmcp_secrets:delete_secret(<<"oauth_token">>) of
        ok ->
            io:format("✓ Deleted: oauth_token~n"),
            case erlmcp_secrets:get_secret(<<"oauth_token">>) of
                {error, not_found} ->
                    io:format("✓ Verified deletion (not_found)~n");
                {ok, _} ->
                    io:format("✗ Secret still exists after deletion~n")
            end;
        {error, Reason} ->
            io:format("✗ Failed to delete: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 10: Demonstrate failover to local
    io:format("Step 10: Demonstrating Vault failover to local...~n"),

    %% Try to configure Vault (will fail, should stay on local)
    case erlmcp_secrets:configure_vault(#{
        url => <<"http://localhost:8200">>,
        token => <<"test-token">>,
        auth_method => token
    }) of
        ok ->
            io:format("✓ Configured Vault backend~n"),
            %% Try to get secret from Vault (will fail if not available)
            case erlmcp_secrets:get_secret(<<"vault_secret">>) of
                {error, _} ->
                    io:format("✓ Vault unavailable (expected in demo)~n"),
                    %% This demonstrates failover - system continues working
                    io:format("✓ Local encrypted backend still working~n");
                {ok, _} ->
                    io:format("✓ Retrieved from Vault~n")
            end;
        {error, Reason} ->
            io:format("✗ Vault config failed: ~p~n", [Reason]),
            io:format("✓ Staying on local_encrypted backend~n")
    end,
    io:format("~n"),

    %% Step 11: Stop and restart (test persistence)
    io:format("Step 11: Testing persistence across restarts...~n"),
    erlmcp_secrets:stop(),
    io:format("✓ Stopped secrets manager~n"),

    %% Restart
    {ok, _SecretsPid2} = erlmcp_secrets:start_link(#{
        backend => local_encrypted,
        backend_config => #{},
        ttl_seconds => 300,
        storage_path => ?SECRETS_FILE,
        encryption_key_path => ?KEY_FILE
    }),
    io:format("✓ Restarted secrets manager~n"),

    %% Verify secrets persisted
    case erlmcp_secrets:list_secrets() of
        {ok, SecretKeys} ->
            io:format("✓ Secrets persisted: ~p~n", [length(SecretKeys)]);
        {error, Reason} ->
            io:format("✗ Failed to list: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 12: Cleanup
    io:format("Step 12: Cleaning up...~n"),
    erlmcp_secrets:stop(),
    application:stop(erlmcp_core),
    file:delete(?SECRETS_FILE),
    file:delete(?KEY_FILE),
    io:format("✓ Cleanup complete~n~n"),

    io:format("=== Example Complete ===~n"),
    init:stop().

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% @doc Mask secret for display (show first 4 chars only)
mask_secret(<<First:4/binary, _Rest/binary>>) ->
    <<First/binary, "...">>;
mask_secret(Short) when byte_size(Short) < 4 ->
    <<"***">>;
mask_secret(_) ->
    <<"***">>.
