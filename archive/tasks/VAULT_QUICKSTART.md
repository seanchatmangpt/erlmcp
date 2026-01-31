# Vault Backend Quick Start

## 1. Compilation

Compile the erlmcp_core application with the fixed Vault implementation:

```bash
cd /home/user/erlmcp
TERM=dumb rebar3 compile
```

Expected output:
```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling erlmcp_validation
```

### Verify Compilation

Check that `erlmcp_secrets.erl` compiled successfully:

```bash
ls -lh _build/default/lib/erlmcp_core/ebin/erlmcp_secrets.beam
```

## 2. Run Unit Tests

Run the basic unit tests (no Vault required):

```bash
rebar3 eunit --module=erlmcp_secrets_vault_tests
```

Expected output:
```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_core
===> Performing EUnit tests...

erlmcp_secrets_vault_tests: start_with_vault_config_test...ok
erlmcp_secrets_vault_tests: configure_vault_backend_test...ok
erlmcp_secrets_vault_tests: local_storage_fallback_test...ok
erlmcp_secrets_vault_tests: secret_rotation_test...ok
erlmcp_secrets_vault_tests: caching_test...ok

Finished in 3.5 seconds
5 tests, 0 failures
```

## 3. Integration Testing with Real Vault

### Start Vault Dev Server

```bash
# Terminal 1: Start Vault in dev mode
vault server -dev -dev-root-token-id=root

# Output:
# ==> Vault server configuration:
#              Api Address: http://127.0.0.1:8200
#                      Cgo: disabled
#          Cluster Address: https://127.0.0.1:8201
#   Environment Variables: ...
#                Go Version: go1.21.3
#                   Listener 1: tcp (addr: "127.0.0.1:8200", ...)
# Root Token: root
```

### Configure AppRole

```bash
# Terminal 2: Configure Vault AppRole
export VAULT_ADDR='http://127.0.0.1:8200'
export VAULT_TOKEN='root'

# Enable AppRole auth method
vault auth enable approle

# Create a policy for erlmcp
vault policy write erlmcp-policy - <<EOF
path "secret/data/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}
path "secret/metadata/*" {
  capabilities = ["list", "read", "delete"]
}
EOF

# Create AppRole with the policy
vault write auth/approle/role/erlmcp \
    token_policies="erlmcp-policy" \
    token_ttl=1h \
    token_max_ttl=4h

# Get role_id
vault read auth/approle/role/erlmcp/role-id
# Output: role_id  12345678-1234-1234-1234-123456789012

# Generate secret_id
vault write -f auth/approle/role/erlmcp/secret-id
# Output: secret_id  abcdef12-3456-7890-abcd-ef1234567890
```

### Test with Erlang Shell

```bash
# Terminal 3: Start Erlang shell with erlmcp
rebar3 shell

# In Erlang shell:
1> Config = #{
    backend => vault,
    backend_config => #{
        url => <<"http://127.0.0.1:8200">>,
        auth_method => approle,
        role_id => <<"YOUR_ROLE_ID_HERE">>,
        secret_id => <<"YOUR_SECRET_ID_HERE">>,
        mount => <<"secret">>
    },
    ttl_seconds => 300
}.

2> {ok, Pid} = erlmcp_secrets:start_link(Config).
% Output: {ok,<0.123.0>}
% Logs: [info] Secrets manager started with backend: vault

3> erlmcp_secrets:set_secret(<<"db/password">>, <<"supersecret123">>).
% Output: ok
% Logs: [info] Vault SET succeeded for <<"db/password">>

4> erlmcp_secrets:get_secret(<<"db/password">>).
% Output: {ok,<<"supersecret123">>}

5> erlmcp_secrets:list_secrets().
% Output: {ok,[<<"db/password">>]}

6> erlmcp_secrets:rotate_secret(<<"api/key">>).
% Output: {ok,<<"dGVzdC1yYW5kb20tc2VjcmV0LXZhbHVl...">>}
% Logs: [info] Secret rotated: <<"api/key">>
%       [info] Vault SET succeeded for <<"api/key">>

7> erlmcp_secrets:delete_secret(<<"db/password">>).
% Output: ok
% Logs: [info] Vault DELETE succeeded for <<"db/password">>

8> erlmcp_secrets:get_secret(<<"db/password">>).
% Output: {error,not_found}
% Logs: [error] Vault GET failed for <<"db/password">>: not_found

9> erlmcp_secrets:stop().
% Output: ok
```

### Verify in Vault

```bash
# Check the secret was created in Vault
vault kv get secret/db/password  # Should return error (deleted)
vault kv get secret/api/key      # Should return the rotated value

# Check Vault audit logs
vault audit enable file file_path=/tmp/vault-audit.log
tail -f /tmp/vault-audit.log  # See all API calls from erlmcp
```

## 4. Error Scenarios

### Test Authentication Failure

```erlang
% Wrong secret_id
Config = #{
    backend => vault,
    backend_config => #{
        url => <<"http://127.0.0.1:8200">>,
        auth_method => approle,
        role_id => <<"YOUR_ROLE_ID">>,
        secret_id => <<"WRONG_SECRET_ID">>,
        mount => <<"secret">>
    }
}.
{ok, Pid} = erlmcp_secrets:start_link(Config).
erlmcp_secrets:get_secret(<<"any/key">>).
% Output: {error,{auth_failed,{http_error,400,<<"...">>}}}
% Logs: [error] Vault AppRole auth failed: ...
```

### Test Connection Failure

```erlang
% Vault not running
Config = #{
    backend => vault,
    backend_config => #{
        url => <<"http://localhost:9999">>,  % Wrong port
        auth_method => token,
        token => <<"test">>,
        mount => <<"secret">>
    }
}.
{ok, Pid} = erlmcp_secrets:start_link(Config).
erlmcp_secrets:get_secret(<<"any/key">>).
% Output: {error,{gun_open_failed,econnrefused}}
```

### Test Permission Denied

```erlang
% Token with insufficient permissions
Config = #{
    backend => vault,
    backend_config => #{
        url => <<"http://127.0.0.1:8200">>,
        auth_method => token,
        token => <<"limited-token">>,
        mount => <<"secret">>
    }
}.
{ok, Pid} = erlmcp_secrets:start_link(Config).
erlmcp_secrets:set_secret(<<"restricted/key">>, <<"value">>).
% Output: {error,{http_error,403,<<"...">>}}
% Logs: [error] Vault SET failed for <<"restricted/key">>: ...
```

## 5. Production Configuration

### sys.config

```erlang
[
    {erlmcp_core, [
        % ... other config ...
    ]},

    {erlmcp_secrets, [
        {backend, vault},
        {backend_config, #{
            url => <<"https://vault.production.com">>,
            auth_method => approle,
            % Load from environment variables
            role_id => {env_var, "VAULT_ROLE_ID"},
            secret_id => {env_var, "VAULT_SECRET_ID"},
            mount => <<"secret">>,
            namespace => <<"production">>,
            timeout => 10000
        }},
        {ttl_seconds, 600}  % Cache for 10 minutes
    ]}
].
```

### Environment Variables

```bash
export VAULT_ROLE_ID="prod-app-role-id"
export VAULT_SECRET_ID="prod-app-secret-id"

# Start application
rebar3 as prod release
_build/prod/rel/erlmcp/bin/erlmcp foreground
```

### Kubernetes Deployment

For Kubernetes with Vault Agent Injector, use kubernetes auth:

```erlang
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        url => <<"https://vault.default.svc.cluster.local:8200">>,
        auth_method => kubernetes,
        k8s_jwt_path => <<"/var/run/secrets/kubernetes.io/serviceaccount/token">>,
        k8s_role => <<"erlmcp-app">>,
        mount => <<"secret">>,
        namespace => <<"apps/production">>
    }},
    {ttl_seconds, 300}
]}
```

## 6. Monitoring & Observability

### Logs

All Vault operations are logged:

```erlang
% Success logs
[info] Secrets manager started with backend: vault
[info] Vault SET succeeded for <<"db/password">>
[info] Secret rotated: <<"api/key">>
[info] Vault DELETE succeeded for <<"old/key">>

% Error logs
[error] Vault GET failed for <<"missing/key">>: not_found
[error] Vault AppRole auth failed: invalid_credentials
[error] Failed to parse Vault response: ...
```

### Metrics (via erlmcp_observability)

Monitor Vault operations:

```erlang
% Get metrics
erlmcp_metrics:get(vault_requests_total).
erlmcp_metrics:get(vault_auth_failures_total).
erlmcp_metrics:get(vault_request_duration_ms).
```

### Health Checks

```erlang
% Check if Vault backend is healthy
case erlmcp_secrets:get_secret(<<"health/check">>) of
    {ok, _} -> healthy;
    {error, _} -> degraded
end.
```

## 7. Performance Benchmarks

Expected performance (measured with erlmcp_bench):

- **AppRole login**: ~50-100ms (one-time per token TTL)
- **Secret GET (cached)**: <1ms (ETS lookup)
- **Secret GET (uncached)**: ~10-30ms (HTTP round-trip)
- **Secret SET**: ~15-40ms (HTTP round-trip)
- **Throughput**: ~1000 ops/sec (limited by network I/O)

### Run Benchmarks

```bash
rebar3 shell

% Benchmark secret operations
erlmcp_bench:run_vault_benchmark(1000).  % 1000 operations

% Output:
% Vault Benchmark Results:
%   Operations: 1000
%   Duration: 1.234s
%   Throughput: 810 ops/sec
%   Latency p50: 12ms
%   Latency p95: 28ms
%   Latency p99: 45ms
```

## 8. Troubleshooting

### Issue: "gun_open_failed"

**Cause**: Cannot connect to Vault
**Solution**:
- Verify Vault URL is correct
- Check Vault is running: `curl http://localhost:8200/v1/sys/health`
- Check firewall/network connectivity

### Issue: "auth_failed"

**Cause**: Invalid credentials
**Solution**:
- Verify role_id and secret_id are correct
- Check AppRole is enabled: `vault auth list`
- Verify AppRole exists: `vault read auth/approle/role/erlmcp`

### Issue: "http_error, 403"

**Cause**: Insufficient permissions
**Solution**:
- Check policy attached to AppRole
- Verify path in policy matches secret path
- Test with Vault CLI: `vault kv get secret/test`

### Issue: "secret_value_not_found"

**Cause**: Secret structure doesn't match KV v2 format
**Solution**:
- Ensure using KV v2 engine
- Check mount path: `vault secrets list`
- Verify secret structure: `vault kv get -format=json secret/key`

## 9. Next Steps

1. ✅ Compilation successful
2. ✅ Unit tests passing
3. ✅ Integration tests with Vault
4. → Add to CI/CD pipeline
5. → Production deployment
6. → Monitor metrics and logs

## References

- [Vault AppRole Docs](https://www.vaultproject.io/docs/auth/approle)
- [Vault KV v2 API](https://www.vaultproject.io/api-docs/secret/kv/kv-v2)
- [erlmcp Secrets Management](docs/SECRETS_MANAGEMENT.md)
- [Implementation Summary](VAULT_IMPLEMENTATION_SUMMARY.md)
