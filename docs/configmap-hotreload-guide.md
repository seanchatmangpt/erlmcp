# ErlMCP v3 ConfigMap Hot-Reload Guide

## Overview

The ErlMCP v3 configuration system provides centralized configuration management with hot-reload capabilities, allowing you to update configuration without restarting pods.

## Features

- **Centralized Configuration**: Single source of truth for all configuration
- **Environment-Specific Overrides**: Separate configurations for dev/staging/production
- **Feature Flags System**: Runtime toggleable features with percentage-based rollout
- **Hot-Reload**: Update configuration without pod restarts
- **Validation**: Configuration validation before applying changes
- **Rollback Support**: Automatic rollback on configuration errors

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Kubernetes                              │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │                   ConfigMap                               │ │
│  │  - erlmcp.conf (main configuration)                      │ │
│  │  - vm.args (VM flags)                                    │ │
│  │  - feature-flags.json (feature toggles)                  │ │
│  │  - env-{dev,staging,prod}.conf (overrides)               │ │
│  └───────────────────────────────────────────────────────────┘ │
│                           │                                   │
│                           ▼                                   │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │                   Pod                                     │ │
│  │  ┌─────────────────┐      ┌───────────────────────────┐  │ │
│  │  │ erlmcp container │◄─────│ config-reloader sidecar  │  │ │
│  │  │                  │      │ - Watches ConfigMap      │  │ │
│  │  │ - Mounts ConfigMap      │ - Triggers reload        │  │ │
│  │  │ - Listens for reload     │ - Validates changes      │  │ │
│  │  └─────────────────┘      └───────────────────────────┘  │ │
│  └───────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Configuration Files

### Main Configuration (erlmcp.conf)

The main configuration file in Erlang term format with environment variable substitution:

```erlang
[
  {erlmcp, [
    {log_level, ${LOG_LEVEL:-info}},
    {server, #{
      max_subscriptions_per_resource => ${MAX_SUBSCRIPTIONS:-5000}
    }}
  ]}
].
```

### VM Arguments (vm.args)

Erlang VM flags for performance tuning:

```erlang
-name erlmcp@${ERLMCP_NODE_NAME:-erlmcp.erlmcp.svc.cluster.local}
+P ${ERL_PROCESS_LIMIT:-262144}
+sbt db
```

### Feature Flags (feature-flags.json)

JSON-based feature flag configuration:

```json
{
  "version": "3.0.0",
  "flags": {
    "observability": {
      "metrics.enabled": true,
      "telemetry.enabled": true
    },
    "experimental": {
      "new_api": {
        "state": "rollout",
        "rollout_percentage": 25
      }
    }
  }
}
```

## Environment-Specific Configuration

Each environment has its own ConfigMap for overrides:

### Development
```bash
kubectl apply -f k8s/configmap.yaml -n erlmcp-dev
```

**Key settings:**
- LOG_LEVEL=debug
- AUTH_ENABLED=false
- CLUSTER_ENABLED=false

### Staging
```bash
kubectl apply -f k8s/configmap.yaml -n erlmcp-staging
```

**Key settings:**
- LOG_LEVEL=info
- AUTH_ENABLED=true
- OTEL_SAMPLER_PROBABILITY=0.5

### Production
```bash
kubectl apply -f k8s/configmap.yaml -n erlmcp
```

**Key settings:**
- LOG_LEVEL=info
- AUTH_ENABLED=true
- OTEL_SAMPLER_PROBABILITY=0.1

## Hot-Reload Usage

### Option 1: Manual Trigger via Script

```bash
# Reload current ConfigMap in production
./scripts/feature-flags/configmap-hotreload.sh erlmcp

# Apply new config and reload
./scripts/feature-flags/configmap-hotreload.sh -c new-config.yaml erlmcp

# Dry-run to see what would happen
./scripts/feature-flags/configmap-hotreload.sh -d -c new-config.yaml erlmcp
```

### Option 2: Automatic Watch Mode

```bash
# Watch for ConfigMap changes every 10 seconds
./scripts/feature-flags/configmap-hotreload.sh -w -i 10 erlmcp
```

### Option 3: Kubernetes CronJob

The cluster-wide CronJob automatically checks for changes every 5 minutes:

```bash
kubectl apply -f k8s/configmap-hotreload-deployment.yaml
```

### Option 4: Direct API Call

Trigger reload via HTTP endpoint from within the cluster:

```bash
kubectl exec -n erlmcp pod/erlmcp-cluster-0 -- \
  curl -X POST http://localhost:8081/admin/reload-config
```

## Feature Flags

### Enable/Disable Features

```erlang
% Check if feature is enabled
erlmcp_feature_flags:is_enabled(<<"observability.metrics.enabled">>).

% Enable a feature
erlmcp_feature_flags:set_flag(<<"new_feature">>, true).

% Disable a feature
erlmcp_feature_flags:set_flag(<<"old_feature">>, false).
```

### Canary Rollout

```erlang
% Set up canary rollout
erlmcp_feature_flags:set_flag(
    <<"experimental.new_api">>,
    false,
    #{
        state => rollout,
        rollout_percentage => 10,
        rollout_strategy => canary
    }
).

% Check if rollout applies to user
erlmcp_feature_flags:evaluate_rollout(
    <<"experimental.new_api">>,
    <<"user-12345">>
).
```

### Conditional Rollout

```erlang
% User segment-based rollout
erlmcp_feature_flags:set_flag(
    <<"experimental.ui_redesign">>,
    false,
    #{
        state => rollout,
        rollout_percentage => 50,
        rollout_strategy => user_segment,
        conditions => #{
            user_segments => [beta_testers, employees]
        }
    }
).
```

## Configuration Validation

### Validate Before Applying

```erlang
Config = #{
    <<"log_level">> => info,
    <<"server">> => #{
        <<"max_subscriptions">> => 5000
    }
},

case erlmcp_config_manager:validate_config(Config) of
    ok ->
        erlmcp_config_manager:import_config(Config);
    {error, Errors} ->
        logger:error("Config validation failed: ~p", [Errors])
end.
```

### Schema Definition

Configuration schemas define valid types and constraints:

```erlang
get_schema() ->
    #{
        <<"log_level">> => #{
            type => atom,
            enum => [debug, info, warning, error]
        },
        <<"server">> => #{
            type => map,
            properties => #{
                <<"max_subscriptions">> => #{
                    type => integer,
                    min => 1
                }
            }
        }
    }.
```

## Troubleshooting

### ConfigMap Not Reflecting

Check the resource version:

```bash
kubectl get configmap erlmcp-config -n erlmcp -o jsonpath='{.metadata.resourceVersion}'
```

### Pods Not Reloading

1. Check if config-reloader sidecar is running:
```bash
kubectl logs -n erlmcp pod/erlmcp-cluster-0 -c config-reloader
```

2. Manual trigger:
```bash
kubectl exec -n erlmcp pod/erlmcp-cluster-0 -c erlmcp -- \
  erl -noshell -s erlmcp_config reload_config -s init stop
```

### Invalid Configuration

View validation errors:

```bash
kubectl logs -n erlmcp pod/erlmcp-cluster-0 | grep "validation"
```

### Rollback on Error

The system automatically rolls back on validation errors. To manually revert:

```bash
kubectl rollout undo statefulset/erlmcp-cluster -n erlmcp
```

## Best Practices

1. **Use Environment Variables**: Sensitive values should use environment variables and secrets
2. **Validate Before Applying**: Always validate configuration in staging first
3. **Gradual Rollout**: Use feature flags for gradual feature rollout
4. **Monitor After Changes**: Check logs and metrics after configuration changes
5. **Document Changes**: Keep track of configuration changes in version control
6. **Test Hot-Reload**: Verify hot-reload works in staging before production

## API Reference

### erlmcp_config_manager

| Function | Description |
|----------|-------------|
| `start_link/0` | Start the configuration manager |
| `reload_config/0` | Reload configuration from all sources |
| `get_config/0` | Get entire configuration |
| `get_config/1` | Get configuration at path |
| `set_config/2` | Set configuration value |
| `subscribe/1` | Subscribe to configuration changes |
| `validate_config/1` | Validate configuration against schema |

### erlmcp_feature_flags

| Function | Description |
|----------|-------------|
| `load_flags/0` | Load feature flags |
| `get_flag/1` | Get feature flag value |
| `set_flag/2` | Set feature flag value |
| `is_enabled/1` | Check if feature is enabled |
| `evaluate_rollout/2` | Evaluate rollout for identifier |
| `list_flags/0` | List all feature flags |
| `watch_config/0` | Start watching for changes |

## Migration Guide

### From v2 to v3

1. Update ConfigMap structure:
```bash
kubectl apply -f k8s/configmap.yaml
```

2. Add hot-reload sidecar to StatefulSet:
```bash
kubectl apply -f k8s/configmap-hotreload-deployment.yaml
```

3. Verify configuration loading:
```bash
kubectl logs -n erlmcp pod/erlmcp-cluster-0 | grep "Configuration manager"
```

4. Test hot-reload:
```bash
./scripts/feature-flags/configmap-hotreload.sh erlmcp
```
