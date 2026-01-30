# Configuration Schema

Complete configuration schema for erlmcp. This document covers all configuration options, their types, defaults, and usage examples.

## Configuration Overview

erlmcp uses a hierarchical configuration system with environment variables, configuration files, and runtime overrides.

### Configuration Hierarchy
1. **Defaults** - Built-in defaults
2. **Environment Variables** - Override with environment
3. **Configuration Files** - `config/sys.config` and `config/vm.args`
4. **Runtime Configuration** - Dynamic updates

## Top-Level Configuration

### Basic Configuration
```erlang
[
    {erlmcp,
        [
            % Basic settings
            {name, "erlmcp"},
            {version, "0.6.0"},
            {env, "production"},
            {log_level, info},

            % Transport configuration
            {transport, tcp},
            {host, "0.0.0.0"},
            {port, 8080},
            {ssl, false},
            {ssl_opts, []},

            % Server configuration
            {server, [
                {max_connections, 1000},
                {timeout, 5000},
                {keep_alive, true},
                {backlog, 128}
            ]},

            % Client configuration
            {client, [
                {timeout, 5000},
                {reconnect_interval, 1000},
                {max_retries, 3},
                {retry_delay, 1000}
            ]},

            % Tools configuration
            {tools, []},
            {tools_path, "tools"},
            {auto_load_tools, true},

            % Registry configuration
            {registry, [
                {backend, gproc},
                {timeout, 5000},
                {cleanup_interval, 30000}
            ]},

            % Monitoring configuration
            {metrics_enabled, true},
            {metrics_port, 9090},
            {metrics_prefix, "erlmcp_"},
            {tracing_enabled, false},
            {tracing_level, info},

            % Security configuration
            {auth_method, none},
            {auth_timeout, 5000},
            {rate_limits, []},
            {allowed_hosts, []},

            % Performance configuration
            {performance, [
                {workers, 10},
                {queue_size, 10000},
                {pool_size, 100},
                {max_overflow, 200},
                {zero_copy, true}
            ]}
        ]
    }
].
```

## Transport Configuration

### TCP Transport
```erlang
[
    {erlmcp,
        [
            {transport, tcp},
            {host, "0.0.0.0"},
            {port, 8080},
            {socket_opts, [
                {backlog, 128},
                {nodelay, true},
                {reuseaddr, true},
                {packet, raw},
                {active, once},
                {buffer, 65536},
                {high_watermark, 1048576},
                {send_timeout, 5000}
            ]},
            {ssl, false},
            {ssl_opts, [
                {verify, verify_none},
                {versions, [tlsv1.2, tlsv1.3]},
                {ciphers, [...]},
                {secure_renegotiate, true}
            ]}
        ]
    }
].
```

### HTTP Transport
```erlang
[
    {erlmcp,
        [
            {transport, http},
            {host, "0.0.0.0"},
            {port, 8081},
            {ssl, false},
            {ssl_opts, []},
            {http_opts, [
                {max_connections, 5000},
                {max_keep_alive, 60},
                {timeout, 30000},
                {compress, true},
                {content_type, "application/json"},
                {headers, [
                    {"X-Frame-Options", "DENY"},
                    {"X-Content-Type-Options", "nosniff"}
                ]},
                {cors, [
                    {enabled, true},
                    {origins, ["*"]},
                    {methods, ["GET", "POST", "OPTIONS"]},
                    {headers, ["*"]}
                ]}
            ]},
            {websocket_opts, [
                {max_frame_size, 1048576},
                {compress, true},
                {idle_timeout, 30000},
                {max_queue, 1000}
            ]}
        ]
    }
].
```

### stdio Transport
```erlang
[
    {erlmcp,
        [
            {transport, stdio},
            {encoding, utf8},
            {mode, binary},
            {buffer_size, 65536},
            {timeout, 5000}
        ]
    }
].
```

## Server Configuration

### General Server Settings
```erlang
[
    {erlmcp,
        [
            {server, [
                % Basic settings
                {max_connections, 1000},
                {timeout, 5000},
                {keep_alive, true},
                {backlog, 128},

                % Protocol settings
                {max_request_size, 1048576},  % 1MB
                {max_response_size, 1048576}, % 1MB
                {compression, gzip},

                % Security settings
                {allowed_methods, [
                    <<"tools/list">>,
                    <<"tools/call">>,
                    <<"tools/info">>
                ]},

                % Rate limiting
                {global_rate_limit, {1000, 60}},  % 1000/min
                {tool_rate_limits, []},

                % Authentication
                {auth_required, false},
                {auth_timeout, 5000},

                % Monitoring
                {request_logging, true},
                {performance_metrics, true}
            ]}
        ]
    }
].
```

### Tool-Specific Configuration
```erlang
[
    {erlmcp,
        [
            {tools, [
                % Tool definitions
                {tool1, [
                    {name, "calculator"},
                    {handler, calculator_tool},
                    {config, [
                        {timeout, 5000},
                        {retries, 3},
                        {cache_enabled, true},
                        {cache_size, 1000}
                    ]}
                ]},
                {tool2, [
                    {name, "weather"},
                    {handler, weather_tool},
                    {config, [
                        {timeout, 10000},
                        {api_key, "your-api-key"},
                        {cache_enabled, false}
                    ]}
                ]}
            ]},

            % Tool loading settings
            {tools_path, "tools"},
            {auto_load_tools, true},
            {tool_search_paths, [
                "tools",
                "lib/tools",
                "deps/*/tools"
            ]},

            % Tool validation
            {validate_tools, true},
            {strict_tool_names, true}
        ]
    }
].
```

## Client Configuration

### Client Settings
```erlang
[
    {erlmcp,
        [
            {client, [
                % Connection settings
                {timeout, 5000},
                {reconnect, true},
                {reconnect_interval, 1000},
                {max_retries, 3},
                {retry_delay, 1000},
                {connection_pool_size, 10},

                % Request settings
                {max_batch_size, 100},
                {stream_timeout, 30000},

                % Authentication
                {auth_token, undefined},
                {auth_type, none},

                % Performance
                {async_requests, true},
                {pipeline_requests, true},
                {pipeline_depth, 10}
            ]}
        ]
    }
].
```

### Client Pool Configuration
```erlang
[
    {erlmcp,
        [
            {pools, [
                {default, [
                    {size, 10},
                    {max_overflow, 20},
                    {max_waiting, 500},
                    {worker_timeout, 5000},
                    {strategy, lifo}
                ]},
                {high_priority, [
                    {size, 5},
                    {max_overflow, 10},
                    {max_waiting, 100},
                    {strategy, fifo}
                ]}
            ]}
        ]
    }
].
```

## Registry Configuration

### Registry Settings
```erlang
[
    {erlmcp,
        [
            {registry, [
                % Backend selection
                {backend, gproc},  % or ets, mnesia

                % Performance settings
                {cleanup_interval, 30000},
                {max_ttl, 3600000},  % 1 hour
                {cache_size, 1000},
                {cache_ttl, 30000},

                % Clustering
                {cluster_enabled, false},
                {cluster_nodes, []},
                {sync_interval, 5000},

                % Monitoring
                {metrics_enabled, true},
                {stats_interval, 10000}
            ]}
        ]
    }
].
```

### gproc Registry Configuration
```erlang
[
    {erlmcp,
        [
            {registry, [
                {backend, gproc},
                {gproc_opts, [
                    {type, l},
                    {scope, local}
                ]},
                {cleanup, [
                    {enabled, true},
                    {interval, 30000},
                    {stale_ttl, 60000}
                ]}
            ]}
        ]
    }
].
```

## Security Configuration

### Authentication Configuration
```erlang
[
    {erlmcp,
        [
            % Authentication settings
            {auth_method, none},  % none, basic, jwt, oauth2
            {auth_timeout, 5000},
            {auth_cache_size, 1000},
            {auth_cache_ttl, 300000},  % 5 minutes

            % Basic authentication
            {auth_basic, [
                {enabled, false},
                {realm, "erlmcp"},
                {users, [
                    {"user1", "password1"},
                    {"user2", "password2"}
                ]}
            ]},

            % JWT authentication
            {auth_jwt, [
                {enabled, false},
                {secret, "your-secret"},
                {issuer, "erlmcp"},
                {algorithm, hs256},
                {expires_in, 3600},
                {claims, []}
            ]},

            % OAuth2 authentication
            {auth_oauth2, [
                {enabled, false},
                {provider, "google"},
                {client_id, "your-client-id"},
                {client_secret, "your-client-secret"},
                {redirect_uri, "http://localhost:8080/callback"},
                {scopes, ["openid", "profile", "email"]}
            ]},

            % API key authentication
            {auth_api_key, [
                {enabled, false},
                {header, "X-API-Key"},
                {prefix, "Bearer "},
                {keys, [
                    {"key1", "user1"},
                    {"key2", "user2"}
                ]}
            ]}
        ]
    }
].
```

### Authorization Configuration
```erlang
[
    {erlmcp,
        [
            % Authorization settings
            {auth_required, false},
            {auth_anonymous_allowed, true},
            {auth_role_mapping, []},

            % Access control
            {access_control, [
                {enabled, false},
                {rules, [
                    {allow, [
                        {pattern, "/tools/*"},
                        {roles, ["user", "admin"]}
                    ]},
                    {deny, [
                        {pattern, "/tools/admin/*"},
                        {roles, ["user"]}
                    ]}
                ]}
            ]},

            % Security headers
            {security_headers, [
                {"X-Content-Type-Options", "nosniff"},
                {"X-Frame-Options", "DENY"},
                {"X-XSS-Protection", "1; mode=block"},
                {"Strict-Transport-Security", "max-age=31536000; includeSubDomains"}
            ]}
        ]
    }
].
```

## Performance Configuration

### Performance Settings
```erlang
[
    {erlmcp,
        [
            {performance, [
                % General settings
                {workers, 10},
                {queue_size, 10000},
                {pool_size, 100},
                {max_overflow, 200},
                {zero_copy, true},

                % Connection settings
                {connection_timeout, 5000},
                {keep_alive, true},
                {tcp_opts, [
                    {nodelay, true},
                    {buffer, 65536},
                    {high_watermark, 1048576}
                ]},

                % Memory settings
                {memory_monitor, true},
                {memory_limit, 1024*1024*1024},  % 1GB
                {gc_interval, 1000},  % GC every second

                % CPU settings
                {scheduler_width, all},
                {async_threads, 64},
                {process_limit, 65536}
            ]}
        ]
    }
].
```

### Load Balancing Configuration
```erlang
[
    {erlmcp,
        [
            {load_balancer, [
                {strategy, round_robin},  % round_robin, least_connections, weighted
                {health_check_interval, 10000},
                {health_check_timeout, 5000},
                {max_workers, 100},
                {worker_timeout, 30000},

                % Weighted load balancing
                {weights, [
                    {worker1, 2},
                    {worker2, 1},
                    {worker3, 1}
                ]}
            ]}
        ]
    }
].
```

## Monitoring Configuration

### Metrics Configuration
```erlang
[
    {erlmcp,
        [
            % Metrics settings
            {metrics_enabled, true},
            {metrics_port, 9090},
            {metrics_prefix, "erlmcp_"},
            {metrics_interval, 1000},
            {metrics_backend, prometheus},  % prometheus, exometer

            % Metric definitions
            {metrics, [
                {requests_total, counter},
                {requests_duration_seconds, histogram},
                {errors_total, counter},
                {active_connections, gauge},
                {tools_calls, counter}
            ]},

            % Prometheus settings
            {prometheus, [
                {registry, default},
                {collect_interval, 1000},
                {metrics_path, "/metrics"},
                {auth_token, undefined}
            ]},

            % Exometer settings
            {exometer, [
                {reporters, [
                    {exometer_prometheus, [
                        {prefix, "erlmcp"},
                        {labels, []}
                    ]}
                ]}
            ]}
        ]
    }
].
```

### Tracing Configuration
```erlang
[
    {erlmcp,
        [
            % Tracing settings
            {tracing_enabled, false},
            {tracing_level, info},
            {tracing_sampling_rate, 1.0},  % 0.0 to 1.0
            {tracing_exporter, jaeger},  % jaeger, zipkin, none

            % Jaeger settings
            {jaeger, [
                {service_name, "erlmcp"},
                {sampler, [
                    {type, const},
                    {param, 1}
                ]},
                {reporter, [
                    {host, "localhost"},
                    {port, 6831}
                ]}
            ]},

            % Zipkin settings
            {zipkin, [
                {base_url, "http://localhost:9411"},
                {sample_rate, 1.0}
            ]}
        ]
    }
].
```

## Logging Configuration

### Logging Settings
```erlang
[
    {logger,
        [
            {handler, default, logger_std_h,
                {formatter,
                    logger_formatter,
                    #{single_line => false,
                      template => [time, " ", level, " ", msg, "\n"]}}},
            {handler, file, logger_std_h,
                {file, "/var/log/erlmcp/erlmcp.log"},
                {formatter, logger_formatter, #{}}},
            {handler, syslog, logger_syslog_h,
                {type, udp},
                {facility, local0}}
        ]
    },

    {erlmcp,
        [
            {log_level, info},
            {log_format, text},  % text, json
            {log_dir, "/var/log/erlmcp"},
            {log_rotate_size, 10485760},  % 10MB
            {log_rotate_count, 5},
            {log_metadata, true},

            % Access logging
            {access_log, [
                {enabled, true},
                {file, "/var/log/erlmcp/access.log"},
                {format, combined},  % combined, common, json
                {fields, [
                    {timestamp, true},
                    {method, true},
                    {path, true},
                    {status, true},
                    {response_time, true},
                    {client_ip, true}
                ]}
            ]}
        ]
    }
].
```

## VM Configuration

### vm.args Settings
```erlang
% vm.args
-name erlmcp@127.0.0.1
-setcookie erlmcp
-args_file config/vm.args

% Process and memory settings
+P 65536      % Process limit
+A 64         % Async threads
-env ERL_MAX_PORTS 65536

% Distribution settings
-kernel inet_dist_listen_min 9100
-kernel inet_dist_listen_max 9109
-kernel inet_dist_use_name false

% Memory settings
+hr 32768     % Heap size 32MB
+hm 32768     % Heap size 32MB
+hb 32768     % Binary heap size

% GC settings
+pc unicode   % Enable Unicode support
+sbwt none    % No spin wait
+spp true     % Enable SMP

% Crash dump settings
-env ERL_CRASH_DUMP /var/log/erlmcp/crash.dump
+ct 20        % Maximum number of crash dump files
```

### sys.config VM Settings
```erlang
[
    {kernel, [
        {inet_dist_use_name, false},
        {inet_dist_listen_min, 9100},
        {inet_dist_listen_max, 9109},
        {error_logger, {file, "/var/log/erlmcp/error.log"}},
        {sync_nodes_mandatory, []},
        {sync_nodes_timeout, 5000}
    ]},

    {sasl, [
        {sasl_error_logger, {file, "/var/log/erlmcp/sasl.log"}},
        {errlog_type, error},
        {sasl_error_logger_mf_dir, "/var/log/erlmcp/sasl-logs"},
        {sasl_error_logger_mf_maxbytes, 10485760},
        {sasl_error_logger_mf_maxfiles, 5}
    ]},

    {stdlib, [
        {shell_history_size, 1000}
    ]}
].
```

## Environment Variables

### Common Environment Variables
```bash
# Basic settings
export ERLMCP_NAME="erlmcp"
export ERLMCP_ENV="production"
export ERLMCP_LOG_LEVEL="info"

# Transport settings
export ERLMCP_TRANSPORT="tcp"
export ERLMCP_HOST="0.0.0.0"
export ERLMCP_PORT="8080"
export ERLMCP_SSL="false"

# Server settings
export ERLMCP_MAX_CONNECTIONS="1000"
export ERLMCP_TIMEOUT="5000"

# Client settings
export ERLMCP_CLIENT_TIMEOUT="5000"
export ERLMCP_CLIENT_RECONNECT="true"
export ERLMCP_CLIENT_MAX_RETRIES="3"

# Metrics settings
export ERLMCP_METRICS_ENABLED="true"
export ERLMCP_METRICS_PORT="9090"
export ERLMCP_METRICS_PREFIX="erlmcp_"

# Security settings
export ERLMCP_AUTH_METHOD="none"
export ERLMCP_AUTH_SECRET="your-secret"

# Performance settings
export ERLMCP_WORKERS="10"
export ERLMCP_QUEUE_SIZE="10000"
export ERLMCP_POOL_SIZE="100"

# Logging settings
export ERLMCP_LOG_DIR="/var/log/erlmcp"
export ERLMCP_LOG_LEVEL="info"

# GCP settings
export GCP_PROJECT_ID="your-project-id"
export GCP_CREDENTIALS_FILE="/path/to/credentials.json"
export GCP_REGION="us-central1"
```

### Environment Variable to Config Mapping

| Environment Variable | Config Path | Type |
|---------------------|-------------|------|
| `ERLMCP_TRANSPORT` | `[erlmcp, transport]` | atom() |
| `ERLMCP_HOST` | `[erlmcp, host]` | string() |
| `ERLMCP_PORT` | `[erlmcp, port]` | integer() |
| `ERLMCP_SSL` | `[erlmcp, ssl]` | boolean() |
| `ERLMCP_SSL_OPTS_KEY` | `[erlmcp, ssl_opts, keyfile]` | string() |
| `ERLMCP_MAX_CONNECTIONS` | `[erlmcp, server, max_connections]` | integer() |
| `ERLMCP_TIMEOUT` | `[erlmcp, client, timeout]` | integer() |
| `ERLMCP_METRICS_ENABLED` | `[erlmcp, metrics_enabled]` | boolean() |

## Configuration Validation

### Validation Rules
```erlang
% Configuration validation
validate_config(Config) ->
    % Validate required fields
    case validate_required_fields(Config) of
        ok -> validate_types(Config);
        Error -> Error
    end.

validate_required_fields(Config) ->
    Required = [
        {port, integer},
        {transport, atom},
        {log_level, atom}
    ],
    validate_fields(Config, Required).

validate_types(Config) ->
    % Validate field types and ranges
    case validate_port(Config) of
        ok -> validate_ssl_config(Config);
        Error -> Error
    end.
```

### Example Validation Error
```erlang
% Example validation result
{error,
    {validation_failed,
        [
            {port, "must be between 1 and 65535"},
            {ssl_opts, "missing required key: certfile"}
        ]
    }
}.
```

## Configuration Examples

### Production Configuration
```erlang
[
    {erlmcp,
        [
            % Production settings
            {name, "erlmcp-prod"},
            {env, "production"},
            {log_level, info},

            % High-performance TCP
            {transport, tcp},
            {host, "0.0.0.0"},
            {port, 8080},
            {socket_opts, [
                {backlog, 128},
                {nodelay, true},
                {reuseaddr, true},
                {buffer, 65536}
            ]},

            % Optimized server
            {server, [
                {max_connections, 5000},
                {timeout, 3000},
                {keep_alive, true},
                {compression, gzip}
            ]},

            % High-performance client
            {client, [
                {timeout, 3000},
                {reconnect, true},
                {max_retries, 3},
                {pipeline_requests, true}
            ]},

            % Production monitoring
            {metrics_enabled, true},
            {metrics_port, 9090},
            {tracing_enabled, true},
            {access_log, enabled}
        ]
    }
].
```

### Development Configuration
```erlang
[
    {erlmcp,
        [
            % Development settings
            {name, "erlmcp-dev"},
            {env, "development"},
            {log_level, debug},

            % stdio for development
            {transport, stdio},

            % Development server
            {server, [
                {max_connections, 100},
                {timeout, 10000},
                {keep_alive, false}
            ]},

            % Development client
            {client, [
                {timeout, 10000},
                {reconnect, false}
            ]},

            % Development monitoring
            {metrics_enabled, false},
            {tracing_enabled, false}
        ]
    }
].
```

### Testing Configuration
```erlang
[
    {erlmcp,
        [
            % Test settings
            {name, "erlmcp-test"},
            {env, "test"},
            {log_level, debug},

            % Test server
            {transport, tcp},
            {host, "127.0.0.1"},
            {port, 8081},

            % Test client
            {client, [
                {timeout, 5000},
                {reconnect, false},
                {max_retries, 1}
            ]},

            % Test tools
            {tools, [
                {test_tool, [
                    {name, "test"},
                    {handler, test_tool},
                    {config, [test_mode]}
                ]}
            ]}
        ]
    }
].
```

---

**Related Documentation:**
- [Environment Variables](./environment.md) - Environment-specific configuration
- [Validation Rules](./validation.md) - Configuration validation
- [Configuration Files](../howto/installation.md#configuration) - File-based configuration