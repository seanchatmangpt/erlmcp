# erlmcp v3.0.0 Component Dependency Matrix
## Internal & External Dependencies

**Version**: 3.0.0
**Date**: 2026-01-31
**OTP Requirement**: 28.3.1+

---

## Overview

This document provides a complete dependency matrix for all erlmcp v3.0.0 components, including:
- Internal application dependencies
- External library dependencies
- Optional dependencies
- Version constraints
- License information

---

## Internal Application Dependencies

### Dependency Graph

```
┌─────────────────────────────────────────────────────────────┐
│                   FOUNDATION LAYER                          │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │         erlmcp_core (108 modules)                    │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ NO INTERNAL DEPENDENCIES                        │  │  │
│  │  │ - JSON-RPC 2.0                                  │  │  │
│  │  │ - MCP protocol engine                           │  │  │
│  │  │ - Client/Server gen_servers                     │  │  │
│  │  │ - Registry (gproc-based)                        │  │  │
│  │  │ - Session management                            │  │  │
│  │  │ - Protection (circuit breaker, rate limiter)    │  │  │
│  │  │ - MCP 2025-11-25 features                       │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────┘  │
│                           │                                  │
│                           │ Depends on                       │
│                           ▼                                  │
└─────────────────────────────────────────────────────────────┘
        │                               │               │
        │                               │               │
        ▼                               ▼               ▼
┌───────────────┐           ┌──────────────┐   ┌──────────────┐
│ erlmcp_...   │           │ erlmcp_...  │   │ erlmcp_...  │
│transports     │           │observability│   │ validation   │
│(28 modules)   │           │ (38 modules)│   │ (16 modules) │
│               │           │              │   │              │
│Depends:       │           │Depends:      │   │Depends:      │
│- core         │           │- core        │   │- core        │
│- gun          │           │- OTEL libs   │   │- transports  │
│- ranch        │           │              │   │              │
│- poolboy      │           │              │   │              │
│- cowboy       │           │              │   │              │
└───────────────┘           └──────────────┘   └──────────────┘
        │                           │               │
        └───────────────────────────┴───────────────┘
                                    │
                                    │ [OPTIONAL]
                                    ▼
                        ┌──────────────────────┐
                        │   tcps_erlmcp        │
                        │   (63 modules)       │
                        │                      │
                        │ Depends:             │
                        │ - core               │
                        │ - observability      │
                        │ - bbmustache         │
                        │ - ontology libs      │
                        └──────────────────────┘
```

### Dependency Rules

| Application | Internal Dependencies | External Dependencies | Optional? |
|-------------|----------------------|----------------------|-----------|
| **erlmcp_core** | None (foundation) | gproc, jesse, jose | No (required) |
| **erlmcp_transports** | erlmcp_core | gun, ranch, poolboy, cowboy | No (required) |
| **erlmcp_observability** | erlmcp_core | opentelemetry_* | No (required) |
| **erlmcp_validation** | erlmcp_core, erlmcp_transports | jesse | No (required) |
| **tcps_erlmcp** | erlmcp_core, erlmcp_observability | bbmustache, jobs, fs | Yes (optional) |

### Application Modules

**erlmcp_core** (108 modules):
- Protocol: `erlmcp_json_rpc`, `erlmcp_capabilities`, `erlmcp_resources`, `erlmcp_tools`, `erlmcp_prompts`
- Client/Server: `erlmcp_client`, `erlmcp_server`, `erlmcp_client_sup`, `erlmcp_server_sup`
- Registry: `erlmcp_registry`, `erlmcp_registry_utils`, `erlmcp_registry_distributed`
- Sessions: `erlmcp_session`, `erlmcp_session_manager`, `erlmcp_session_ets`, `erlmcp_session_dets`, `erlmcp_session_mnesia`, `erlmcp_session_replicator`, `erlmcp_session_failover`
- Protection: `erlmcp_circuit_breaker`, `erlmcp_rate_limiter`, `erlmcp_connection_limiter`, `erlmcp_connection_monitor`, `erlmcp_memory_monitor`, `erlmcp_cpu_quota`
- Cache: `erlmcp_cache`, `erlmcp_icon_cache`, `erlmcp_sse_event_store`, `erlmcp_cache_warmer_sup`
- MCP 2025-11-25: `erlmcp_cancellation`, `erlmcp_pagination`, `erlmcp_completion`, `erlmcp_elicitation`, `erlmcp_roots_server`, `erlmcp_apps_server`, `erlmcp_notification_handler_sup`
- Infrastructure: `erlmcp_reload_sup`, `erlmcp_hooks`, `erlmcp_health`, `erlmcp_graceful_drain`
- Cluster: `erlmcp_cluster_sup`, `erlmcp_node_monitor`, `erlmcp_split_brain_detector`
- LLM: `erlmcp_llm_provider_anthropic`, `erlmcp_llm_provider_openai`, `erlmcp_llm_provider_local`, `erlmcp_mock_llm`
- Security: `erlmcp_auth`, `erlmcp_auth_rate_limiter`, `erlmcp_secrets`
- Utilities: `erlmcp_logging`, `erlmcp_message_handler`, `erlmcp_message_parser`, `erlmcp_message_size`, `erlmcp_path_canonicalizer`, `erlmcp_request_id`, `erlmcp_refusal`, `erlmcp_schema_registry`, `erlmcp_schema_validator`, `erlmcp_uri_validator`

**erlmcp_transports** (28 modules):
- Behavior: `erlmcp_transport_behavior`
- Implementations: `erlmcp_transport_stdio`, `erlmcp_transport_tcp`, `erlmcp_transport_http`, `erlmcp_transport_http_server`, `erlmcp_transport_ws`, `erlmcp_transport_sse`, `erlmcp_transport_sse_manager`
- Infrastructure: `erlmcp_transport_sup`, `erlmcp_transport_adapter`, `erlmcp_transport_pool`, `erlmcp_transport_pipeline`, `erlmcp_transport_registry`, `erlmcp_transport_discovery`
- Health: `erlmcp_transport_health`, `erlmcp_transport_validation`
- Security: `erlmcp_http_header_validator`, `erlmcp_origin_validator`, `erlmcp_security_headers`, `erlmcp_tls_validation`
- Pooling: `erlmcp_pool_manager`, `erlmcp_pool_strategy`
- OTP: `erlmcp_transports_app`, `erlmcp_transports_sup`

**erlmcp_observability** (38 modules):
- OpenTelemetry: `erlmcp_otel`, `erlmcp_otel_middleware`, `erlmcp_otel_datadog`, `erlmcp_otel_honeycomb`, `erlmcp_otel_jaeger`
- Tracing: `erlmcp_tracing`, `erlmcp_trace_analyzer`
- Metrics: `erlmcp_metrics`, `erlmcp_metrics_server`, `erlmcp_metrics_aggregator`, `erlmcp_metrology_validator`
- Dashboard: `erlmcp_dashboard_server`, `erlmcp_dashboard_http_handler`
- Monitoring: `erlmcp_health_monitor`, `erlmcp_process_monitor`, `erlmcp_debugger`, `erlmcp_profiler`, `erlmcp_memory_analyzer`
- Chaos: `erlmcp_chaos`, `erlmcp_chaos_network`, `erlmcp_chaos_process`, `erlmcp_chaos_resource`, `erlmcp_chaos_worker`, `erlmcp_chaos_worker_sup`, `erlmcp_recovery_manager`
- Audit: `erlmcp_audit_log`, `erlmcp_receipt_chain`, `erlmcp_evidence_path`
- Benchmark: `erlmcp_bench_rate_limit`
- OTP: `erlmcp_observability_app`, `erlmcp_observability_sup`

**erlmcp_validation** (16 modules):
- Validators: `erlmcp_protocol_validator`, `erlmcp_transport_validator`, `erlmcp_security_validator`, `erlmcp_performance_validator`, `erlmcp_spec_parser`, `erlmcp_uri_validator`
- Reporting: `erlmcp_compliance_report`, `erlmcp_compliance_report_html`, `erlmcp_compliance_report_json`
- Testing: `erlmcp_test_client`
- CLI: `erlmcp_validate_cli`
- Resources: `erlmcp_memory_manager`
- OTP: `erlmcp_validation_app`

---

## External Library Dependencies

### Core Dependencies (Required)

| Library | Version | Purpose | License | Used By |
|---------|---------|---------|---------|---------|
| **gproc** | 0.9.0 | Process registry | APL 2.0 | erlmcp_core |
| **jesse** | 1.8.1 | JSON Schema validation | Apache 2.0 | erlmcp_core, erlmcp_validation |
| **jose** | 1.11.1 | JWT validation | MIT | erlmcp_core (security) |
| **bbmustache** | 1.12.2 | Template engine | MIT | erlmcp_core (prompts) |
| **gun** | 2.0.1 | HTTP client | MIT | erlmcp_transports |
| **ranch** | 2.1.0 | TCP acceptor pools | APL 2.0 | erlmcp_transports |
| **poolboy** | 1.5.2 | Connection pooling | APL 2.0 | erlmcp_transports |
| **cowboy** | 2.10.0 | HTTP server | APL 2.0 | erlmcp_transports, erlmcp_observability |

### Observability Dependencies (Required)

| Library | Version | Purpose | License | Used By |
|---------|---------|---------|---------|---------|
| **opentelemetry_api** | 1.5.0 | OTEL API | Apache 2.0 | erlmcp_observability |
| **opentelemetry** | 1.7.0 | OTEL SDK | Apache 2.0 | erlmcp_observability |
| **opentelemetry_exporter** | 1.10.0 | OTEL exporters | Apache 2.0 | erlmcp_observability |

### Optional Dependencies (TCPS)

| Library | Version | Purpose | License | Used By |
|---------|---------|---------|---------|---------|
| **jobs** | Latest | Job queue | Apache 2.0 | tcps_erlmcp |
| **fs** | Latest | File system operations | Apache 2.0 | tcps_erlmcp |

### Test Dependencies (Test Profile Only)

| Library | Version | Purpose | License | Used By |
|---------|---------|---------|---------|---------|
| **proper** | 1.4.0 | Property-based testing | GPL3 | Test suite |
| **meck** | 0.9.2 | Mocking framework | Apache 2.0 | Test suite |
| **coveralls** | 2.2.0 | Coverage reporting | MIT | CI/CD |

### Development Dependencies (Dev Profile Only)

| Library | Version | Purpose | License | Used By |
|---------|---------|---------|---------|---------|
| **recon** | 2.5.3 | Runtime debugging | APL 2.0 | Development |
| **observer_cli** | 1.7.4 | CLI observer | APL 2.0 | Development |
| **rebar3_lint** | 3.0.1 | Linting | APL 2.0 | CI/CD |

### Removed Dependencies (v3.0.0 Cleanup)

| Library | Version | Reason | Replacement |
|---------|---------|--------|-------------|
| **jsx** | 3.1.0 | Replaced by OTP 28+ native json module | `json:encode/decode` |

---

## Dependency Version Constraints

### Erlang/OTP Version

**Minimum**: OTP 28.3.1 (exclusive - no backward compatibility)
**Recommended**: OTP 28.3.1 or later
**Tested**: OTP 28.3.1

**Reasoning**:
- Native `json` module requires OTP 27+
- Priority messages require OTP 28+
- Improved process iteration requires OTP 28+

### Library Version Constraints

```erlang
%% Critical security dependencies (strict versions)
{jesse, "1.8.1"},        % JSON Schema validation
{jose, "1.11.1"},         % JWT validation (security-critical)

%% Process registry (stable API)
{gproc, "0.9.0"},         % Extended process registry

%% Network libraries (stable versions)
{gun, "2.0.1"},           % HTTP client
{ranch, "2.1.0"},         % TCP acceptor pools
{poolboy, "1.5.2"},       % Connection pooling
{cowboy, "2.10.0"},       % HTTP server

%% OpenTelemetry (semantic versioning)
{opentelemetry_api, "1.5.0"},
{opentelemetry, "1.7.0"},
{opentelemetry_exporter, "1.10.0"},

%% Template engine
{bbmustache, "1.12.2"},   % Mustache templates
```

---

## Licensing Matrix

### Apache-2.0 Licensed (OSS)

All core components are Apache-2.0 licensed:

- ✅ erlmcp_core
- ✅ erlmcp_transports
- ✅ erlmcp_observability
- ✅ erlmcp_validation

### External Library Licenses

| Library | License | Compatible with Apache-2.0? |
|---------|---------|----------------------------|
| gproc | APL 2.0 | ✅ Yes |
| jesse | Apache 2.0 | ✅ Yes |
| jose | MIT | ✅ Yes |
| bbmustache | MIT | ✅ Yes |
| gun | MIT | ✅ Yes |
| ranch | APL 2.0 | ✅ Yes |
| poolboy | APL 2.0 | ✅ Yes |
| cowboy | APL 2.0 | ✅ Yes |
| opentelemetry_api | Apache 2.0 | ✅ Yes |
| opentelemetry | Apache 2.0 | ✅ Yes |
| opentelemetry_exporter | Apache 2.0 | ✅ Yes |
| proper | GPL3 | ⚠️ Test-only (okay) |
| meck | Apache 2.0 | ✅ Yes |
| coveralls | MIT | ✅ Yes |

**Note**: GPL3 (Proper) is test-only and not linked into production releases.

---

## Dependency Cleanup Actions

### Phase 1: Remove Unused Dependencies

**Check for unused dependencies**:
```bash
# Use rebar3 to analyze unused dependencies
rebar3 tree
rebar3 dialyzer
```

**Potential candidates for removal**:
- Any libraries not directly referenced in code
- Duplicate functionality (e.g., multiple JSON libraries)
- Test dependencies in production profile

### Phase 2: Consolidate Versions

**Action**: Ensure consistent versions across all apps

**Before** (example inconsistency):
```erlang
% erlmcp_core.app.src
{applications, [kernel, stdlib, gproc]},

% erlmcp_transports.app.src
{applications, [kernel, stdlib, gproc, gun]},
```

**After** (consistent):
```erlang
% All apps use rebar.config for dependency management
% Remove hard-coded versions from .app.src files
```

### Phase 3: Remove jsx Dependency

**Status**: Already removed in v3.0.0 codebase

**Verification**:
```bash
# Check for any remaining jsx references
grep -r "jsx" apps/ --include="*.erl" --include="*.app.src"

% Expected: No results (all replaced with json module)
```

**Migration example**:
```erlang
% Before (jsx)
Json = jsx:encode(Map),
Decoded = jsx:decode(Json).

% After (OTP 28+ json module)
Json = json:encode(Map),
Decoded = json:decode(Json).
```

### Phase 4: Update OTP Version Requirement

**rebar.config**:
```erlang
% BEFORE
{minimum_otp_vsn, "25.0"},

% AFTER
{minimum_otp_vsn, "28.3.1"},
```

**Verification**:
```bash
# Check OTP version in CI/CD
erl -version  # Must be 28.3.1+

# Use OTP 28+ features
{platform_define, "^28", 'OTP_28_PLUS'}
```

---

## Dependency Validation

### Pre-Commit Validation

**Checklist**:
- [ ] All .app.src files have consistent application lists
- [ ] No unused dependencies (verified via `rebar3 tree`)
- [ ] No version conflicts (verified via `rebar3 compile`)
- [ ] All licenses are Apache-2.0 compatible
- [ ] No GPL/AGPL libraries in production (test-only is okay)
- [ ] jsx is completely removed (use OTP 28+ json)
- [ ] OTP version is 28.3.1+ (exclusive)

### Automated Validation

**Script**: `scripts/validate_deps.sh`
```bash
#!/bin/bash
# Validate dependencies

echo "Checking for unused dependencies..."
rebar3 tree | grep "WARNING" && exit 1

echo "Checking for jsx references..."
grep -r "jsx" apps/ --include="*.erl" && exit 1

echo "Checking OTP version..."
erl -noshell -eval "V = erlang:system_info(otp_release),
  case V of
    \"28\" -> ok;
    _ -> halt(1)
  end"

echo "All dependency checks passed!"
```

---

## Dependency Maintenance

### Update Policy

**Stable Dependencies** (rare updates):
- gproc, jesse, jose (security-critical)
- gun, ranch, cowboy (networking)

**Active Dependencies** (regular updates):
- opentelemetry_* (active development)
- bbmustache (feature updates)

### Update Process

1. **Check version compatibility**:
   ```bash
   rebar3 upgrade package_name
   ```

2. **Run tests**:
   ```bash
   rebar3 eunit
   rebar3 ct
   ```

3. **Check for breaking changes**:
   - Read CHANGELOG
   - Check for API changes
   - Verify license compatibility

4. **Update rebar.config**:
   ```erlang
   {deps, [
       {package_name, "NewVersion"}
   ]}.
   ```

5. **Commit with justification**:
   ```
   Upgrade package_name from OldVersion to NewVersion

   Reason: Security patch / New feature / Bug fix
   Breaking changes: None / List changes
   Test results: All passing
   ```

---

## Conclusion

This dependency matrix provides:

✅ **Complete dependency graph** - Internal and external dependencies
✅ **Version constraints** - Specific versions for reproducibility
✅ **Licensing matrix** - All Apache-2.0 compatible
✅ **Cleanup plan** - Remove unused/duplicate dependencies
✅ **Maintenance guide** - How to update dependencies safely

**Next Steps**:
1. Execute cleanup plan (remove POC code, jsx)
2. Update version numbers to 3.0.0
3. Validate all dependencies
4. Tag v3.0.0-oss release

---

**Document Status**: ✅ Complete
**Related Documents**:
- `10_architecture_design_plan.md` - Overall v3.0.0 architecture
- `20_supervision_tree_v3.md` - Complete supervision hierarchy
