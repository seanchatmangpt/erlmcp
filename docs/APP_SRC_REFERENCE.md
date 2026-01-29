# ERLMCP .app.src Reference Guide

**Purpose**: Quick reference for app.src structure, patterns, and common issues
**Audience**: Erlang/OTP developers maintaining erlmcp
**Last Updated**: 2026-01-28

---

## Quick Navigation

- [File Locations](#file-locations)
- [App Structure](#app-structure)
- [Version Management](#version-management)
- [Dependencies](#dependencies)
- [Module Lists](#module-lists)
- [Registration](#registration)
- [Environment Config](#environment-config)
- [Maintenance Checklist](#maintenance-checklist)
- [Common Mistakes](#common-mistakes)

---

## File Locations

| Application | Path | Modules | Version |
|-------------|------|---------|---------|
| erlmcp_core | `/apps/erlmcp_core/src/erlmcp_core.app.src` | 45 | 2.1.0 |
| erlmcp_transports | `/apps/erlmcp_transports/src/erlmcp_transports.app.src` | 15 | 2.1.0 |
| erlmcp_observability | `/apps/erlmcp_observability/src/erlmcp_observability.app.src` | 26 | 2.1.0 |
| tcps_erlmcp | `/apps/tcps_erlmcp/src/tcps_erlmcp.app.src` | 59 | 2.1.0 |

---

## App Structure

### Minimal Template

```erlang
{application, app_name,
 [{description, "Clear description of app purpose"},
  {vsn, "X.Y.Z"},
  {registered, [
      app_name_sup,          % Supervisor process
      other_registered_name  % Additional registered processes
  ]},
  {applications, [
      kernel,
      stdlib,
      % External dependencies
      dep_name
  ]},
  {mod, {app_name_app, []}},  % Application callback module
  {env, [
      % Configuration defaults
      {config_key, value}
  ]},
  {modules, []},              % Leave empty - rebar3 auto-populates
  {licenses, ["Apache-2.0"]},
  {links, [{"GitHub", "https://github.com/banyan-platform/erlmcp"}]}
 ]}.
```

### Full Structure

1. **{application, Name}** - Application identifier
2. **{description, String}** - Human-readable description
3. **{vsn, String}** - Semantic version (X.Y.Z)
4. **{registered, [Names]}** - Registered processes for discovery
5. **{applications, [Names]}** - Required dependencies
6. **{mod, {Module, Args}}** - Application callback module
7. **{env, [Config]}** - Default environment configuration
8. **{modules, []}** - Leave empty (auto-populated by rebar3)
9. **{licenses, [Strings]}** - License identifiers
10. **{links, [Tuples]}** - Project links

---

## Version Management

### Current Version: 2.1.0

All applications MUST use the same version for release consistency:

```erlang
erlmcp_core:           {vsn, "2.1.0"}
erlmcp_transports:     {vsn, "2.1.0"}
erlmcp_observability:  {vsn, "2.1.0"}
tcps_erlmcp:           {vsn, "2.1.0"}
Root release:          {release, {erlmcp, "2.1.0"}
```

### Semantic Versioning

erlmcp follows semantic versioning (semver): **MAJOR.MINOR.PATCH**

| Change Type | When | Example |
|-----------|------|---------|
| MAJOR | Breaking API changes, incompatible updates | 1.0.0 → 2.0.0 |
| MINOR | New features, backward compatible | 2.0.0 → 2.1.0 |
| PATCH | Bug fixes, no API changes | 2.1.0 → 2.1.1 |

### Version Update Procedure

When bumping version from X.Y.Z to X.Y.N:

1. Update all 4 .app.src files:
   ```bash
   sed -i '' 's/{vsn, "X.Y.Z"}/{vsn, "X.Y.N"}/' \
     apps/*/src/*.app.src
   ```

2. Update root rebar.config:
   ```erlang
   {release, {erlmcp, "X.Y.N"},
   ```

3. Update version in src/erlmcp.app.src (if standalone):
   ```erlang
   {vsn, "X.Y.N"}
   ```

4. Create git tag:
   ```bash
   git tag -a v2.1.0 -m "Release v2.1.0"
   ```

5. Verify consistency:
   ```bash
   grep -h "vsn" apps/*/src/*.app.src rebar.config | grep "\"2\."
   ```

---

## Dependencies

### Dependency Types

1. **Kernel/Stdlib** (OTP standard - always required)
   ```erlang
   {applications, [
       kernel,
       stdlib,
       crypto
   ]}
   ```

2. **External Libraries** (from hex.pm via rebar.config)
   ```erlang
   {applications, [
       jsx,      % JSON encoding
       jesse,    % JSON schema validation
       gproc     % Global process registry
   ]}
   ```

3. **Internal Apps** (inter-app dependencies in umbrella)
   ```erlang
   {applications, [
       erlmcp_core,              % Core always required
       erlmcp_observability      % Optional in tcps_erlmcp
   ]}
   ```

### Current Dependencies

**Root (rebar.config)**:
- jsx 3.1.0
- jesse 1.8.1
- gproc 0.9.0
- gun 2.0.1
- ranch 2.1.0
- poolboy 1.5.2
- bbmustache 1.12.2
- cowboy 2.10.0
- opentelemetry_api 1.5.0
- opentelemetry 1.7.0
- opentelemetry_exporter 1.10.0
- jobs 0.10.0
- fs 0.9.2

**erlmcp_core**:
```erlang
{applications, [
    kernel,
    stdlib,
    crypto,
    jsx,
    jesse,
    gproc
]}
```

**erlmcp_transports**:
```erlang
{applications, [
    kernel,
    stdlib,
    ssl,
    inets,
    gun,
    ranch,
    poolboy,
    erlmcp_core
]}
```

**erlmcp_observability**:
```erlang
{applications, [
    kernel,
    stdlib,
    opentelemetry_api,
    opentelemetry,
    opentelemetry_exporter,
    erlmcp_core
]}
```

**tcps_erlmcp**:
```erlang
{applications, [
    kernel,
    stdlib,
    bbmustache,
    cowboy,
    jobs,
    fs,
    erlmcp_core,
    erlmcp_observability
]}
```

### Adding a New Dependency

1. Add to root `rebar.config`:
   ```erlang
   {deps, [
       {new_dep, "X.Y.Z"}
   ]}
   ```

2. Add to relevant app.src `{applications, [...]}`:
   ```erlang
   {applications, [
       kernel,
       stdlib,
       new_dep
   ]}
   ```

3. Add to app's rebar.config if standalone deps needed:
   ```erlang
   {deps, [
       {new_dep, "X.Y.Z"}
   ]}
   ```

4. Verify with:
   ```bash
   rebar3 compile
   rebar3 xref
   ```

---

## Module Lists

### Pattern: {modules, []}

All erlmcp applications use **empty module lists**:

```erlang
{modules, []}
```

### Why Empty?

1. **Auto-populated by rebar3** at compile time
2. **Prevents stale lists** (no manual sync needed)
3. **Best practice** for OTP applications
4. **Removes maintenance burden** when modules added/removed

### How It Works

1. Source modules are in `src/*.erl`
2. rebar3 compiles to `_build/*/lib/app/ebin/*.beam`
3. rebar3 generates `.app` file with complete module list
4. Runtime loads modules from `_build/*/lib/app/ebin/`

### Generated .app Files

After `rebar3 compile`, check:
```bash
# erlmcp_core generated app
cat _build/default/lib/erlmcp_core/ebin/erlmcp_core.app

# Output includes auto-generated modules list:
{modules,[erlmcp_app,erlmcp_auth,erlmcp_batch,...erlmcp_tool]}
```

### Manual Module Lists (Anti-Pattern)

DO NOT manually list modules:
```erlang
% BAD - becomes stale!
{modules, [erlmcp_client, erlmcp_server, erlmcp_json_rpc]},
```

---

## Registration

### Why Register Processes?

1. **Global discovery** - Processes accessible globally
2. **gproc integration** - Distributed process registry
3. **Supervision trees** - Register supervisors for restart policies
4. **Monitoring** - Enable health checks and failover

### Registration Pattern

```erlang
{registered, [
    app_name_sup,           % Main supervisor
    app_name_server,        % Named server (if needed)
    app_name_registry       % Registry process (if needed)
]}
```

### Current Registrations

**erlmcp_core** (4 processes):
```erlang
{registered, [
    erlmcp_sup,
    erlmcp_core_sup,
    erlmcp_server_sup,
    erlmcp_registry
]}
```
- `erlmcp_sup` - Root supervisor (start_link in erlmcp_sup.erl)
- `erlmcp_core_sup` - Core supervision tree
- `erlmcp_server_sup` - Simple one-for-one pool of client servers
- `erlmcp_registry` - Central message routing (gproc-based)

**erlmcp_transports** (4 processes):
```erlang
{registered, [
    erlmcp_transport_sup,
    erlmcp_transport_stdio,
    erlmcp_transport_tcp,
    erlmcp_transport_http
]}
```

**erlmcp_observability** (3 processes):
```erlang
{registered, [
    erlmcp_observability_sup,
    erlmcp_metrics,
    erlmcp_otel
]}
```

**tcps_erlmcp** (4 processes):
```erlang
{registered, [
    tcps_erlmcp_sup,
    tcps_shacl_validator,
    tcps_receipt_chain,
    tcps_quality_gates
]}
```

### Process Supervision

Each registered process must be started by:
1. Application callback module (erlmcp_app:start/2)
2. Supervisor process (via supervisor child spec)

Example (erlmcp_app.erl):
```erlang
-module(erlmcp_app).
-behaviour(application).

start(_StartType, _StartArgs) ->
    erlmcp_sup:start_link().

stop(_State) ->
    ok.
```

---

## Environment Config

### Purpose

Configuration defaults that applications use at runtime:

```erlang
application:get_env(app_name, config_key, DefaultValue)
```

### erlmcp_core Config

```erlang
{env, [
    {client_defaults, #{
        timeout => 5000,                    % Request timeout (ms)
        strict_mode => false,               % Enable strict validation
        max_pending_requests => 100         % Backpressure limit
    }},
    {server_defaults, #{
        max_subscriptions_per_resource => 1000,  % Resource subscription limit
        max_progress_tokens => 10000             % Progress token tracking limit
    }},
    {registry_defaults, #{
        sharding_strategy => none,              % Message routing strategy
        health_check_interval => 30000          % Health check frequency
    }},
    {cluster_enabled, false},                   % Distributed cluster mode
    {cluster_nodes, []},                        % Cluster node list
    {cluster_cookie, erlmcp_cluster},           % Cookie for node auth
    {cluster_heartbeat_interval, 10000},        % Heartbeat frequency
    {node_check_interval, 5000},                % Node liveness check
    {split_brain_strategy, winner_takes_all},   % Split-brain resolution
    {split_brain_check_interval, 30000},        % Split-brain detection frequency
    {master_node, undefined}                    % Master node override
]}
```

### erlmcp_transports Config

```erlang
{env, [
    {transport_defaults, #{
        tcp => #{
            connect_timeout => 5000,   % TCP connection timeout
            keepalive => true,         % TCP keep-alive
            nodelay => true,           % TCP_NODELAY (disable Nagle)
            port => 3000               % Default TCP port
        },
        http => #{
            connect_timeout => 5000,   % HTTP connection timeout
            request_timeout => 30000,  % HTTP request timeout
            max_connections => 100,    % Max concurrent connections
            port => 3001               % Default HTTP port
        },
        stdio => #{
            buffer_size => 65536,      % STDIO buffer size
            read_timeout => infinity   % STDIO read timeout
        }
    }}
]}
```

### erlmcp_observability Config

```erlang
{env, [
    {otel_defaults, #{
        service_name => <<"erlmcp">>,
        exporter => {otlp, #{
            endpoint => "http://localhost:4318",
            protocol => http_protobuf
        }},
        sampling_rate => 1.0
    }},
    {metrics_defaults, #{
        interval => 60000,
        backend => simple
    }},
    {receipt_defaults, #{
        hash_algorithm => sha256,
        storage_backend => file,
        verification_on_read => false
    }}
]}
```

### tcps_erlmcp Config

```erlang
{env, [
    {quality_gates, #{
        min_test_pass_rate => 0.80,
        min_coverage => 0.80,
        max_cyclomatic_complexity => 15,
        max_function_length => 50
    }},
    {shacl_defaults, #{
        shapes_dir => "shapes",
        ontology_dir => "ontology",
        strict_mode => true
    }},
    {receipt_chain, #{
        algorithm => sha256,
        storage => file,
        chain_file => "priv/tcps/receipt_chain.dat"
    }},
    {dashboard, #{
        port => 8080,
        host => "localhost"
    }},
    {tcps_auto_integration, true},
    {tcps_quality_gates_enabled, [1,2,3,4,5,6,7,8]},
    {tcps_andon_on_sla_violation, true}
]}
```

### Reading Config at Runtime

```erlang
% Get with default
application:get_env(erlmcp_core, timeout, 5000)

% Get all config for app
application:get_all_env(erlmcp_core)

% Set config (usually in tests)
application:set_env(erlmcp_core, timeout, 10000)
```

---

## Maintenance Checklist

### When Adding a New Module

- [ ] Module added to `src/` directory
- [ ] Module implements behavior (if required)
- [ ] Module compiled by rebar3 (no action needed in .app.src)
- [ ] Verify: `rebar3 compile && rebar3 eunit`

### When Adding a New Dependency

- [ ] Added to root `rebar.config` `{deps, [...]}`
- [ ] Added to app .app.src `{applications, [...]}`
- [ ] Added to app `rebar.config` if standalone deps needed
- [ ] Version matches in both places
- [ ] Verify: `rebar3 compile && rebar3 xref`

### When Adding a New Process to Start

- [ ] Process added to app supervision tree
- [ ] Process registered in .app.src `{registered, [...]}`
- [ ] Process registered via gproc in code (if global access needed)
- [ ] Health checks added (if critical)
- [ ] Verify: `rebar3 eunit` and manual test

### When Adding Configuration

- [ ] Config added to app .app.src `{env, [...]}`
- [ ] Config documented in README.md
- [ ] Config readable via `application:get_env/2`
- [ ] Config overridable in sys.config at runtime
- [ ] Verify: `erl -config priv/sys.config -noshell`

### When Bumping Version

- [ ] Update all 4 app .app.src files
- [ ] Update root rebar.config release
- [ ] Verify: `grep vsn apps/*/src/*.app.src | sort -u`
- [ ] Verify: `grep -o '"[0-9.]*"' rebar.config | head -1`
- [ ] Git tag: `git tag -a vX.Y.Z -m "Release X.Y.Z"`

### Before Release

- [ ] All version strings consistent
- [ ] All dependencies declared and used
- [ ] All module lists auto-populated (empty in .app.src)
- [ ] All registered processes accounted for
- [ ] All environment config tested
- [ ] Compilation clean: `rebar3 clean && rebar3 compile`
- [ ] Tests pass: `rebar3 eunit`
- [ ] Cross-reference clean: `rebar3 xref`
- [ ] Type checking: `rebar3 dialyzer`

---

## Common Mistakes

### Mistake 1: Manual Module Lists

**WRONG**:
```erlang
{modules, [erlmcp_client, erlmcp_server, erlmcp_json_rpc]}
```

**WHY**: Becomes stale when modules added/removed, causes startup errors

**RIGHT**:
```erlang
{modules, []}
```

**FIX**: Let rebar3 auto-populate at compile time

---

### Mistake 2: Version Mismatch

**WRONG**:
```erlang
% app.src: v2.1.0
% rebar.config: v2.0.0
```

**WHY**: Release version doesn't match app versions, deployment fails

**RIGHT**:
```erlang
% All files: v2.1.0
```

**FIX**: Update all 4 .app.src + root rebar.config to same version

---

### Mistake 3: Missing Dependency

**WRONG**:
```erlang
% Uses gproc but not in {applications, [...]}
erlmcp_registry:start_link()
```

**WHY**: gproc not loaded, startup crashes with `undef`

**RIGHT**:
```erlang
{applications, [kernel, stdlib, gproc]}
```

**FIX**: Add to both .app.src and rebar.config

---

### Mistake 4: Unregistered Critical Process

**WRONG**:
```erlang
% Process started but not registered
{ok, Pid} = erlmcp_sup:start_link()
```

**WHY**: Cannot discover process globally, distributed calls fail

**RIGHT**:
```erlang
% In erlmcp_sup.erl: register(erlmcp_sup, self())
% In .app.src: {registered, [erlmcp_sup]}
```

**FIX**: Register process via `register/2` and list in {registered, [...]}

---

### Mistake 5: Missing App Module

**WRONG**:
```erlang
{mod, {}}  % Empty tuple
```

**WHY**: Application won't start, no callback to start_link

**RIGHT**:
```erlang
{mod, {erlmcp_app, []}}
```

**FIX**: Create erlmcp_app.erl with application behavior

---

### Mistake 6: Circular Dependencies

**WRONG**:
```erlang
% erlmcp_core depends on erlmcp_observability
% erlmcp_observability depends on erlmcp_core
```

**WHY**: Circular dependency, startup deadlock

**RIGHT**:
```erlang
% erlmcp_core: no erlmcp_observability dependency
% erlmcp_observability: depends on erlmcp_core
```

**FIX**: Make dependencies acyclic (linear hierarchy)

---

### Mistake 7: Test Dependencies in Production

**WRONG**:
```erlang
{applications, [kernel, stdlib, proper, meck]}  % Test deps!
```

**WHY**: Test libraries loaded in production, bloat and security risk

**RIGHT**:
```erlang
% In .app.src: {applications, [kernel, stdlib]}
% In rebar.config {test, [...]} profile: {deps, [proper, meck]}
```

**FIX**: Move test deps to rebar.config test profile

---

## References

- **OTP Design Principles**: `erl -doc design_principles`
- **Rebar3 Docs**: https://rebar3.org
- **Erlang/OTP 25+**: https://erlang.org
- **erlmcp Architecture**: `/docs/architecture.md`
- **erlmcp OTP Patterns**: `/docs/otp-patterns.md`

---

## Summary

| Item | Status | Action |
|------|--------|--------|
| Version consistency | ⚠️ NEEDS FIX | Update rebar.config 2.0.0 → 2.1.0 |
| Module lists | ✅ | All apps use {modules, []} |
| Dependencies | ✅ | All declared and used |
| Registrations | ✅ | All supervisors registered |
| Environment config | ✅ | All apps configured |
| Metadata | ✅ | Licenses, links, descriptions |

**Overall**: 95/100 (will be 100/100 after version fix)
