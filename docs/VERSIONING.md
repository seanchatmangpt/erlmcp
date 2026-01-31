# Versioning Policy - erlmcp

## Executive Summary

erlmcp follows **Semantic Versioning 2.0.0** (SemVer) with strict compatibility guarantees across OTP 25-28. This document defines version bumping rules, breaking change policies, deprecation procedures, and release schedules to ensure predictable upgrades and minimal disruption to dependent systems.

**Key Principles:**
- **MAJOR**: Breaking changes, incompatible API modifications
- **MINOR**: New features, backward-compatible additions
- **PATCH**: Bug fixes, backward-compatible corrections
- **Deprecation Grace Period**: Minimum 2 minor versions before removal
- **Release Cadence**: Monthly minor releases, quarterly major releases

## Version Format

```
MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]

Examples:
2.1.0          - Stable release
2.2.0-rc.1     - Release candidate
2.2.0+20260131 - Build metadata
```

### Version Components

| Component | Increment When | Example |
|-----------|----------------|---------|
| **MAJOR** | Incompatible API changes, protocol breaking changes | 1.x.x → 2.0.0 |
| **MINOR** | New features, backward-compatible API additions | 2.1.x → 2.2.0 |
| **PATCH** | Bug fixes, documentation, performance improvements | 2.1.0 → 2.1.1 |
| **PRERELEASE** | Alpha, beta, release candidate | 2.2.0-rc.1 |
| **BUILD** | Build metadata, commit SHA | 2.2.0+abc123 |

## Breaking Changes Policy

### What Constitutes a Breaking Change

**MAJOR version bump required for:**

1. **API Signature Changes**
   ```erlang
   % BREAKING: Removed parameter
   % v2.x.x
   erlmcp_client:start_link(TransportType, TransportOpts, ClientOpts)

   % v3.0.0
   erlmcp_client:start_link(TransportType, ClientOpts)
   ```

2. **Return Value Changes**
   ```erlang
   % BREAKING: Changed error format
   % v2.x.x
   {error, {invalid_request, Reason}}

   % v3.0.0
   {error, #{code => -32600, message => <<"Invalid Request">>, data => Reason}}
   ```

3. **Behavior Callback Changes**
   ```erlang
   % BREAKING: New required callback
   % v2.x.x
   -callback init(Args) -> {ok, State}.

   % v3.0.0
   -callback init(Args) -> {ok, State}.
   -callback validate(Request) -> ok | {error, Reason}. % NEW REQUIRED
   ```

4. **Protocol Changes**
   - JSON-RPC message format modifications
   - MCP capability negotiation changes
   - Transport handshake protocol changes

5. **Configuration Changes**
   ```erlang
   % BREAKING: Renamed config key
   % v2.x.x
   {erlmcp_session, [{backend, erlmcp_session_ets}]}

   % v3.0.0
   {erlmcp_session, [{persistence_backend, erlmcp_session_ets}]}
   ```

6. **Type Specification Changes**
   ```erlang
   % BREAKING: Changed return type
   % v2.x.x
   -spec get_session(SessionId) -> {ok, Session} | {error, not_found}.

   % v3.0.0
   -spec get_session(SessionId) -> Session | undefined.
   ```

### What is NOT a Breaking Change

**MINOR version bump allowed for:**

1. **New Optional Parameters**
   ```erlang
   % NOT BREAKING: New optional parameter with default
   % v2.1.x
   erlmcp_client:call_tool(Client, ToolName, Args).

   % v2.2.0
   erlmcp_client:call_tool(Client, ToolName, Args, Opts).
   % Opts defaults to #{} if not provided
   ```

2. **New Functions**
   ```erlang
   % NOT BREAKING: New function added
   % v2.2.0
   erlmcp_session:get_metadata(SessionId) -> {ok, Metadata}.
   ```

3. **New Optional Behavior Callbacks**
   ```erlang
   % NOT BREAKING: New optional callback
   -callback handle_metadata(Metadata, State) -> {ok, State}.
   -optional_callbacks([handle_metadata/2]).
   ```

4. **Extended Error Information** (backward-compatible)
   ```erlang
   % NOT BREAKING: More detailed error
   % v2.1.x
   {error, timeout}

   % v2.2.0
   {error, {timeout, #{duration_ms => 5000, operation => call_tool}}}
   % Pattern match on {error, timeout} still works
   ```

5. **Performance Improvements**
   - Algorithm optimizations
   - Memory usage reductions
   - Throughput increases

6. **New OTP Application** (in umbrella)
   - Adding `erlmcp_validation` to existing umbrella

## Deprecation Process

### Deprecation Timeline

**Minimum 2 minor versions before removal:**

```
v2.1.0 - Feature introduced
v2.2.0 - Feature marked deprecated (warning added)
v2.3.0 - Deprecation warning continues
v3.0.0 - Feature removed (MAJOR bump)
```

### Deprecation Annotations

**Code Level:**
```erlang
%% @deprecated Use erlmcp_session:create/2 instead
%% Deprecated in v2.2.0, will be removed in v3.0.0
-spec new_session(Options) -> {ok, SessionId} | {error, Reason}.
new_session(Options) ->
    logger:warning("erlmcp_session:new_session/1 is deprecated. "
                   "Use erlmcp_session:create/2 instead. "
                   "This function will be removed in v3.0.0."),
    erlmcp_session:create(Options, #{}).
```

**Documentation:**
```markdown
## Deprecated Functions

| Function | Deprecated In | Remove In | Replacement |
|----------|---------------|-----------|-------------|
| `new_session/1` | v2.2.0 | v3.0.0 | `create/2` |
| `register_tool/2` | v2.3.0 | v3.0.0 | `add_tool/3` |
```

**CHANGELOG Entry:**
```markdown
## [2.2.0] - 2026-02-15

### Deprecated
- `erlmcp_session:new_session/1` - Use `create/2` instead (removal in v3.0.0)
```

### Graceful Migration Path

**Provide side-by-side compatibility:**

```erlang
%% v2.2.0 - Both old and new APIs work
create(SessionId, Options) ->
    % New API implementation
    ...

new_session(Options) ->
    % Delegate to new API
    SessionId = generate_session_id(),
    create(SessionId, Options).
```

**Document migration in release notes:**
```markdown
### Migration Guide: v2.1.x → v2.2.0

**Deprecated `new_session/1`:**
```erlang
% Old (deprecated)
{ok, SessionId} = erlmcp_session:new_session(#{timeout => 300000}).

% New (recommended)
{ok, SessionId} = erlmcp_session:create(
    <<"session_abc123">>,
    #{timeout_ms => 300000}
).
```
```

## Version Compatibility Matrix

### OTP Version Support

| erlmcp Version | OTP 25 | OTP 26 | OTP 27 | OTP 28 |
|----------------|--------|--------|--------|--------|
| 2.0.x          | ✅     | ✅     | ✅     | ❌     |
| 2.1.x          | ✅     | ✅     | ✅     | ✅     |
| 2.2.x          | ✅     | ✅     | ✅     | ✅     |
| 3.0.x (planned)| ❌     | ✅     | ✅     | ✅     |

### Dependency Compatibility

| erlmcp | gproc | gun | ranch | poolboy | jsx | jesse |
|--------|-------|-----|-------|---------|-----|-------|
| 2.0.x  | 0.9.0 | 2.0.1 | 2.1.0 | 1.5.2 | 3.1.0 | 1.5.2 |
| 2.1.x  | 0.9.0 | 2.0.1 | 2.1.0 | 1.5.2 | 3.1.0 | 1.5.2 |
| 2.2.x  | 0.9.1 | 2.1.0 | 2.1.0 | 1.5.2 | 3.1.0 | 1.6.0 |

### Inter-Application Compatibility (Umbrella)

| erlmcp_core | erlmcp_transports | erlmcp_observability | erlmcp_validation |
|-------------|-------------------|----------------------|-------------------|
| 2.1.0       | 2.0.x             | 0.1.x                | 0.1.x             |
| 2.2.0       | 2.1.x             | 0.2.x                | 0.2.x             |
| 3.0.0       | 3.0.x             | 1.0.x                | 1.0.x             |

## Release Schedule

### Cadence

| Release Type | Frequency | Scope | Branch |
|--------------|-----------|-------|--------|
| **PATCH**    | As needed | Critical bug fixes, security patches | `hotfix/*` |
| **MINOR**    | Monthly   | New features, deprecations | `release/2.x` |
| **MAJOR**    | Quarterly | Breaking changes, major features | `main` |

### Release Calendar (2026)

| Version | Type  | Planned Date | Focus |
|---------|-------|--------------|-------|
| 2.1.1   | PATCH | 2026-02-01   | Bug fixes |
| 2.2.0   | MINOR | 2026-02-28   | Session failover, new transports |
| 2.3.0   | MINOR | 2026-03-31   | Enhanced observability |
| 3.0.0   | MAJOR | 2026-06-30   | Protocol v2, API cleanup |

## Version Bumping Examples

### Example 1: Bug Fix (PATCH)

**Change:**
```erlang
% Fix: Memory leak in session cleanup
% apps/erlmcp_core/src/erlmcp_session_manager.erl
handle_info({cleanup_expired, _Ref}, State) ->
    ExpiredSessions = find_expired_sessions(State),
    NewState = lists:foldl(fun cleanup_session/2, State, ExpiredSessions),
    schedule_cleanup(NewState),
    {noreply, NewState}.  % FIX: Was missing cleanup scheduling
```

**Version Bump:**
```
2.1.0 → 2.1.1
```

**CHANGELOG:**
```markdown
## [2.1.1] - 2026-02-01

### Fixed
- Memory leak in session cleanup when cleanup timer wasn't rescheduled
```

### Example 2: New Feature (MINOR)

**Change:**
```erlang
% New: Add session metadata update API
% apps/erlmcp_core/src/erlmcp_session.erl
-spec update_metadata(SessionId, Metadata) -> ok | {error, Reason}.
update_metadata(SessionId, Metadata) when is_map(Metadata) ->
    erlmcp_session_manager:update_metadata(SessionId, Metadata).
```

**Version Bump:**
```
2.1.1 → 2.2.0
```

**CHANGELOG:**
```markdown
## [2.2.0] - 2026-02-28

### Added
- `erlmcp_session:update_metadata/2` - Update session metadata without recreating session
```

### Example 3: Breaking Change (MAJOR)

**Change:**
```erlang
% BREAKING: Unified error format across all modules
% apps/erlmcp_core/src/erlmcp_client.erl

% OLD (v2.x.x)
-spec call_tool(Client, ToolName, Args) ->
    {ok, Result} | {error, atom() | tuple()}.

% NEW (v3.0.0)
-spec call_tool(Client, ToolName, Args) ->
    {ok, Result} | {error, error_map()}.

-type error_map() :: #{
    code := integer(),
    message := binary(),
    data => term()
}.
```

**Version Bump:**
```
2.3.0 → 3.0.0
```

**CHANGELOG:**
```markdown
## [3.0.0] - 2026-06-30

### Changed (BREAKING)
- **Error Format**: All error returns now use structured maps instead of atoms/tuples
  - Old: `{error, timeout}` or `{error, {invalid_request, Reason}}`
  - New: `{error, #{code => -32000, message => <<"Timeout">>, data => Details}}`

### Migration Guide
See [MIGRATION_v2_to_v3.md](./MIGRATION_v2_to_v3.md) for detailed upgrade instructions.
```

## Git Tag Conventions

### Tag Format

```
vMAJOR.MINOR.PATCH[-PRERELEASE]

Examples:
v2.1.0         - Stable release
v2.2.0-rc.1    - Release candidate
v3.0.0-beta.2  - Beta release
```

### Tagging Process

```bash
# 1. Update version in app.src files
vim apps/erlmcp_core/src/erlmcp_core.app.src
# Change: {vsn, "2.1.0"} → {vsn, "2.2.0"}

# 2. Update CHANGELOG.md
vim CHANGELOG.md

# 3. Commit version bump
git add apps/*/src/*.app.src CHANGELOG.md
git commit -m "chore: Bump version to v2.2.0"

# 4. Create annotated tag
git tag -a v2.2.0 -m "Release v2.2.0

### Added
- Session metadata update API
- WebSocket transport enhancements

### Fixed
- Memory leak in session cleanup

See CHANGELOG.md for full details."

# 5. Push tag
git push origin v2.2.0
```

### Pre-release Tags

```bash
# Alpha release
git tag -a v3.0.0-alpha.1 -m "Alpha release for v3.0.0"

# Beta release
git tag -a v3.0.0-beta.1 -m "Beta release for v3.0.0"

# Release candidate
git tag -a v3.0.0-rc.1 -m "Release candidate 1 for v3.0.0"
```

## CHANGELOG Requirements

### Format (Keep a Changelog)

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- New feature in development

## [2.2.0] - 2026-02-28

### Added
- `erlmcp_session:update_metadata/2` for in-place metadata updates
- WebSocket transport now supports compression (RFC 7692)

### Changed
- Session timeout default increased from 5min to 15min

### Deprecated
- `erlmcp_session:new_session/1` in favor of `create/2` (removal in v3.0.0)

### Fixed
- Memory leak in session cleanup timer rescheduling
- Race condition in registry during high-concurrency startup

### Security
- Fixed timing attack in session ID validation

## [2.1.1] - 2026-02-01

### Fixed
- Memory leak in session cleanup

[Unreleased]: https://github.com/banyan/erlmcp/compare/v2.2.0...HEAD
[2.2.0]: https://github.com/banyan/erlmcp/compare/v2.1.1...v2.2.0
[2.1.1]: https://github.com/banyan/erlmcp/compare/v2.1.0...v2.1.1
```

### Required Sections

| Section | Use For | Example |
|---------|---------|---------|
| **Added** | New features, APIs | New transport type |
| **Changed** | Modifications to existing features | Default timeout increase |
| **Deprecated** | Soon-to-be-removed features | Old API marked for removal |
| **Removed** | Deleted features (MAJOR only) | Removed deprecated function |
| **Fixed** | Bug fixes | Memory leak fix |
| **Security** | Vulnerability patches | Auth bypass fix |

## Versioning in Code

### app.src Files

```erlang
% apps/erlmcp_core/src/erlmcp_core.app.src
{application, erlmcp_core,
 [{vsn, "2.2.0"},  % <-- Update this
  {description, "Erlang MCP Core Protocol"},
  ...
]}.
```

### Module Versions (Optional)

```erlang
-module(erlmcp_client).
-vsn("2.2.0").  % Optional: Track module version
```

### Runtime Version Check

```erlang
% Check erlmcp version at runtime
erlmcp:version().
%=> <<"2.2.0">>

% Check compatibility
erlmcp:compatible_with(<<"2.1.0">>).
%=> true  % 2.2.0 is backward-compatible with 2.1.0

erlmcp:compatible_with(<<"3.0.0">>).
%=> false  % Major version incompatibility
```

## Toyota Production System Integration

### Poka-Yoke (Mistake-Proofing)

**Automated version validation in CI/CD:**
```bash
#!/bin/bash
# scripts/validate_version.sh

# Check all app.src files have same MAJOR.MINOR
CORE_VSN=$(grep '{vsn,' apps/erlmcp_core/src/erlmcp_core.app.src | cut -d'"' -f2)
TRANS_VSN=$(grep '{vsn,' apps/erlmcp_transports/src/erlmcp_transports.app.src | cut -d'"' -f2)

if [[ "${CORE_VSN%.*}" != "${TRANS_VSN%.*}" ]]; then
    echo "ERROR: Version mismatch between applications"
    exit 1
fi
```

### Jidoka (Built-in Quality)

**Pre-commit hook prevents version errors:**
```bash
# .git/hooks/pre-commit
#!/bin/bash

# Verify version format
if ! grep -E '{vsn, "[0-9]+\.[0-9]+\.[0-9]+"}' apps/*/src/*.app.src; then
    echo "ERROR: Invalid version format in app.src"
    exit 1
fi

# Verify CHANGELOG updated
if git diff --cached CHANGELOG.md | grep -q "## \[Unreleased\]"; then
    echo "ERROR: CHANGELOG.md not updated for this release"
    exit 1
fi
```

### Kaizen (Continuous Improvement)

**Version analytics for improvement:**
- Track breaking change frequency (target: <1 MAJOR per quarter)
- Monitor deprecation period (enforce 2+ MINOR versions)
- Analyze upgrade adoption rates
- Identify frequently broken APIs for redesign

## FAQ

### Q: When should I bump the MAJOR version?

**A:** When you make incompatible API changes that break existing client code:
- Function signature changes
- Removed functions/modules
- Changed return types
- Protocol breaking changes
- Required configuration changes

### Q: Can I backport bug fixes to older MAJOR versions?

**A:** Yes, maintain LTS (Long Term Support) for N-1 MAJOR version:
```
v3.0.0 - Current stable
v2.x.x - LTS (security + critical bugs only)
v1.x.x - EOL (no updates)
```

### Q: How do I handle database schema changes?

**A:** Provide migration scripts and bump appropriately:
- **MINOR**: New optional fields, indexes
- **MAJOR**: Renamed/removed fields, incompatible schema changes

Include migration guide:
```erlang
%% Migrate v2.x.x Mnesia schema to v3.0.0
erlmcp_migration:run(from_version, "2.3.0", to_version, "3.0.0").
```

### Q: What about internal/private API changes?

**A:** Document clearly which APIs are public vs. internal:
```erlang
%% @doc Public API - Semantic versioning guarantees apply
-spec call_tool(...) -> ...

%% @private Internal API - May change without MAJOR bump
-spec internal_validate(...) -> ...
```

Only public APIs trigger MAJOR bumps.

---

**References:**
- [Semantic Versioning 2.0.0](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- Erlang/OTP Design Principles
- Toyota Production System (Poka-Yoke, Jidoka)
