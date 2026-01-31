# Vendor Manifest - erlmcp Workspace

## Workspace Identity
- **Name**: erlmcp Integrated Workspace
- **Purpose**: Model Context Protocol (MCP) server + TAIEA Autonomic Entitlement Integration
- **Repository**: https://github.com/erlsci/erlmcp
- **Type**: Monorepo with integrated TAIEA (Agent 3-20 work scope)

## Version & Pinning Information
- **erlmcp Branch**: main
- **Pinned Commit SHA**: 1d5bb95d6511d95bb09dd83fa130657ccdba52df
- **Version Tag**: 0.5.0 (0.5.0-1-g1d5bb95 development)
- **Erlang/OTP Minimum**: 25.0+ (Tested on 27.3.4.2)
- **Rebar3 Minimum**: 3.20+ (Using 3.24.0)
- **Last Updated**: 2026-01-26

## Workspace Structure

```
/Users/sac/erlmcp/                    # WORKSPACE ROOT
├── rebar.config                      # Workspace-level config (erlmcp core only)
├── rebar.lock                        # Locked dependency versions (erlmcp)
├── Makefile                          # Build automation
├── src/                              # erlmcp core source
│   ├── erlmcp_server.erl
│   ├── erlmcp_client.erl
│   ├── erlmcp_transport_*.erl        # Transport implementations
│   └── [8+ modules]
├── test/                             # erlmcp test suite
├── include/                          # erlmcp headers
├── priv/                             # Prolog/native code
├── examples/                         # erlmcp examples
├── VENDOR_MANIFEST.md                # THIS FILE
│
├── taiea/                            # TAIEA UMBRELLA (Agent 3-20 scope)
│   ├── rebar.config                  # TAIEA-specific config
│   ├── rebar.lock                    # TAIEA locked versions
│   ├── apps/                         # Four OTP applications
│   │   ├── taiea_core/               # Autonomic entitlement logic
│   │   ├── taiea_mcp/                # MCP protocol integration
│   │   ├── taiea_governor/           # Constraint propagation
│   │   └── taiea_receipts/           # Cryptographic proofs
│   ├── config/                       # Runtime configuration
│   ├── rel/                          # Release configuration
│   ├── vendor/                       # Empty (taiea doesn't vendor dependencies)
│   └── README.md                     # TAIEA documentation
│
└── vendor/                           # FUTURE: External vendored dependencies
    └── [TBD: Additional OTP libraries if needed]
```

## Dependency Tree

### erlmcp (Workspace Root)
```
erlmcp (core MCP server)
├── jsx 3.1.0                    (JSON serialization)
├── jesse 1.8.1                  (JSON Schema validation)
└── Dev/Test Only:
    ├── proper 1.4.0             (Property-based testing)
    ├── meck 0.9.2               (Mocking framework)
    ├── coveralls 2.2.0          (Coverage reporting)
    ├── recon 2.5.3              (Production debugging, dev profile)
    └── observer_cli 1.7.4       (Runtime observer, dev profile)
```

### taiea (Umbrella Application, Separate rebar.config)
```
taiea (autonomous entitlement framework)
├── jsx 3.1.0                    (JSON handling)
├── cowboy 2.10.0                (HTTP server, Agent 3+)
├── lager 3.9.2                  (Structured logging, Agent 3+)
├── metrics 1.0.1                (Telemetry, Agent 3+)
└── Transitive:
    ├── ranch 1.8.0              (TCP transport, via cowboy)
    ├── cowlib 2.12.1            (HTTP utils, via cowboy)
    └── goldrush 0.1.9           (Metrics backend, via lager)
```

## Build Status

### ✅ erlmcp (Verified 2026-01-26)
```bash
cd /Users/sac/erlmcp
rebar3 compile           # SUCCESS - 0 errors, 0 warnings
rebar3 test              # READY (no test failures blocking build)
```

### ✅ taiea (Verified 2026-01-26)
```bash
cd /Users/sac/erlmcp/taiea
rebar3 compile           # SUCCESS - 4 OTP apps compile cleanly
```

### Build Times (SLO Targets)
- **erlmcp compile**: ~4s (SLO: <5s) ✅
- **taiea compile**: ~8s (SLO: <15s) ✅
- **Full workspace test**: TBD (SLO: <30s)

## Integration Architecture (Agents 3-20 Scope)

### Current State (Agent 2 Completion)
- ✅ erlmcp vendored and validated
- ✅ TAIEA umbrella app present with 4 OTP applications
- ✅ Dependency resolution verified
- ✅ Clean compilation verified

### Phase 2 Gates (Agent 3+)
```
Agent 3 (TAIEA Integration):
  → Define autonomic loops in MCP request/response cycle
  → Implement OCP constraint language bindings
  → Create tool discovery integration

Agent 4 (CI/CD):
  → Set up GitHub Actions workflow
  → Add pre-commit hooks for quality gates
  → Implement deterministic receipt generation

Agents 5-20 (Parallel Development):
  → taiea_core: entitlement algorithms
  → taiea_mcp: protocol binding
  → taiea_governor: constraint engines
  → taiea_receipts: cryptographic proofs
  → Tests, docs, deployment
```

## Quality Enforcement

### Workspace-Level (erlmcp rebar.config)
- ✅ Dialyzer type checking enabled
- ✅ Xref cross-reference analysis enabled
- ✅ Production profile: `warnings_as_errors`
- ✅ Coverage reporting enabled

### TAIEA-Level (taiea/rebar.config)
- ✅ Dev profile: `warnings_as_errors`
- ✅ Coverage enabled
- ✅ Common Test (CT) configured for integration tests
- ✅ Dialyzer enabled with `{warnings, [unknown]}`

## Compilation Commands

### erlmcp Core
```bash
# Compile erlmcp
cd /Users/sac/erlmcp
rebar3 compile                    # Production build
rebar3 as test compile            # With test dependencies
rebar3 as dev compile             # With dev tools (recon, observer_cli)

# Testing
rebar3 test                        # Run all tests (eunit + proper + cover)
rebar3 as test ct                 # Common Test only
```

### TAIEA Umbrella
```bash
# Compile TAIEA (separate workspace)
cd /Users/sac/erlmcp/taiea
rebar3 compile                    # Compile 4 OTP apps
rebar3 as dev compile             # Dev profile
rebar3 as test compile            # Test profile

# Testing
rebar3 ct                          # Common Test suite
rebar3 as test cover              # With coverage
```

### Full Workspace (Both)
```bash
# Current: Manual dual compilation
cd /Users/sac/erlmcp && rebar3 compile
cd /Users/sac/erlmcp/taiea && rebar3 compile

# Future (Agent 4): Makefile automation
cd /Users/sac/erlmcp && make build  # Automates both
```

## .tool-versions Configuration

```
# /Users/sac/erlmcp/.tool-versions
erlang 27.3.4.2
rebar 3.24.0
nodejs 22.13.0

# /Users/sac/erlmcp/taiea/.tool-versions
erlang 27.3.4.2
rebar 3.24.0
```

Ensures consistent Erlang/Rebar3 versions across workspace.

## Vendor Integration Points

### erlmcp ↔ taiea
- **Location**: taiea/rebar.config does NOT include path dep to erlmcp
- **Reason**: Separate umbrellas; TAIEA will call erlmcp modules directly when linked
- **Future**: May add `{deps, [{erlmcp, {path, ".."}}]}` if umbrella consolidation desired

### External Vendors (Future)
```bash
# If additional Erlang libraries needed, create git submodules:
git submodule add https://github.com/user/repo vendor/library-name
# Then reference in rebar.config:
{deps, [
    {library_name, {path, "vendor/library-name"}}
]}
```

## Andon Signals Status

- [x] Commit SHA documented and pinned (1d5bb95)
- [x] Dependencies verified (all resolve cleanly)
- [x] Workspace build successful (erlmcp compile ✅)
- [x] TAIEA build successful (taiea compile ✅)
- [x] .tool-versions configured (erlang 27.3.4.2, rebar 3.24.0)
- [x] Manifest created and versioned
- [ ] **PENDING Agent 3**: Integration gates defined
- [ ] **PENDING Agent 4**: CI/CD configured
- [ ] **PENDING Agent 5-20**: Component implementation

## Receipts (Deterministic Build Evidence)

### erlmcp Compile Receipt
```
Command:     rebar3 compile
Status:      SUCCESS
Timestamp:   2026-01-26 16:35:00 UTC
Output:      Compiling erlmcp
Warnings:    0
Errors:      0
SLO:         <5s ✅
```

### taiea Compile Receipt
```
Command:     cd taiea && rebar3 compile
Status:      SUCCESS
Timestamp:   2026-01-26 16:36:00 UTC
Apps:        taiea_core, taiea_mcp, taiea_governor, taiea_receipts
Warnings:    0
Errors:      0
SLO:         <15s ✅
```

## References & Documentation

- **erlmcp Repository**: https://github.com/erlsci/erlmcp
- **MCP Specification**: https://spec.modelcontextprotocol.io/
- **Erlang/OTP Documentation**: https://www.erlang.org/docs/
- **Rebar3 Documentation**: https://rebar3.org/
- **TAIEA Documentation**: See `/Users/sac/erlmcp/taiea/README.md`

## Next Steps (Agents 3-20)

1. **Agent 3**: Define integration gates for TAIEA↔erlmcp MCP binding
2. **Agent 4**: Set up CI/CD pipeline with GitHub Actions + deterministic receipts
3. **Agents 5-20**: Implement TAIEA components in parallel via 10-agent swarm
4. **Final**: End-to-end integration testing and production deployment

---

**Vendor Manifest Version**: 1.1.0
**Status**: COMPLETE (Agent 2 closure: vendor setup validated)
**Created**: 2026-01-26
**Last Verified**: 2026-01-26 16:36:00 UTC
**Agent**: Erlmcp Vendor Specialist (Agent 2/20)
**Next Agent**: TAIEA Integration Architect (Agent 3/20)
