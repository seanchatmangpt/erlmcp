# erlmcp Quick Start Guide

**Get productive in 5 minutes.**

---

## Prerequisites

```bash
erl -version              # Erlang/OTP 28.3.1+ (REQUIRED)
rebar3 --version          # Any recent version (3.22+)
```

Basic Erlang knowledge: https://www.erlang.org/doc/getting_started/users_guide.html

---

## Health Check

Verify environment before starting work:

```bash
make doctor   # Checks: OTP version, rebar3, deps, structure, profile
```

---

## Tiered Workflow

| Command | Time | Use Before | What It Runs |
|---------|------|------------|--------------|
| `make test-smoke` | 2 min | Quick commits | Codec, lifecycle, basic transport |
| `make quick` | 5 min | Regular commits | Compile + smoke + validator |
| `make test-full` | 10 min | Feature complete | All EUnit + CT + coverage |
| `make verify` | 15 min | Opening PR | xref + dialyzer + spec + transport + tests |

---

## One-Command Starts

```bash
make console                 # Erlang shell with apps loaded
make observer                # Process visualization GUI
make example-mcp-complete    # Full MCP demo (resources, tools, prompts)
```

See `examples/mcp_complete/` for production-ready server examples.

---

## Validation & Compliance

```bash
make validate-spec           # MCP 2025-11-25 compliance
make validate                # All quality gates (compile, test, coverage, dialyzer, xref, bench)
```

**Quality Gates** (BLOCKING):
- Compilation: 0 errors
- Tests: 0 failures
- Coverage: ≥80%
- Dialyzer: 0 warnings
- Xref: 0 undefined calls
- Benchmarks: <10% regression

---

## Contributing in 60 Seconds

```bash
make doctor              # 1. Health check (10s)
make test-smoke          # 2. Quick validation (2 min)
# ... make changes ...
make quick               # 3. Pre-commit check (5 min)
make verify              # 4. Before PR (15 min)
```

---

## Common Tasks

| Task | Command | Time |
|------|---------|------|
| Quick sanity check | `make test-smoke` | 2 min |
| Pre-commit validation | `make quick` | 5 min |
| PR readiness | `make verify` | 15 min |
| Full CI reproduction | `make ci-local` | 20 min |
| View example | `make example-mcp-complete` | 1 min |
| Generate compliance | `make validate` | 20 min |
| Check coverage | `make coverage` | 3 min |

---

## Troubleshooting

**OTP version mismatch** → Install OTP 28+, run `make doctor`

**Compilation errors** → `make distclean && make compile`

**Test failures** → Run `make test-smoke` first to isolate failures

**Dialyzer warnings** → `rm -rf _build/default/*_plt && rebar3 dialyzer`

**Still stuck?** See [DEVELOPMENT.md](../DEVELOPMENT.md) for detailed troubleshooting.

---

## Next Steps

**Read:**
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Workflow, PR templates, standards
- [CLAUDE.md](../CLAUDE.md) - Architecture, quality gates, TDD requirements
- [docs/otp-patterns.md](otp-patterns.md) - gen_server, supervision, behaviors

**Explore:**
- `examples/mcp_complete/` - Full-featured MCP server reference
- `examples/calculator/` - Simple tool server example

---

## Key Principles

- **Chicago School TDD**: No mocks. Real processes only.
- **Quality Gates**: BLOCKING. Zero compromises.
- **Let-It-Crash**: Supervision handles failures, not defensive code.

**Ready?** Run `make quick`, make changes, run `make verify`, open PR.
