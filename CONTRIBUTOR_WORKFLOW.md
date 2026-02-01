# Contributing in 60 Seconds

## Prerequisites (Verify Once)

```bash
make doctor
```

## Workflow for Each Feature

```bash
make smoke              # 2 min: quick validation
# ... make your changes ...
make quick              # 10 min: full validation
# ... review output ...
make verify             # 15 min: PR-ready
```

## Before Push

- Run: `make verify`
- Ensure all gates pass (✓)
- Create descriptive commit message

## Quality Gates (What Passes)

- ✓ Compilation (0 errors)
- ✓ Tests (0 failures)
- ✓ Coverage (≥80%)
- ✓ Dialyzer (0 warnings, advisory)
- ✓ Xref (0 undefined calls)

## If Something Fails

- Check logs in `/tmp/erlmcp_*.log`
- Use `make doctor` to diagnose
- See [DEVELOPMENT.md](DEVELOPMENT.md) for deeper issues

## Tier Descriptions

| Tier | Time | What It Does |
|------|------|--------------|
| **smoke** | ≤2 min | Minimal tests—compile + protocol suite |
| **quick** | ≤10 min | Core validation—smoke + integration tests |
| **verify** | ≤15 min | PR-ready—quick + dialyzer + xref + spec compliance |

## Hands-Off (Automated)

- Pre-commit hooks run `make quick` automatically
- CI runs `make verify` + `make ci-local` on PRs
- Failures block commits—no way around it (intentional)

## Tips

- Run `make doctor` first if anything is weird
- Use `make console` to debug interactively
- Use `make observer` to watch processes live
- See [examples/](examples/) for reference implementations

## Learn More

- [CLAUDE.md](CLAUDE.md) - Architecture & OTP patterns
- [DEVELOPMENT.md](DEVELOPMENT.md) - Full setup
- [CONTRIBUTING.md](CONTRIBUTING.md) - Code standards
