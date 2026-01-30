# erlmcp_validation

MCP specification compliance validation framework for erlmcp.

## Purpose

This application provides black-box validation tools to prove erlmcp correctly implements the MCP specification through observable behavior testing.

## Structure

- **src/**: Source modules
- **test/**: Test suites (Common Test and EUnit)
- **include/**: Header files with record definitions

## Usage

```bash
# Compile
rebar3 compile

# Run tests
rebar3 ct

# Run EUnit tests
rebar3 eunit
```

## Approach

- **Specification is source of truth** - Validation driven by MCP spec
- **Black-box testing** - Test observable behavior, not implementation
- **Proof by demonstration** - Each test proves a spec requirement
