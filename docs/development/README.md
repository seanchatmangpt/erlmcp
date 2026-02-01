# Development Process Overview

**Last Updated**: 2026-01-31 | **Status**: Active

This document provides a comprehensive overview of the erlmcp development process with visual workflows and practical guidance.

## Table of Contents

- [Quick Start](#quick-start)
- [Development Workflow](#development-workflow)
- [Testing Strategy](#testing-strategy)
- [Quality Gates](#quality-gates)
- [Tooling](#tooling)

---

## Quick Start

### First-Time Setup

```bash
# Clone repository
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp

# Install dependencies
make setup

# Enable environment (direnv recommended)
direnv allow

# Verify installation
make workspace-check
```

### Development Environment Validation

```mermaid
graph TB
    A[Start] --> B{Erlang/OTP ≥28?}
    B -->|No| C[Install Erlang]
    B -->|Yes| D{rebar3 installed?}
    C --> D
    D -->|No| E[Install rebar3]
    D -->|Yes| F{Git configured?}
    E --> F
    F -->|No| G[Configure Git]
    F -->|Yes| H{Dependencies OK?}
    G --> H
    H -->|No| I[run make setup]
    H -->|Yes| J{Tests Pass?}
    I --> J
    J -->|No| K[Fix Issues]
    J -->|Yes| L[Ready to Develop]
    K --> J

    style L fill:#51cf66
    style K fill:#ff6b6b
```

---

## Development Workflow

### Feature Development Cycle

```mermaid
stateDiagram-v2
    [*] --> Planning: Create Issue
    Planning --> Design: Architecture Review
    Design --> TDD_Red: Write Failing Test
    TDD_Red --> TDD_Green: Implement Feature
    TDD_Green --> TDD_Refactor: Improve Code
    TDD_Refactor --> QualityGates: Run Checks
    QualityGates --> TDD_Red: Gate Failed
    QualityGates --> CodeReview: Gate Passed
    CodeReview --> TDD_Red: Changes Requested
    CodeReview --> Merge: Approved
    Merge --> [*]

    note right of TDD_Red
        Chicago School TDD
        - No mocks
        - Real processes
        - Black-box tests
    end note

    note right of QualityGates
        Mandatory Gates:
        - Compile (errors=0)
        - Tests (pass_rate=1.0)
        - Coverage (≥80%)
        - Lint (0 warnings)
    end note
```

### Daily Development Loop

```mermaid
graph LR
    A[pull latest main] --> B[create feature branch]
    B --> C[write test FIRST]
    C --> D[confirm test FAILS]
    D --> E[implement feature]
    E --> F[test PASSES]
    F --> G[run full test suite]
    G --> H{all tests pass?}
    H -->|No| E
    H -->|Yes| I[run quality gates]
    I --> J{gates pass?}
    J -->|No| E
    J -->|Yes| K[refactor]
    K --> L[commit]
    L --> M[push to remote]
    M --> N[create PR]

    style D fill:#ff6b6b
    style F fill:#51cf66
    style N fill:#339af0
```

### Branch Strategy

```mermaid
gitGraph
    commit id: "Initial"
    branch main
    checkout main
    commit id: "v1.0.0"

    branch feature/auth
    checkout feature/auth
    commit id: "Add auth tests"
    commit id: "Implement auth"
    commit id: "Tests pass"

    checkout main
    merge feature/auth
    commit id: "Merge auth"

    branch hotfix/security
    checkout hotfix/security
    commit id: "Fix security issue"
    commit id: "Add tests"

    checkout main
    merge hotfix/security
    commit id: "Merge hotfix"
    commit id: "v1.0.1"

    checkout main
```

**Branch Types**:
- `main`: Production-ready code
- `feature/*`: New features (from main)
- `fix/*`: Bug fixes (from main)
- `hotfix/*`: Critical production fixes (from main)
- `release/*`: Release preparation (from develop)

---

## Testing Strategy

### Test Pyramid

```mermaid
graph TB
    subgraph Test_Pyramid
        A[EUnit<br/>Unit Tests<br/>~5s] --> B[Common Test<br/>Integration Tests<br/>~15s]
        B --> C[PropEr<br/>Property Tests<br/>~10s]
        C --> D[Manual<br/>E2E Tests<br/>Ad-hoc]
    end

    style A fill:#51cf66
    style B fill:#339af0
    style C fill:#fab005
    style D fill:#ff6b6b
```

### Test Execution Flow

```mermaid
graph TB
    A[Developer pushes code] --> B[Travis CI/GitHub Actions]
    B --> C[Unit Tests EUnit]
    C --> D{Pass?}
    D -->|No| E[Notify Developer]
    D -->|Yes| F[Integration Tests CT]
    F --> G{Pass?}
    G -->|No| E
    G -->|Yes| H[Property Tests PropEr]
    H --> I{Pass?}
    I -->|No| E
    I -->|Yes| J[Coverage Analysis]
    J --> K{≥80%?}
    K -->|No| E
    K -->|Yes| L[Benchmarks]
    L --> M{Regression OK?}
    M -->|No| E
    M -->|Yes| N[All Checks Pass]

    style E fill:#ff6b6b
    style N fill:#51cf66
```

### Test Categories

**Unit Tests (EUnit)**:
- Test individual functions
- Fast execution (<5s)
- No external dependencies
- 100% coverage for critical paths

**Integration Tests (Common Test)**:
- Test module interactions
- Test OTP behaviors
- Test transport protocols
- Real processes (no mocks)

**Property-Based Tests (PropEr)**:
- Generate random inputs
- Verify invariants
- Find edge cases
- Complex algorithms only

---

## Quality Gates

### Mandatory Quality Gates

```mermaid
graph TB
    subgraph Quality_Gates
        A[Gate 1: Compilation<br/>errors = 0]
        B[Gate 2: Tests<br/>pass_rate = 1.0]
        C[Gate 3: Coverage<br/>≥ 80%]
        D[Gate 4: Dialyzer<br/>warnings → 0]
        E[Gate 5: Xref<br/>undefined = ∅]
    end

    A --> B
    B --> C
    C --> D
    D --> E

    style A fill:#ff6b6b
    style B fill:#fab005
    style C fill:#339af0
    style D fill:#51cf66
    style E fill:#845ef7
```

### Gate Enforcement

```mermaid
graph LR
    A[git commit] --> B{Pre-commit Hook}
    B -->|Fast fail| C[Block Commit]
    B -->|Pass| D[git push]

    D --> E{CI Pipeline}
    E -->|Fail| F[Block Merge]
    E -->|Pass| G[Ready to Merge]

    C --> H[Fix Issues]
    F --> H
    H --> A

    style C fill:#ff6b6b
    style F fill:#ff6b6b
    style G fill:#51cf66
```

**Quality Gate Commands**:

```bash
# Gate 1: Compilation
TERM=dumb rebar3 compile           # errors = 0

# Gate 2: Tests
rebar3 eunit --module=M_tests      # pass_rate = 1.0
rebar3 ct --suite=test/S           # pass_rate = 1.0

# Gate 3: Coverage
rebar3 cover                       # ≥80%
rebar3 cover --verbose             # see detailed report

# Gate 4: Dialyzer
rebar3 dialyzer                    # warnings → 0

# Gate 5: Xref
rebar3 xref                        # undefined = ∅

# All gates
make check                         # runs all gates
```

---

## Tooling

### Development Tools

```mermaid
mindmap
    root((Development Tools))
        Build
            rebar3 compile
            make build
            make workspace-build
        Test
            rebar3 eunit
            rebar3 ct
            rebar3 proper
            make test
        Quality
            rebar3 dialyzer
            rebar3 xref
            make lint
            make format
        Debug
            make console
            make observer
            make debugger
        Release
            rebar3 release
            make workspace-release
        Docs
            edoc
            ex_doc
```

### Makefile Targets

```mermaid
graph TB
    subgraph Makefile_Targets
        A[build<br/>Compile code]
        B[test<br/>Run tests]
        C[lint<br/>Static analysis]
        D[check<br/>All quality gates]
        E[console<br/>Erlang shell]
        F[observer<br/>Debug GUI]
        G[release<br/>Build release]
        H[workspace-*<br/>Multi-project targets]
    end

    A --> B
    B --> C
    C --> D
    D --> E
    E --> F
    F --> G
    G --> H

    style A fill:#51cf66
    style B fill:#51cf66
    style C fill:#339af0
    style D fill:#fab005
    style E fill:#845ef7
    style F fill:#845ef7
    style G fill:#ff6b6b
    style H fill:#ff6b6b
```

### Essential Commands

**Build & Test**:
```bash
make build                    # Compile
make test                     # Run tests
make test-unit                # Unit tests only
make test-integration         # Integration tests only
make workspace-test           # All projects
```

**Quality**:
```bash
make lint                     # Dialyzer + Xref
make check                    # All gates
make coverage-report          # Coverage HTML
make format                   # Format code
```

**Debug**:
```bash
make console                  # Erlang shell
make observer                 # Observer GUI
make dev-console              # Dev config shell
```

**Release**:
```bash
make release                  # Production release
make workspace-release        # All releases
rebar3 release -n dev         # Dev release
```

---

## Development Patterns

### OTP Development

```mermaid
graph TB
    A[Identify Need] --> B{gen_server?}
    B -->|Yes| C[Define Callbacks]
    B -->|No| D{supervisor?}
    D -->|Yes| E[Define Child Specs]
    D -->|No| F[Plain Module]

    C --> G[init/1]
    C --> H[handle_call/3]
    C --> I[handle_cast/2]
    C --> J[handle_info/2]
    C --> K[terminate/2]
    C --> L[code_change/3]

    G --> M[Write Tests FIRST]
    H --> M
    I --> M
    J --> M
    K --> M
    L --> M

    M --> N[Implement Callbacks]
    N --> O[Run Quality Gates]
    O --> P{Pass?}
    P -->|No| N
    P -->|Yes| Q[Complete]

    style Q fill:#51cf66
```

### Transport Implementation

```mermaid
graph TB
    A[New Transport] --> B[Create Module]
    B --> C[Implement Behavior]
    C --> D[-behaviour erlmcp_transport]

    D --> E[init/2]
    D --> F[send/2]
    D --> G[close/1]

    E --> H[Write Tests]
    F --> H
    G --> H

    H --> I[Test Real Transport]
    I --> J{Pass?}
    J -->|No| K[Fix Issues]
    J -->|Yes| L[Register Transport]

    K --> I
    L --> M[Complete]

    style M fill:#51cf66
```

---

## Continuous Improvement

### Feedback Loops

```mermaid
graph LR
    A[Write Code] --> B[Automated Tests]
    B --> C[Code Review]
    C --> D[Integration Tests]
    D --> E[Staging Deploy]
    E --> F[Canary Test]
    F --> G[Production Deploy]
    G --> H[Monitor Metrics]
    H --> I{Issues?}
    I -->|Yes| J[Create Hotfix]
    I -->|No| K[Success]
    J --> A

    style K fill:#51cf66
    style J fill:#ff6b6b
```

### Kaizen (Continuous Improvement)

```mermaid
graph TB
    A[Identify Waste] --> B[Analyze Root Cause]
    B --> C[Propose Improvement]
    C --> D[Implement Change]
    D --> E[Measure Impact]
    E --> F{Better?}
    F -->|Yes| G[Standardize]
    F -->|No| H[Rollback]
    G --> A

    style G fill:#51cf66
    style H fill:#ff6b6b
```

---

## Related Documentation

- **Testing**: See [../testing-guide.md](../testing-guide.md)
- **OTP Patterns**: See [../otp-patterns.md](../otp-patterns.md)
- **Architecture**: See [../architecture.md](../architecture.md)
- **Quality Gates**: See [../../CLAUDE.md](../../CLAUDE.md#quality-gates)
- **Contributing**: See [../../CONTRIBUTING.md](../../CONTRIBUTING.md)

---

**Last Updated**: 2026-01-31
**Status**: Active
**Maintainers**: erlmcp development team
