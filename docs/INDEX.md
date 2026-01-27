# erlmcp Documentation Index

Welcome to the erlmcp workspace documentation. This is your navigation hub for understanding, developing, deploying, and operating erlmcp and its integrated TAIEA autonomic system.

## Quick Links by Role

### For Developers
Start here if you're writing code, running tests, or debugging:
- **[Getting Started](GETTING_STARTED.md)** - 5-minute quickstart for new developers
- **[For Developers Guide](FOR_DEVELOPERS.md)** - Complete development setup and workflows
- **[Build System](BUILD_SYSTEM.md)** - Build targets, SLOs, and performance gates
- **[Architecture Overview](ARCHITECTURE_OVERVIEW.md)** - System design and components
- **[API Reference](api-reference.md)** - Complete HTTP and MCP API documentation

### For Operators
Start here if you're deploying, monitoring, or managing infrastructure:
- **[For Operators Guide](FOR_OPERATORS.md)** - Deployment, monitoring, and scaling
- **[Deployment Guide](DEPLOYMENT.md)** - How to deploy to GCP
- **[Troubleshooting](TROUBLESHOOTING.md)** - Common issues and solutions
- **[GCP Setup](GCP_SETUP.md)** - GCP project configuration and cloud infrastructure

### For Architects
Start here if you're designing systems, making technology decisions, or planning capacity:
- **[For Architects Guide](FOR_ARCHITECTS.md)** - System design, performance, security
- **[Architecture Overview](ARCHITECTURE_OVERVIEW.md)** - Detailed component design
- **[OTP Patterns](otp-patterns.md)** - Erlang/OTP best practices and patterns
- **[Protocol Guide](protocol.md)** - MCP protocol implementation details

## Documentation by Topic

### Getting Started
- [GETTING_STARTED.md](GETTING_STARTED.md) - 5-minute quickstart (all roles)
- [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md) - Development setup and workflows
- [FOR_OPERATORS.md](FOR_OPERATORS.md) - Operational setup and management
- [FOR_ARCHITECTS.md](FOR_ARCHITECTS.md) - Architecture and design decisions

### Core Systems
- [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md) - System design, erlmcp + TAIEA
- [protocol.md](protocol.md) - MCP protocol specification and implementation
- [otp-patterns.md](otp-patterns.md) - Erlang/OTP patterns and best practices
- [api-reference.md](api-reference.md) - HTTP endpoints, MCP tools, Erlang APIs

### Development & Testing
- [BUILD_SYSTEM.md](BUILD_SYSTEM.md) - Build targets, performance gates, SLOs
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Code style, PR process, testing requirements

### Operations & Deployment
- [DEPLOYMENT.md](DEPLOYMENT.md) - How to deploy to GCP, release procedures
- [FOR_OPERATORS.md](FOR_OPERATORS.md) - Operational runbooks and procedures
- [GCP_SETUP.md](GCP_SETUP.md) - GCP infrastructure setup and configuration
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Common problems and solutions

### Advanced Topics
- [POOLBOY_INTEGRATION.md](POOLBOY_INTEGRATION.md) - Worker pool management
- [otp-architecture-redesign.md](otp-architecture-redesign.md) - Architecture design patterns
- [v0.6.0-IMPLEMENTATION-SUMMARY.md](v0.6.0-IMPLEMENTATION-SUMMARY.md) - Library integration summary
- [phase3_implementation_plan.md](phase3_implementation_plan.md) - v0.6.0 implementation details

## Key Features Overview

### erlmcp Core
- **Full MCP Protocol Support** - Resources, tools, prompts, subscriptions
- **OTP Compliance** - Supervision trees, fault tolerance, distributed
- **Multiple Transports** - stdio, TCP, HTTP/2
- **JSON Schema Validation** - Automatic tool argument validation
- **Production Ready** - Comprehensive error handling, monitoring, logging

### TAIEA Integration (v0.6.0)
- **gproc Registry** - Distributed process monitoring and cleanup
- **gun HTTP/2** - Modern HTTP/1.1 and HTTP/2 support
- **ranch TCP** - Production-grade connection pooling
- **poolboy** - Efficient worker pool management
- **~770 LOC Reduction** - Less custom code, more reliability

## Workspace Structure

```
erlmcp/
├── README.md                    # Project overview (main entry point)
├── DEVELOPMENT.md               # Development workflows
├── CONTRIBUTING.md              # Contributing guidelines
├── docs/
│   ├── INDEX.md                # This file - documentation navigation hub
│   ├── GETTING_STARTED.md       # 5-minute quickstart
│   ├── ARCHITECTURE_OVERVIEW.md # System design
│   ├── BUILD_SYSTEM.md          # Build targets and SLOs
│   ├── DEPLOYMENT.md            # Deployment procedures
│   ├── FOR_DEVELOPERS.md        # Developer setup and workflows
│   ├── FOR_OPERATORS.md         # Operational procedures
│   ├── FOR_ARCHITECTS.md        # Architecture decisions
│   ├── TROUBLESHOOTING.md       # Common issues and solutions
│   ├── api-reference.md         # API documentation
│   ├── protocol.md              # MCP protocol details
│   ├── otp-patterns.md          # Erlang/OTP patterns
│   └── [legacy docs]/           # Historical documentation
├── src/                         # erlmcp source code
├── test/                        # Test suite
├── examples/                    # Example applications
├── config/                      # Configuration files
├── taiea/                       # TAIEA autonomic system
│   ├── docs/                    # TAIEA-specific documentation
│   ├── apps/
│   │   ├── taiea_core/
│   │   ├── taiea_mcp/
│   │   ├── taiea_governor/
│   │   └── taiea_receipts/
│   └── rel/                     # Release configuration
└── gcp/                         # GCP deployment files
```

## Common Tasks

### I want to...

**Get started quickly**
→ [GETTING_STARTED.md](GETTING_STARTED.md) (5 minutes)

**Set up my development environment**
→ [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md) → [BUILD_SYSTEM.md](BUILD_SYSTEM.md)

**Deploy to production**
→ [DEPLOYMENT.md](DEPLOYMENT.md) → [FOR_OPERATORS.md](FOR_OPERATORS.md)

**Understand the architecture**
→ [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md) → [FOR_ARCHITECTS.md](FOR_ARCHITECTS.md)

**Troubleshoot an issue**
→ [TROUBLESHOOTING.md](TROUBLESHOOTING.md)

**Write an MCP client or server**
→ [api-reference.md](api-reference.md) → [examples/README.md](../examples/README.md)

**Understand the MCP protocol**
→ [protocol.md](protocol.md)

**Learn about Erlang/OTP patterns**
→ [otp-patterns.md](otp-patterns.md)

**Contribute code**
→ [CONTRIBUTING.md](../CONTRIBUTING.md) → [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md)

## Document Ownership

| Document | Owner | Last Updated |
|----------|-------|--------------|
| INDEX.md | DevOps | 2026-01-26 |
| GETTING_STARTED.md | DevOps | 2026-01-26 |
| ARCHITECTURE_OVERVIEW.md | Architecture | 2026-01-26 |
| BUILD_SYSTEM.md | DevOps | 2026-01-26 |
| DEPLOYMENT.md | DevOps | 2026-01-26 |
| TROUBLESHOOTING.md | DevOps | 2026-01-26 |
| FOR_DEVELOPERS.md | Engineering | 2026-01-26 |
| FOR_OPERATORS.md | Operations | 2026-01-26 |
| FOR_ARCHITECTS.md | Architecture | 2026-01-26 |
| api-reference.md | Engineering | 2026-01-26 |
| protocol.md | Engineering | 2026-01-26 |
| otp-patterns.md | Architecture | 2026-01-26 |

## Documentation Standards

- All documents use Markdown with proper link references
- Code examples are executable and tested
- Prerequisites are clearly stated upfront
- Estimated reading/task time is provided
- Links to related documents are included
- Role-specific guides tailor information to audience

## Feedback & Updates

Found an error? Documentation unclear? Want to contribute?

1. Open an issue on GitHub describing the problem
2. Submit a pull request with suggested improvements
3. Tag maintainers for review

See [CONTRIBUTING.md](../CONTRIBUTING.md) for detailed guidelines.

---

**Last Updated**: 2026-01-26
**Status**: Complete documentation hub
**Erlang/OTP**: 25+
**rebar3**: 3.22+
