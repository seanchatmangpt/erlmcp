# erlmcp Documentation Hub

> **Production-grade Erlang/OTP SDK for the Model Context Protocol (MCP)**
>
> Complete documentation following the Diátaxis framework - tutorials, how-to guides, technical reference, and conceptual explanations.

---

## 🚀 Quick Start Paths

### 5-Minute Docker Quick Start
**DOCKER-ONLY: All operations execute via Docker (no local Erlang/OTP required)**

```bash
# Clone and start runtime
git clone https://github.com/banyan-platform/erlmcp && cd erlmcp
docker compose --profile runtime up -d

# Verify health
curl http://localhost:8080/health

# Run quality gates via Docker
docker compose run --rm erlmcp-build rebar3 compile
docker compose run --rm erlmcp-unit rebar3 eunit
docker compose run --rm erlmcp-ct rebar3 ct
```

**See**: [Main README](../README.md) for complete Docker workflows

### Choose Your Path

| I want to... | Start here | Time |
|--------------|------------|------|
| **Understand what erlmcp is** | [Introduction](explain/introduction.md) | 15 min |
| **Build my first integration** | [First Integration](howto/first-integration.md) | 30 min |
| **Deploy to production** | [Deployment Guide](deployment/) + [Docker workflows](../README.md#production-deployment) | 45 min |
| **Look up API details** | [API Reference](reference/) | Reference |
| **Optimize performance** | [Performance Guide](howto/performance-optimization.md) | 45 min |
| **See working examples** | [Examples](appendices/) + [/examples directory](../examples/) | Browse |

---

## 📚 Documentation Structure (Diátaxis Framework)

### 🎓 [Explain](explain/) - Understanding (Learning-oriented)
**Why does this exist? What are the concepts?**

- [Introduction to erlmcp](explain/introduction.md) - What it is and why it exists
- [Architecture Overview](explain/architecture.md) - System design and supervision trees
- [Design Principles](explain/design-principles.md) - Core values and decisions
- [Protocol Understanding](explain/protocol.md) - MCP protocol fundamentals

**Best for**: New users, architects, technical leaders evaluating erlmcp

---

### 🛠️ [How-to Guides](howto/) - Problem-solving (Goal-oriented)
**How do I accomplish specific tasks?**

#### Getting Started
- [Installation Guide](howto/installation.md) - Set up via Docker (REQUIRED approach)
- [First Integration](howto/first-integration.md) - Build your first MCP connection
- [Local Development](howto/local-dev.md) - Docker-based development workflow

#### Integration & Deployment
- [Docker Deployment](howto/deploy.md) - Single-node and multi-node Docker deployments
- [GCP Integration](howto/gcp-integration.md) - Connect with Google Cloud Platform
- [Performance Optimization](howto/performance-optimization.md) - Tuning and optimization

#### Operations
- [Benchmarking & Chaos](howto/bench-and-chaos.md) - Performance testing and fault injection
- [Upgrade Guide](howto/upgrade.md) - Version migration paths

**Best for**: Developers, DevOps engineers, system integrators

---

### 📖 [Reference](reference/) - Information (Information-oriented)
**What are the technical specifications?**

#### API Documentation
- [API Reference](reference/api-reference/) - Complete API specifications
- [Configuration Reference](reference/configuration/) - All configuration options
- [Config Profiles](reference/config-profiles.md) - Development vs production profiles

#### Technical Specifications
- [Receipts System](reference/receipts.md) - Proof and verification receipts
- [Receipts CLI Examples](reference/receipts_cli_examples.md) - CLI usage patterns
- [Refusals](reference/refusals.md) - Error handling and rejection semantics

**Best for**: Experienced developers, API consumers, system administrators

---

### 📎 [Appendices](appendices/) - Supporting Materials
**Additional resources, examples, and references**

- Code Examples - Working implementations
- Glossary - Terms and definitions
- Community Resources - Get help and contribute
- Contributing Guidelines - How to contribute

**Best for**: Everyone - supplementary materials and context

---

## 🐳 Docker-Based Workflows (REQUIRED)

### Quality Lanes (Docker Services)

All compilation, testing, and validation MUST execute via Docker quality lanes:

| Lane | Service | Purpose | Command |
|------|---------|---------|---------|
| **Compile** | `erlmcp-build` | Build gate | `docker compose run --rm erlmcp-build rebar3 compile` |
| **Unit Tests** | `erlmcp-unit` | Unit test gate | `docker compose run --rm erlmcp-unit rebar3 eunit` |
| **Integration** | `erlmcp-ct` | Common Test gate | `docker compose run --rm erlmcp-ct rebar3 ct` |
| **Quality** | `erlmcp-check` | Dialyzer + Xref | `docker compose run --rm erlmcp-check rebar3 dialyzer` |
| **Bench** | `erlmcp-bench` | Performance gate | `docker compose run --rm erlmcp-bench make benchmark` |

**Why Docker-only?** Ensures deterministic builds, reproducible results, and production parity. See [CLAUDE.md](../CLAUDE.md) for complete Docker constitution.

### Deployment Options

| Method | Use Case | Documentation |
|--------|----------|---------------|
| **Docker Compose** | Single-node, POC, testing | [Main README](../README.md#production-deployment) |
| **Docker Swarm** | Multi-node HA (RECOMMENDED) | [Swarm Deployment](deployment/SWARM_DEPLOYMENT.md) |
| **Kubernetes** | Cloud-native, auto-scaling | [K8s Deployment](../k8s/) |

---

## 📋 Essential Documentation

### Top Documents (Start Here)

| Document | Purpose | Audience |
|----------|---------|----------|
| [00_START_HERE_MASTER_SYNTHESIS.md](00_START_HERE_MASTER_SYNTHESIS.md) | Production readiness report | Decision makers |
| [CLI Reference](CLI_REFERENCE.md) | Complete CLI documentation | Developers |
| [CLI Interactive Guide](CLI_INTERACTIVE_GUIDE.md) | REPL workflows | Developers |
| [Deployment Checklist](DEPLOYMENT_CHECKLIST.md) | Production deployment verification | Operators |
| [API Quick Reference](API_QUICK_REFERENCE.md) | Fast API lookup | Developers |

### Architecture & Design

| Document | Purpose |
|----------|---------|
| [Architecture](explain/architecture.md) | System design |
| [Design Principles](explain/design-principles.md) | Design philosophy |
| [Protocol](explain/protocol.md) | MCP protocol details |

### Operations & Deployment

| Document | Purpose |
|----------|---------|
| [Deployment Guide](DEPLOYMENT.md) | Complete deployment procedures |
| [Deployment Runbook](DEPLOYMENT_RUNBOOK.md) | Step-by-step operations |
| [Disaster Recovery](DISASTER_RECOVERY_QUICK_REFERENCE.md) | DR procedures |
| [Observability](observability/) | Monitoring and metrics |
| [Runbooks](runbooks/) | Operational procedures |

### Development & Testing

| Document | Purpose |
|----------|---------|
| [Development Guide](development/) | Local development setup |
| [Testing Guide](testing/) | Test strategies |
| [Quality Gates](quality-gates/) | Quality enforcement |
| [Benchmarks](benchmarks/) | Performance testing |

---

## 🔍 Navigation by Role

### Developers
1. [Introduction](explain/introduction.md) - Understand erlmcp
2. [First Integration](howto/first-integration.md) - Build your first integration
3. [CLI Reference](CLI_REFERENCE.md) - Command-line tools
4. [API Reference](reference/) - API specifications
5. [Examples](../examples/) - Working code samples

### Operators / DevOps
1. [Docker Quick Start](../README.md#5-minute-docker-quick-start) - Get running fast
2. [Deployment Guide](DEPLOYMENT.md) - Production deployment
3. [Deployment Checklist](DEPLOYMENT_CHECKLIST.md) - Verification steps
4. [Runbooks](runbooks/) - Operational procedures
5. [Disaster Recovery](DISASTER_RECOVERY_QUICK_REFERENCE.md) - DR plans

### Architects
1. [Architecture](explain/architecture.md) - System design
2. [Design Principles](explain/design-principles.md) - Design philosophy
3. [Performance](howto/performance-optimization.md) - Scaling considerations
4. [Security](security/) - Security architecture
5. [HA Procedures](ha-procedures/) - High availability

### Contributors
1. [Contributing Guide](../CONTRIBUTING.md) - How to contribute
2. [Development Guide](development/) - Local setup
3. [Quality Gates](quality-gates/) - Quality requirements
4. [Testing Guide](testing/) - Test expectations

---

## 🎯 Documentation by Category

### 📦 Deployment & Infrastructure
- [deployment/](deployment/) - Deployment guides and automation
- [dr/](dr/) - Disaster recovery procedures
- [ha-procedures/](ha-procedures/) - High availability procedures
- [ci-cd/](ci-cd/) - CI/CD pipeline documentation

### 🔒 Security
- [security/](security/) - Security architecture and procedures
- [secrets/](secrets/) - Secrets management

### 📊 Observability
- [observability/](observability/) - Monitoring, metrics, tracing
- [metrics/](metrics/) - Metrics collection and analysis
- [metrology/](metrology/) - Measurement systems

### ⚡ Performance
- [performance/](performance/) - Performance tuning
- [benchmarks/](benchmarks/) - Benchmark results
- [bench/](bench/) - Benchmarking tools

### 🏗️ Architecture
- [architecture/](architecture/) - Architecture documentation
- [c4/](c4/) - C4 architecture diagrams
- [diagrams/](diagrams/) - System diagrams

### 🔧 Development
- [development/](development/) - Development guides
- [cli/](cli/) - CLI documentation
- [api/](api/) - API documentation
- [testing/](testing/) - Testing strategies
- [quality-enforcement/](quality-enforcement/) - Quality standards

### 🌐 Integration
- [integration/](integration/) - Integration guides
- [network/](network/) - Network configuration
- [protocol/](protocol/) - Protocol specifications

### 📚 Reference
- [reference/](reference/) - Technical reference
- [v3/](v3/) - Version 3 documentation
- [migration/](migration/) - Migration guides
- [upgrades/](upgrades/) - Upgrade procedures

---

## 📊 Version Information

- **Current Version**: erlmcp v3.0.0
- **OTP Requirement**: Erlang/OTP 28.3.1+ (REQUIRED)
- **MCP Spec**: 2025-11-25 (95.7% compliance)
- **Last Updated**: February 2026

### Breaking Changes in v3.0
- OTP 28.3.1+ required (exclusive use of native features)
- Native JSON module (2-3x faster, no jsx dependency)
- Priority messages (EEP 76) for sub-ms latency
- See [Main README](../README.md#otp-2831-migration) for migration guide

---

## 🆘 Getting Help

### Quick References
- [CLI Quick Reference](API_QUICK_REFERENCE.md) - Fast command lookup
- [Troubleshooting](reference/) - Common issues and solutions
- [FAQ](appendices/) - Frequently asked questions

### Community
- **Issues**: [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues)
- **Discussions**: [GitHub Discussions](https://github.com/banyan-platform/erlmcp/discussions)
- **Documentation**: This directory - navigate by role or category above

---

## 📝 Documentation Standards

All documentation adheres to:
- **Diátaxis Framework**: Tutorials, how-to, reference, explanation
- **Docker-First**: All examples use Docker workflows
- **Actionable**: Clear commands and expected outputs
- **Maintained**: Regular updates with version changes
- **Accessible**: Clear navigation and audience targeting

---

## 🔄 Next Steps

**New to erlmcp?** → Start with [Introduction](explain/introduction.md), then [First Integration](howto/first-integration.md)

**Ready to deploy?** → Follow [Docker Quick Start](../README.md#5-minute-docker-quick-start), then [Deployment Checklist](DEPLOYMENT_CHECKLIST.md)

**Need API details?** → Browse [API Reference](reference/) or [API Quick Reference](API_QUICK_REFERENCE.md)

**Want to contribute?** → Read [Contributing Guide](../CONTRIBUTING.md) and [Development Guide](development/)

---

**License**: Apache 2.0 | **Documentation**: Actively maintained | **Feedback**: [Open an issue](https://github.com/banyan-platform/erlmcp/issues)