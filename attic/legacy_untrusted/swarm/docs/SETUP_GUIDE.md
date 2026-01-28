# Docker Swarm Testing Infrastructure - Setup Guide

## Overview

The erlmcp Docker Swarm testing infrastructure provides a complete benchmarking and stress testing environment for MCP (Model Context Protocol) clients and servers. This infrastructure supports:

- **Horizontal Scaling**: 5-10 MCP server replicas with load balancing
- **Client Simulation**: 50-100+ simulated MCP clients with configurable profiles
- **Comprehensive Metrics**: Prometheus and Grafana for real-time monitoring
- **Stress Testing**: 10+ predefined test scenarios
- **Automated Benchmarking**: Complete test suite with report generation

## Architecture

```
┌─────────────────────────────────────────────────────┐
│           Docker Swarm Manager (Master)             │
├─────────────────────────────────────────────────────┤
│                                                     │
│  ┌──────────────────────────────────────────────┐  │
│  │         Load Balancer (Traefik)              │  │
│  │  Port 80: HTTP                              │  │
│  │  Port 5555: WebSocket                       │  │
│  │  Port 9090: Metrics                         │  │
│  └──────────────────────────────────────────────┘  │
│          ↓         ↓         ↓         ↓           │
│  ┌──────────────────────────────────────────────┐  │
│  │     MCP Server Cluster (8 replicas)         │  │
│  │  - Erlang/OTP processes                     │  │
│  │  - Per-instance metrics export              │  │
│  │  - Health checks                            │  │
│  └──────────────────────────────────────────────┘  │
│                                                     │
│  ┌──────────────────────────────────────────────┐  │
│  │     Monitoring Stack                        │  │
│  │  - Prometheus (metrics collection)          │  │
│  │  - Grafana (dashboards)                     │  │
│  │  - Alert Manager                            │  │
│  └──────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────┘
             ↓         ↓         ↓
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ Worker Node1 │ │ Worker Node2 │ │ Worker Node3 │
├──────────────┤ ├──────────────┤ ├──────────────┤
│ MCP Servers  │ │ MCP Servers  │ │ MCP Servers  │
│ Clients      │ │ Clients      │ │ Clients      │
│ Monitoring   │ │ Monitoring   │ │ Monitoring   │
└──────────────┘ └──────────────┘ └──────────────┘
```

## Prerequisites

### System Requirements

- **Docker Engine**: 20.10+ (Swarm mode required)
- **CPU**: 4+ cores minimum (8+ cores recommended)
- **RAM**: 16GB minimum (32GB+ recommended for heavy load)
- **Disk**: 50GB+ free space (for logs and metrics storage)
- **Network**: Low-latency network connection between nodes
- **OS**: Linux, macOS (with Docker Desktop), or Windows (with Docker Desktop)

### Tools

```bash
# Required
docker version                # 20.10+
docker-compose version        # v2.0+
bash                          # 4.0+

# Recommended
curl                          # For health checks
jq                            # For JSON parsing
python3                       # For analytics
