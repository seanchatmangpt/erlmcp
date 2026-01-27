# Getting Started with erlmcp

**Estimated time: 5 minutes**

Welcome to erlmcp! This quick guide gets you up and running in 5 minutes, whether you're a developer, operator, or architect.

## Prerequisites

- **Erlang/OTP 25** or later
- **rebar3** 3.22 or later
- **git**

## The 5-Minute Setup

### 1. Clone and Enter the Workspace (30 seconds)

```bash
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp
```

### 2. Load Environment Variables (10 seconds)

```bash
direnv allow              # Enable automatic environment loading
# (or set ERL_LIBS manually if not using direnv)
```

### 3. Build Everything (60 seconds)

```bash
make workspace-build
```

This compiles:
- erlmcp (Model Context Protocol implementation)
- taiea (Autonomic governance system)
- All dependencies

### 4. Run Tests (90 seconds)

```bash
make workspace-test
```

All tests pass? You're ready! ✅

### 5. Verify Installation (30 seconds)

```bash
make help
```

See all available commands.

## What You Just Built

You now have a complete Erlang/OTP workspace with:

- **erlmcp**: Full MCP protocol implementation with client/server SDKs
- **taiea**: Autonomic system for governance and receipts
- **Testing**: EUnit, Common Test, PropEr property-based tests
- **Build System**: Makefile orchestration, performance gates, SLOs
- **Examples**: Weather server, calculator client, full applications

## Next Steps

### For Developers
1. Read [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md)
2. Explore [examples/](../examples/README.md)
3. Learn about [BUILD_SYSTEM.md](BUILD_SYSTEM.md)
4. Dive into [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)

### For Operators
1. Read [FOR_OPERATORS.md](FOR_OPERATORS.md)
2. Review [DEPLOYMENT.md](DEPLOYMENT.md)
3. Check [GCP_SETUP.md](GCP_SETUP.md)
4. Study [TROUBLESHOOTING.md](TROUBLESHOOTING.md)

### For Architects
1. Read [FOR_ARCHITECTS.md](FOR_ARCHITECTS.md)
2. Study [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)
3. Learn [otp-patterns.md](otp-patterns.md)
4. Review [protocol.md](protocol.md)

## Common Commands

```bash
# Daily development
make build                    # Compile (5s)
make test                     # Run tests (30s)
make lint                     # Static analysis (30s)
make check                    # Full validation (65s)

# Create an MCP server interactively
make console                  # Start Erlang shell

# Debugging
make observer                 # Launch Erlang Observer GUI

# Deployment
make workspace-release        # Build production releases
```

## Your First MCP Server

Create a file `my_server.erl`:

```erlang
-module(my_server).
-export([start/0]).

start() ->
    {ok, Server} = erlmcp_server:start_link({stdio, []}, #{
        server_info => #{
            name => <<"My MCP Server">>,
            version => <<"1.0.0">>
        }
    }),

    % Add a simple resource
    erlmcp_server:add_resource(Server, <<"greeting://world">>,
        fun(_Uri) -> <<"Hello from erlmcp!">> end),

    % Keep the server running
    receive _ -> ok end.
```

Compile and run:
```bash
erlc my_server.erl
erl -noshell -pa . -eval 'my_server:start()' -s init stop
```

## Troubleshooting Setup Issues

### Build fails with "rebar3 not found"
```bash
brew install rebar3            # macOS
sudo apt-get install rebar3    # Ubuntu/Debian
```

### Tests fail with "Timeout"
```bash
make clean
make workspace-build
make workspace-test
```

### Permission denied errors
```bash
chmod +x _build/prod/rel/erlmcp/bin/erlmcp
```

For more help, see [TROUBLESHOOTING.md](TROUBLESHOOTING.md).

## Key Resources

- **Main README**: [README.md](../README.md) - Project overview
- **MCP Protocol**: [ModelContextProtocol.io](https://modelcontextprotocol.io/)
- **Erlang Docs**: [erlang.org](https://www.erlang.org/doc/)
- **rebar3 Guide**: [rebar3.org](https://rebar3.org/)

## What's Next?

You have three paths:

1. **Learn by Doing** → Explore [examples/](../examples/README.md), build something
2. **Understand the System** → Read [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)
3. **Deploy to Production** → Follow [DEPLOYMENT.md](DEPLOYMENT.md)

Welcome aboard! 🚀

---

**Status**: ✅ Ready to develop
**Build time**: ~2 minutes
**Next document**: [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md) or [FOR_OPERATORS.md](FOR_OPERATORS.md)
