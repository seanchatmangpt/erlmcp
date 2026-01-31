# Quick Start Guide - erlmcp POC Demo

## 1. Prerequisites

Ensure you have:
- Erlang/OTP 25-28 installed
- rebar3 build tool
- erlmcp project compiled

## 2. Quick Start (3 steps)

### Step 1: Compile the project

```bash
cd /home/user/erlmcp
TERM=dumb rebar3 compile
```

### Step 2: Start Erlang shell

```bash
make console
# or
rebar3 shell
```

### Step 3: Run the demo

```erlang
%% In the Erlang shell:
erlmcp_poc_demo:run_full_demo().
```

That's it! The demo will run for 60 seconds and display live metrics.

## Expected Output

```
╔════════════════════════════════════════════════════════════════╗
║          erlmcp POC Master Integration Demo                   ║
║  Showcasing: Circuit Breaker, Pub/Sub, Streaming, Metrics    ║
╚════════════════════════════════════════════════════════════════╝

🚀 Starting core components...
  ✓ Circuit breaker started
  ✓ Metrics collector started
  ✓ MCP server started with tools, resources, and prompts

👥 Starting 10 AI agent clients...
  ✓ Agent #1 connected
  ✓ Agent #2 connected
  ...

▶ Demo running (60 seconds)...

📊 Metrics at T+10s:
  Tool Calls:           47
  Subscriptions:        10
  ...

[Final summary after 60 seconds]
```

## Custom Run

```erlang
%% Run for 2 minutes with 5-second metrics
erlmcp_poc_demo:run_full_demo(#{
    duration_sec => 120,
    metrics_interval_ms => 5000
}).
```

## Troubleshooting

### "Module not found"
Ensure you compiled the project:
```bash
TERM=dumb rebar3 compile
```

### "Circuit breaker not started"
The demo will start it automatically. If issues persist:
```erlang
erlmcp_circuit_breaker:start_link().
```

### "Metrics not available"
Start observability app:
```erlang
application:ensure_all_started(erlmcp_observability).
```

## What Next?

Read the full documentation in `README_POC_DEMO.md` to understand:
- Architecture details
- Metrics explanation
- Extending the demo
- Performance expectations

Enjoy the demo! 🚀
